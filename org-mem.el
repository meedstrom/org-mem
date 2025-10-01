;;; org-mem.el --- Fast info from a large number of Org file contents -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author:   Martin Edström <meedstrom@runbox.eu>
;; URL:      https://github.com/meedstrom/org-mem
;; Created:  2025-03-15
;; Keywords: text
;; Package-Version: 0.21.0
;; Package-Requires: ((emacs "29.1") (el-job "2.4.2") (llama "0.5.0"))

;;; Commentary:

;; A cache of metadata about the structure of all your Org files - headings,
;; links and so on.

;; Builds quickly, so that there is no need to persist data
;; across sessions.

;; Provides two independent APIs:

;;  - Emacs Lisp: regularly named accessors such as `org-mem-entry-olpath',
;;                `org-mem-link-pos', `org-mem-all-id-nodes' etc

;;  - SQL: an in-memory SQLite database that can be queried like
;;         (emacsql (org-mem-roamy-db) [:select * :from links :limit 10])

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'llama)
(require 'el-job)
(require 'org-mem-parser)

(defvar org-id-locations)
(defvar org-id-track-globally)
(defvar org-id-locations-file)
(defvar org-id-extra-files)
(defvar org-id-files)
(defvar org-element-cache-persistent)
(defvar org-inhibit-startup)
(defvar org-agenda-files)
(declare-function org-id-locations-load "org-id")
(declare-function org-id-locations-save "org-id")
(declare-function org-id-alist-to-hash "org-id")
(declare-function org-id-hash-to-alist "org-id")
(define-obsolete-variable-alias 'org-mem-watch-dirs-exclude 'org-mem-exclude "0.13.0 (May 2025)")

(defgroup org-mem nil "Fast info from a large amount of Org file contents."
  :group 'org)

(defcustom org-mem-do-cache-text nil
  "Whether to also cache text contents of all entries.

This makes the raw text available via accessor `org-mem-entry-text'.

May slow Emacs init if set to t before enabling `org-mem-updater-mode',
so you might consider setting it afterwards, instead.
If so, the text would only be available from the first time that
`org-mem-updater--timer' fires."
  :type 'boolean
  :package-version '(org-mem . "0.9.0"))

(defcustom org-mem-do-warn-title-collisions t
  "Whether to print a message when two ID-nodes have the same title."
  :type 'boolean
  :package-version '(org-mem . "0.2.0"))

(defcustom org-mem-do-sync-with-org-id nil
  "Whether to exchange data with `org-id-locations'.

Benefits:
- Org-mem gets to know about files from anywhere, so long as they
  contain some ID and can thus be found in `org-id-locations'.
- Help ensure that ID-links to somewhere inside `org-mem-watch-dirs'
  always work, so they never trigger a fallback attempt to run
  `org-id-update-id-locations' when clicked, which can take a while.

No effect until after Org has loaded."
  :type 'boolean
  :package-version '(org-mem . "0.6.0"))

(defcustom org-mem-watch-dirs nil
  "List of directories in which to look for Org files.
Each directory is checked recursively \(looking in subdirectories,
sub-subdirectories etc\).

Exceptions:

- Subdirectories starting with underscore or dot, such as \".emacs.d\".
  To check such a directory, add its full path explicitly,
  or else rely on `org-mem-do-sync-with-org-id' as additional source.
- Subdirectories that are symlinks.
- Anything matching `org-mem-exclude'.
- Any file not ending in one of `org-mem-suffixes'.

Can be left at nil, if `org-mem-do-sync-with-org-id' is t.
Benefits of configuring this anyway:

- Awareness of files that contain no ID at all.
- React when new files appear in these directories.
  - Useful if this Emacs session is not the only program
    that may create, move or rename files.

Tip: If past misconfiguration has recorded duplicate IDs,
try command \\[org-mem-forget-id-locations-recursively]."
  :type '(repeat directory)
  :package-version '(org-mem . "0.5.0"))

;; REVIEW: Use backslashes on Windows?
(defcustom org-mem-exclude
  '("/logseq/bak/"
    "/logseq/version-files/"
    "/node_modules/"
    ".sync-conflict-" ;; Match e.g. "~/org/foo.sync-conflict-234234.org"
    "/backup" ;; Match e.g. "~/backups-2024/"
    ".#")
  "Literal substrings of file paths that should not be scanned.
Aside from this variable, some filters are hard-coded:

- We only scan files that end in one of `org-mem-suffixes'
  - Thus backups ending in ~, # or similar are excluded in any case,
    under the default setting for that variable
- We exclude symlinks

Main reason to configure this is to prevent counting various kinds of
\"back-up\" and \"auto-save\" files as duplicate ID locations,
especially such files appearing somewhere inside `org-mem-watch-dirs'.

You can also speed up `org-mem-reset' a bit by excluding directories
found inside `org-mem-watch-dirs' with a very large amount of files
\(on the order of 100,000), such as the infamous \"node_modules\"."
  :type '(repeat string)
  :package-version '(org-mem . "0.15.1"))

(defcustom org-mem-seek-link-types
  '("http" "https" "id" "file")
  "Which types of plain \(un-bracketed\) links to look for.

Org-mem will pick up most links that are wrapped in double brackets,
regardless of type.  To see what unusual types it may have found, try
this expression:

\(inspector-inspect
 \(seq-remove (##member (org-mem-link-type %) org-mem-seek-link-types)
             \(org-mem-all-links)))"
  :type '(repeat string)
  :package-version '(org-mem . "0.7.0"))

(defcustom org-mem-suffixes '(".org" ".org_archive")
  "File name suffixes to consider valid for scanning the file."
  :type '(repeat string)
  :package-version '(org-mem . "0.21.0"))

(defcustom org-mem-load-features nil
  "List of features to load in child process.
These features must have been loaded at some point in the current
process, because `load-history' is used to find the corresponding file."
  :type '(repeat symbol)
  :package-version '(org-mem . "0.21.0"))

(defcustom org-mem-inject-vars '((file-name-handler-alist . nil))
  "Alist of variable-value pairs to set in child process."
  :type '(alist :key-type symbol :value-type sexp)
  :package-version '(org-mem . "0.21.0"))

(defvar org-mem-scratch nil
  "Work buffer held current while executing some hooks.
These are hooks called many times:
- `org-mem-record-file-functions'
- `org-mem-record-entry-functions'
- `org-mem-record-link-functions'
- `org-mem-forget-file-functions'
- `org-mem-forget-entry-functions'
- `org-mem-forget-link-functions'

This lets a function on these hooks sidestep the performance overhead of
`with-temp-buffer' or `with-work-buffer', in favor of using the
already current buffer:
    \(cl-assert (eq (current-buffer) org-mem-scratch))
    \(erase-buffer)

Buffer is in `fundamental-mode'.  For an Org buffer see function
`org-mem-org-mode-scratch'.")


;;; Basics

(defvar org-mem--title<>id (make-hash-table :test 'equal)
  "1:1 table mapping a heading, file-title or alias, to an ID.")

(defvar org-mem--id<>entry (make-hash-table :test 'equal)
  "1:1 table mapping an ID to an `org-mem-entry' record.")

(defvar org-mem--truename<>content (make-hash-table :test 'equal)
  "1:1 table mapping a file name to the text content of that file.")

(defvar org-mem--truename<>entries (make-hash-table :test 'equal)
  "1:N table mapping a file name to a sorted list of `org-mem-entry' records.

Sorted by the order those entries are found in that file\;
effectively `org-mem-entry-pos' in ascending order.")

(defvar org-mem--truename<>metadata (make-hash-table :test 'equal)
  "1:1 table mapping a file name to a list of facts about that file.

Users have no reason to inspect this table, prefer stable API
in `org-mem-file-mtime' and friends.")

(defvar org-mem--target<>links (make-hash-table :test 'equal)
  "1:N table mapping a link target to a list of `org-mem-link' records.
The list represents all links from anywhere that have that exact target.

A target is a citekey or the path component of an Org link \(see Group 2
in `org-link-plain-re').  In practice, it is often an org-id like
\"57707152-9c05-43f5-9f88-5d4f3a0d019a\", an URI path like
\"//www.gnu.org\", or a citekey like \"@ioannidis2005\".

NOTE! A future version may omit the sigil @ in citekeys.")

(defvar org-mem--internal-entry-id<>links (make-hash-table :test 'eq)
  "1:N table mapping internal entry ID to list of `org-mem-link' records.
The list represents all links found in that entry,
but not in its children.")

(defvar org-mem--key<>subtable (make-hash-table :test 'eq)
  "Big bag of memoized values, smelling faintly of cabbage.")

(define-inline org-mem--table (key subkey)
  "In a table identified by KEY, access value at SUBKEY.
Store all tables in `org-mem--key<>subtable'.
Note: All tables cleared often, meant for memoizations."
  (inline-quote
   (gethash ,subkey (or (gethash ,key org-mem--key<>subtable)
                        (puthash ,key (make-hash-table :test 'equal)
                                 org-mem--key<>subtable)))))

(defun org-mem--get-file-metadata (file/entry/link)
  "Return list of assorted data if FILE/ENTRY/LINK known, else error."
  (let ((wild-file (if (stringp file/entry/link)
                       (org-mem--truename-maybe file/entry/link)
                     (if (org-mem-entry-p file/entry/link)
                         (org-mem-entry-file-truename file/entry/link)
                       (if (org-mem-link-p file/entry/link)
                           (org-mem-link-file-truename file/entry/link)
                         (error "org-mem: FILE/ENTRY/LINK is nil"))))))
    (or (gethash wild-file org-mem--truename<>metadata)
        (error "org-mem: File seems not yet scanned: %s" wild-file))))

(defun org-mem--fast-abbrev (absolute-file-name)
  "Abbreviate ABSOLUTE-FILE-NAME, faster than `abbreviate-file-name'."
  (let ((case-fold-search nil))
    (setq absolute-file-name (directory-abbrev-apply absolute-file-name))
    (if (string-match (with-memoization (org-mem--table 0 'org-mem--fast-abbrev)
                        (with-temp-buffer ;; No buffer-env
                          (directory-abbrev-make-regexp
                           (expand-file-name "~"))))
                      absolute-file-name)
        (concat "~" (substring absolute-file-name (match-beginning 1)))
      absolute-file-name)))

(cl-defstruct org-mem-link
  (file-truename      ""  :read-only t :type string)
  (pos                -1  :read-only t :type integer)
  (type               nil :read-only t :type string-or-nil)
  (target             ""  :read-only t :type string)
  (description        nil :read-only t :type string-or-nil)
  (citation-p         nil :read-only t :type boolean)
  (nearby-id          nil :read-only t :type string-or-nil)
  (-internal-entry-id -1  :read-only t :type integer))

(cl-defstruct org-mem-entry
  ;; Ordered for easy visual overview of a printed list of objects.
  (file-truename         ""  :read-only t :type string)
  (lnum                  -1  :read-only t :type integer)
  (pos                   -1  :read-only t :type integer)
  (title-maybe           nil :read-only t :type string-or-nil)
  (level                 -1  :read-only t :type integer)
  (id                    nil :read-only t :type string-or-nil)
  ;; Ordered alphabetically.
  (active-timestamps-int nil :read-only t :type list)
  (clocks-int            nil :read-only t :type list)
  (closed-int            nil :read-only t :type integer-or-nil)
  (crumbs                nil :read-only t :type list)
  (deadline-int          nil :read-only t :type integer-or-nil)
  (priority              nil :read-only t :type string-or-nil)
  (properties-inherited  nil :read-only t :type list)
  (properties-local      nil :read-only t :type list)
  (scheduled-int         nil :read-only t :type integer-or-nil)
  (stats-cookies         nil :read-only t :type list)
  (tags-inherited        nil :read-only t :type list)
  (tags-local            nil :read-only t :type list)
  (todo-state            nil :read-only t :type string-or-nil)
  (-internal-id          -1  :read-only t :type integer))


;;; To find objects to operate on

(defun org-mem-all-ids ()
  "All org-ids known to org-mem.
If `org-mem-do-sync-with-org-id' is nil, the output may NOT overlap
perfectly with `org-id-locations'."
  (with-memoization (org-mem--table 0 'org-mem-all-ids)
    (hash-table-keys org-mem--id<>entry)))

(defun org-mem-all-files ()
  "All Org files."
  (with-memoization (org-mem--table 0 'org-mem-all-files)
    (mapcar #'org-mem--fast-abbrev (org-mem-all-file-truenames))))

(defun org-mem-all-file-truenames ()
  "Truename of all Org files.
When in doubt, you should prefer `org-mem-all-files', because
`directory-abbrev-alist' exists for a reason.

Org-mem uses truenames internally, but if some public function does
not work with alternative names, that should be considered a bug."
  (hash-table-keys org-mem--truename<>metadata))

(defun org-mem-all-entries ()
  "All entries."
  (with-memoization (org-mem--table 0 'org-mem-all-entries)
    (apply #'append (hash-table-values org-mem--truename<>entries))))

(defun org-mem-all-id-nodes ()
  "All ID-nodes.
An ID-node is an entry that has an ID property."
  (with-memoization (org-mem--table 0 'org-mem-all-id-nodes)
    (hash-table-values org-mem--id<>entry)))

(defun org-mem-all-links ()
  "All links and citations.
Citations are `org-mem-link' objects that satisfy
`org-mem-link-citation-p'."
  (with-memoization (org-mem--table 0 'org-mem-all-links)
    (apply #'append (hash-table-values org-mem--target<>links))))

(defun org-mem-all-id-links ()
  "All ID-links."
  (with-memoization (org-mem--table 0 'org-mem-all-id-links)
    (org-mem-links-of-type "id")))

(defun org-mem-all-entries-with-active-timestamps ()
  "All entries that contain one or more active Org timestamps."
  (with-memoization (org-mem--table 0 'org-mem-all-entries-with-active-timestamps)
    (seq-filter #'org-mem-entry-active-timestamps (org-mem-all-entries))))

(defun org-mem-all-files-with-active-timestamps ()
  "All files that contain one or more active Org timestamps."
  (with-memoization (org-mem--table 0 'org-mem-all-files-with-active-timestamps)
    (cl-loop for file in (org-mem-all-files)
             when (seq-find #'org-mem-entry-active-timestamps
                            (org-mem-entries-in-file file))
             collect file)))

(defun org-mem-all-entries-with-dangling-clock ()
  "All entries with an incomplete CLOCK: line."
  (with-memoization (org-mem--table 0 'org-mem-all-entries-with-dangling-clock)
    (cl-loop for entry in (org-mem-all-entries)
             when (seq-find (##length= % 1)
                            (org-mem-entry-clocks-int entry))
             collect entry)))

(defun org-mem-entry-by-id (id)
  "The entry with :ID: property equal to \(presumed unique) ID."
  (and id (gethash id org-mem--id<>entry)))

(defun org-mem-entry-at-lnum-in-file (lnum file)
  "The entry that is current at line-number LNUM in FILE."
  (with-memoization (org-mem--table 11 (list lnum file))
    (let ((entries (org-mem-entries-in-file file)))
      (if (and (cadr entries) (= lnum 1 (org-mem-entry-lnum (cadr entries))))
          (cadr entries)
        (cl-loop
         for (prev next) on entries
         if (or (not next) (< lnum (org-mem-entry-lnum next))) return prev)))))

(defun org-mem-entry-at-pos-in-file (pos file)
  "The entry that is current at char-position POS in FILE."
  (with-memoization (org-mem--table 12 (list pos file))
    (let ((entries (org-mem-entries-in-file file)))
      (if (and (cadr entries) (= pos 1 (org-mem-entry-pos (cadr entries))))
          (cadr entries)
        (cl-loop
         for (prev next) on entries
         if (or (not next) (< pos (org-mem-entry-pos next))) return prev)))))

(define-inline org-mem-entry-at-file-pos (file pos)
  "Like `org-mem-entry-at-pos-in-file' with flipped argument order."
  (inline-quote (org-mem-entry-at-pos-in-file ,pos ,file)))

(define-inline org-mem-entry-at-file-lnum (file pos)
  "Like `org-mem-entry-at-lnum-in-file' with flipped argument order."
  (inline-quote (org-mem-entry-at-lnum-in-file ,pos ,file)))

(defun org-mem-next-entry (entry)
  "The next entry after ENTRY in the same file, if any."
  (with-memoization (org-mem--table 20 entry)
    (let ((entries (gethash (org-mem-entry-file-truename entry)
                            org-mem--truename<>entries)))
      (while (and (car entries)
                  (not (= (org-mem-entry-pos (car entries))
                          (org-mem-entry-pos entry))))
        (pop entries))
      (pop entries)
      (car entries))))

(defun org-mem-previous-entry (entry)
  "The entry before ENTRY in the same file, if any."
  (with-memoization (org-mem--table 22 entry)
    (let ((entries (gethash (org-mem-entry-file-truename entry)
                            org-mem--truename<>entries)))
      (if (= (org-mem-entry-pos entry) 1)
          nil
        (while (and (cadr entries)
                    (not (= (org-mem-entry-pos (cadr entries))
                            (org-mem-entry-pos entry))))
          (pop entries))
        (car entries)))))

(defun org-mem-entries-in-file (file)
  "List of entries in same order as they appear in FILE, if FILE known.
The list always contains at least one entry, which
represents the content before the first heading."
  (cl-assert (stringp file))
  (gethash (org-mem--truename-maybe file) org-mem--truename<>entries))

(defalias 'org-mem-file-entries #'org-mem-entries-in-file)

(defun org-mem-entries-in-files (files)
  "Combined list of entries from all of FILES."
  (with-memoization (org-mem--table 13 files)
    (cl-loop for file in (delete-dups (mapcar #'org-mem--truename-maybe files))
             when (stringp file)
             append (gethash file org-mem--truename<>entries))))

(defun org-mem-file-by-id (id)
  "The file that contains an :ID: property matching ID."
  (let ((entry (and id (gethash id org-mem--id<>entry))))
    (and entry (org-mem-entry-file entry))))

(defun org-mem-entry-that-contains-link (link)
  "The entry where LINK was found."
  (org-mem-entry-at-pos-in-file (org-mem-link-file link)
                                (org-mem-link-pos link)))

(defun org-mem-id-nodes-in-files (files)
  "All ID-nodes in FILES."
  (with-memoization (org-mem--table 15 files)
    (setq files (delete-dups
                 (seq-keep #'org-mem--truename-maybe (ensure-list files))))
    (seq-filter (##member (org-mem-entry-file-truename %) files)
                (org-mem-all-id-nodes))))

(defun org-mem-links-with-type-and-path (type path)
  "Links with components TYPE and PATH, see `org-link-plain-re'."
  (with-memoization (org-mem--table 23 (list type path))
    (cl-loop for link in (gethash path org-mem--target<>links)
             when (equal type (org-mem-link-type link))
             collect link)))

(defun org-mem-id-links-to-entry (entry)
  "All ID-links that point to ENTRY."
  (let ((id (org-mem-entry-id entry)))
    (and id (org-mem-id-links-to-id id))))

(defun org-mem-links-to-target (target)
  "All link objects with link target equal to TARGET."
  (cl-assert (stringp target))
  (gethash target org-mem--target<>links))

(defun org-mem-id-links-to-id (id)
  "All ID-links targeting ID."
  (with-memoization (org-mem--table 14 id)
    (when-let* ((links (org-mem-links-to-target id)))
      ;; For a vast majority of people, this filter is safe to skip.  But it's
      ;; possible, for example, to have a heading named identical to some ID
      ;; that you also have, and have non-ID links targeting that.
      (seq-filter (##equal (org-mem-link-type %) "id") links))))

(defun org-mem-id-node-by-title (title)
  "The ID-node titled TITLE."
  (and title (gethash (org-mem-id-by-title title) org-mem--id<>entry)))

(defun org-mem-id-by-title (title)
  "The ID that currently corresponds to TITLE.
TITLE is either a heading, a file title, or an alias.

Assumes unique titles.  If two IDs exist with same title, it is
undefined which ID is returned.  User can prevent this from becoming a
problem with the help of option `org-mem-do-warn-title-collisions'."
  (and title (gethash title org-mem--title<>id)))

(defun org-mem-links-from-id (id)
  "Links from context where local or inherited ID property is ID."
  (with-memoization (org-mem--table 16 id)
    (seq-filter (##equal (org-mem-link-nearby-id %) id)
                (org-mem-all-links))))

(defun org-mem-id-links-from-id (id)
  "ID-links from context where local or inherited ID property is ID."
  (with-memoization (org-mem--table 17 id)
    (seq-filter (##equal (org-mem-link-nearby-id %) id)
                ;; Likely to be already memoized, reducing GC
                (org-mem-all-id-links))))

(defun org-mem-id-links-into-file (file)
  "ID-links from anywhere, leading into somewhere in FILE."
  (with-memoization (org-mem--table 21 file)
    (let ((ids (seq-keep #'org-mem-entry-id (org-mem-entries-in-file file))))
      (cl-loop for link in (org-mem-all-id-links)
               when (member (org-mem-link-target link) ids)
               collect link))))

(defun org-mem-links-of-type (type)
  "All links of type TYPE."
  (with-memoization (org-mem--table 18 type)
    (seq-filter (##equal (org-mem-link-type %) type)
                (org-mem-all-links))))

(defun org-mem-links-in-file (file)
  "All links found inside FILE."
  (with-memoization (org-mem--table 19 file)
    (seq-mapcat #'org-mem-links-in-entry (org-mem-entries-in-file file))))

(defun org-mem-links-in-entry (entry)
  "All links found inside ENTRY, ignoring descendant entries.
Do not trust the result if used during `org-mem-forget-entry-functions'
or similar hook.  Trustworthy on `org-mem-post-full-scan-functions'."
  (and entry (gethash (org-mem-entry--internal-id entry)
                      org-mem--internal-entry-id<>links)))


;;; Entry info

(defun org-mem-entry-file (entry)
  "Abbreviated truename of file where ENTRY is.
Better than `org-mem-entry-file-truename' when users may see the name.
When in doubt, prefer this, but it should not matter what form of file
name you input to the org-mem API."
  (org-mem--fast-abbrev (org-mem-entry-file-truename entry)))

(defun org-mem-entry-subtree-p (entry)
  "Non-nil if ENTRY is a subtree, nil if a \"file-level node\"."
  (not (= 0 (org-mem-entry-level entry))))

(defun org-mem-entry-text (entry)
  "Full unfontified text content of ENTRY.
Requires `org-mem-do-cache-text' t."
  (with-memoization (org-mem--table 28 entry)
    (let ((content (gethash (org-mem-entry-file-truename entry)
                            org-mem--truename<>content))
          (next (org-mem-next-entry entry)))
      (and content
           (substring content
                      (- (org-mem-entry-pos entry) 1)
                      (and next (- (org-mem-entry-pos next) 1)))))))

(defun org-mem-entry-olpath (entry)
  "Outline path to ENTRY."
  (with-memoization (org-mem--table 25 entry)
    (mapcar #'cl-fourth (cdr (reverse (cdr (org-mem-entry-crumbs entry)))))))

(defun org-mem-entry-olpath-with-self (entry)
  "Outline path, including ENTRY\\='s own heading."
  (with-memoization (org-mem--table 26 entry)
    (and (org-mem-entry-title-maybe entry)
         (if (org-mem-entry-subtree-p entry)
             (mapcar #'cl-fourth (cdr (reverse (org-mem-entry-crumbs entry))))
           (mapcar #'cl-fourth (reverse (org-mem-entry-crumbs entry)))))))

;; TODO: Deprecate the optional arg
(defun org-mem-entry-olpath-with-file-title (entry &optional filename-fallback)
  "Outline path to ENTRY, including file #+title.
With FILENAME-FALLBACK, use file basename if there is no #+title."
  (with-memoization (org-mem--table 27 (list entry filename-fallback))
    (let ((olp (mapcar #'cl-fourth (reverse (cdr (org-mem-entry-crumbs entry)))))
          file-name-handler-alist)
      ;; The car of `olp' is the potentially nil file title.
      (when (null (car olp))
        (pop olp)
        (when filename-fallback
          (push (file-name-nondirectory (org-mem-entry-file-truename entry))
                olp)))
      olp)))

;; Better than above: single-argument func.
(defun org-mem-entry-olpath-with-file-title-or-basename (entry)
  "Outline path to ENTRY, including file #+title.
Use file basename if there is no #+title."
  (with-memoization (org-mem--table 30 entry)
    (let ((olp (mapcar #'cl-fourth (reverse (cdr (org-mem-entry-crumbs entry)))))
          file-name-handler-alist)
      ;; The car of `olp' is the potentially nil file title.
      (when (null (car olp))
        (pop olp)
        (push (file-name-nondirectory (org-mem-entry-file-truename entry))
              olp))
      olp)))

;; TODO: Deprecate the optional arg
(defun org-mem-entry-olpath-with-self-with-file-title (entry &optional filename-fallback)
  "Outline path, including file #+title, and ENTRY\\='s own heading.
With FILENAME-FALLBACK, use file basename if there is no #+title.

If ENTRY is itself a file-level entry, this still results in a list of
zero or one strings, not two."
  (with-memoization (org-mem--table 24 (list entry filename-fallback))
    (let ((olp (mapcar #'cl-fourth (reverse (org-mem-entry-crumbs entry))))
          file-name-handler-alist)
      ;; The car of `olp' is the potentially nil file title.
      (when (null (car olp))
        (pop olp)
        (when filename-fallback
          (push (file-name-nondirectory (org-mem-entry-file-truename entry))
                olp)))
      olp)))

(defun org-mem-entry-olpath-with-self-with-file-title-or-basename (entry)
  "Outline path, including file #+title, and ENTRY\\='s own heading.
Use file basename if there is no #+title."
  (with-memoization (org-mem--table 29 entry)
    (let ((olp (mapcar #'cl-fourth (reverse (org-mem-entry-crumbs entry))))
          file-name-handler-alist)
      ;; The car of `olp' is the potentially nil file title.
      (when (null (car olp))
        (pop olp)
        (push (file-name-nondirectory (org-mem-entry-file-truename entry))
              olp))
      olp)))

(defun org-mem-entry-title (entry)
  "Like `org-mem-entry-title-maybe' but always return a string.
In the case that ENTRY is a file-level entry with no title, return the
file basename \(file name sans directory component\)."
  (or (org-mem-entry-title-maybe entry)
      (progn (cl-assert (not (org-mem-entry-subtree-p entry)))
             (let (file-name-handler-alist)
               (file-name-nondirectory (org-mem-entry-file-truename entry))))))

(defalias 'org-mem-entry-properties #'org-mem-entry-properties-local
  "Alist of ENTRY properties, no inheritance.

Note the difference from `org-mem-entry-tags', which does include
inherited tags!  This attempts to mirror what you would expect from Org,
since `org-entry-properties' does not use inheritance while
`org-get-tags' does.

To get ancestor properties, use `org-mem-entry-properties-inherited'.

Unlike `org-entry-properties', this omits special properties to return
only the properties explicitly written in the file.")

(defun org-mem-entry-property (prop entry)
  "Value of property PROP in ENTRY."
  (cdr (assoc (upcase prop) (org-mem-entry-properties entry))))

(defun org-mem-entry-property-with-inheritance (prop entry)
  "Value of property PROP in ENTRY."
  ;; NOTE: A value can be nil (i.e. not string "nil" but symbol nil), but
  ;;       should still override any inherited value, so remember to use
  ;;       `assoc' correctly with that in mind.
  (cdr (or (assoc (upcase prop) (org-mem-entry-properties entry))
           (assoc (upcase prop) (org-mem-entry-properties-inherited entry)))))

(defun org-mem-entry-tags (entry)
  "ENTRY tags, with inheritance if allowed at ENTRY.
Combines `org-mem-entry-tags-local' and `org-mem-entry-tags-inherited'."
  (delete-dups (append (org-mem-entry-tags-inherited entry)
                       (org-mem-entry-tags-local entry))))

(define-inline org-mem--iso8601 (int-time)
  "Translate INT-TIME into a string \"yyyy-mm-ddThh:mm\"."
  (inline-quote (format-time-string "%FT%H:%M" ,int-time)))

(defun org-mem-entry-closed (entry)
  "CLOSED-timestamp of ENTRY, suitable for `iso8601-parse'."
  (let ((ts (org-mem-entry-closed-int entry)))
    (and ts (org-mem--iso8601 ts))))

(defun org-mem-entry-deadline (entry)
  "DEADLINE-timestamp of ENTRY, suitable for `iso8601-parse'.
WARNING: If the timestamp is expressed as a diary-sexp,
such as <%%\(memq (calendar-day-of-week date) \\='(1 2 3 4 5)))>,
this returns nil!"
  (let ((ts (org-mem-entry-deadline-int entry)))
    (and ts (org-mem--iso8601 ts))))

(defun org-mem-entry-scheduled (entry)
  "SCHEDULED-timestamp of ENTRY, suitable for `iso8601-parse'.
WARNING: If the timestamp is expressed as a diary-sexp,
such as <%%\(memq (calendar-day-of-week date) \\='(1 2 3 4 5)))>,
this returns nil!"
  (let ((ts (org-mem-entry-scheduled-int entry)))
    (and ts (org-mem--iso8601 ts))))

(defun org-mem-entry-active-timestamps (entry)
  "Active timestamps in ENTRY, suitable for `iso8601-parse'."
  (mapcar #'org-mem--iso8601 (org-mem-entry-active-timestamps-int entry)))

(defun org-mem-entry-clocks (entry)
  "Alist \((START END MINUTES) ...) representing clock lines in ENTRY.
START and END are ISO8601 time strings.
MINUTES is an integer amount of minutes.
To get all three as Lisp time values, use `org-mem-entry-clocks-int'.

Any dangling clock line is represented as just \(START)."
  (cl-loop for (start end secs) in (org-mem-entry-clocks-int entry)
           if end collect (list (org-mem--iso8601 start)
                                (org-mem--iso8601 end)
                                (/ secs 60))
           else collect (list (org-mem--iso8601 start))))

(defun org-mem-entry-dangling-clocks (entry)
  "List \(START1 START2 ...) representing clocks in ENTRY with no end.
See also `org-mem-all-entries-with-dangling-clock'."
  (cl-loop for clock in (org-mem-entry-clocks-int entry)
           when (length= clock 1)
           collect (org-mem--iso8601 (car clock))))


;;; Link info

(defun org-mem-link-file (link)
  "Abbreviated truename of file where LINK is.
See more info at `org-mem-entry-file'."
  (org-mem--fast-abbrev (org-mem-link-file-truename link)))

(defun org-mem-id-link-p (link)
  "Return t if LINK is an `org-mem-link' object of type \"id\"."
  (and (org-mem-link-p link)
       (equal (org-mem-link-type link) "id")))


;;; File info

;; These take plain file-names and we look up associated data elsewhere,
;; because the alternative is imposing on the user to pass around some kind of
;; "file-data" object and keep track of whether the variable they naively
;; named "file" is such an object or a plain file name.

;; These are also polymorphic because the alternative would be writing code
;; like this to find e.g. file title given an entry:
;; (org-mem-file-title (org-mem-entry-file-truename entry))
;; which is... okay.  Until you also have a short alias `org-mem-file-truename'
;; that accepts an entry, making it inconsistent that you cannot also pass
;; an entry to `org-mem-file-title'.  So we make it so you can do that too.

;; Surprising input types are not unprecedented; see Org-roam's
;; `org-roam-node-file-title', which only accepts an entry as input.

(defun org-mem-file-attributes (file/entry/link)
  "The cached `file-attributes' list for file at FILE/ENTRY/LINK.
The uid and gid are strings, see ID-FORMAT in `file-attributes'."
  (nth 1 (org-mem--get-file-metadata file/entry/link)))

(defun org-mem-file-line-count (file/entry/link)
  "Count of lines in whole file at FILE/ENTRY/LINK."
  (nth 2 (org-mem--get-file-metadata file/entry/link)))

(defun org-mem-file-ptmax (file/entry/link)
  "Value of `point-max' in whole file at FILE/ENTRY/LINK.
This is just `org-mem-file-char-count' plus 1."
  (nth 3 (org-mem--get-file-metadata file/entry/link)))

(defun org-mem-file-char-count (file/entry/link)
  "Count of characters in whole file at FILE/ENTRY/LINK.

Often close to but not exactly `org-mem-file-size' due to text encoding.
The coding system used is `org-mem-file-coding-system'."
  (- (nth 3 (org-mem--get-file-metadata file/entry/link)) 1))

(defun org-mem-file-coding-system (file/entry/link)
  "Detected coding system of file at FILE/ENTRY/LINK.

This is the system automatically selected by an \"emacs -Q\" process
when it visits that file, so long as you do not have a non-standard
setting for `org-mem-inject-vars' or `org-mem-load-features' that would
influence that."
  (nth 4 (org-mem--get-file-metadata file/entry/link)))

(defun org-mem-file-size (file/entry/link)
  "Size of file at FILE/ENTRY/LINK, in bytes."
  (file-attribute-size (org-mem-file-attributes file/entry/link)))

(defun org-mem-file-mtime (file/entry/link)
  "Modification time for file at FILE/ENTRY/LINK.
Lisp timestamp in the style of `current-time'.

Note that this timestamp is cached from the last time that Org-mem
scanned the file!  If you need a guaranteed up-to-date result, ask the
filesystem with an expression like:

\(file-attribute-modification-time (file-attributes (org-mem-file ENTRY))\)"
  (file-attribute-modification-time (org-mem-file-attributes file/entry/link)))

;; NOTE: Named "-floor" instead of "-int" because it can matter in downstream
;; code whether it was rounded down or up, and programmers may not think of
;; that until after a bug crops up.
;; While Org-mem has other getters named "-int", those come from Org
;; timestamps which are only precise to the minute anyway.
(defun org-mem-file-mtime-floor (file/entry/link)
  "Modification time for file at FILE/ENTRY/LINK, as integer.
Rounded down.  See also `org-mem-file-mtime'."
  (time-convert (org-mem-file-mtime file/entry/link) 'integer))

;; NOTE: Above getters accept a link as input, and the below could too, but
;; that'd take extra LoC.  Mainly wanted equivalents to
;; `org-roam-node-file-title' and `org-roam-node-file-mtime', and got 'em.
;; Perhaps it was a flaw in Org-roam that it even had those, though.

(defun org-mem-file-title-or-basename (file/entry)
  "Value of #+title in file at FILE/ENTRY; fall back on file basename.
Unlike `org-mem-entry-file-title-strict', always return a string."
  (or (org-mem-file-title-strict file/entry)
      (file-name-nondirectory
       (if (stringp file/entry) file/entry
         (org-mem-entry-file-truename file/entry)))))

;; REVIEW: Docstrings are hard...
;; At least the initial summary sentence (<=72 chars).
;; The following would follow theme of other docstrings, but less informative:
;; "Value of #+title in file at FILE/ENTRY; fall back on topmost heading."
(defun org-mem-file-title-topmost (file/entry)
  "Topmost title in file at FILE/ENTRY, be that a heading or a #+title.
Can refer to a different entry than `org-mem-file-id-topmost', in the
case that there exists a file-level ID but no #+title:, or vice versa.
Can be nil."
  (let ((entries (org-mem-entries-in-file
                  (if (stringp file/entry) file/entry
                    (org-mem-entry-file-truename file/entry)))))
    (or (org-mem-entry-title-maybe (car entries))
        (and (cadr entries)
             (org-mem-entry-title-maybe (cadr entries))))))

(defun org-mem-file-title-strict (file/entry)
  "Value of #+title setting in file at FILE/ENTRY, if any.
Can be nil."
  (org-mem-entry-title-maybe
   (car (org-mem-entries-in-file
         (if (stringp file/entry) file/entry
           (org-mem-entry-file-truename file/entry))))))

(defun org-mem-file-id-topmost (file/entry)
  "ID from file properties or topmost heading in file at FILE/ENTRY.
Can be nil."
  (let ((entries (org-mem-entries-in-file
                  (if (stringp file/entry) file/entry
                    (org-mem-entry-file-truename file/entry)))))
    (or (org-mem-entry-id (car entries))
        (ignore-errors (org-mem-entry-id (cadr entries))))))

(defun org-mem-file-id-strict (file/entry)
  "File-level ID property in file at FILE/ENTRY, if any.
Can be nil."
  (org-mem-entry-id (car (org-mem-entries-in-file
                          (if (stringp file/entry) file/entry
                            (org-mem-entry-file-truename file/entry))))))

;; REVIEW: Find a situation where it matters to use this instead of
;;         just `org-mem--truename-maybe'.
(defun org-mem-file-known-p (file)
  "Return non-nil when FILE is known to org-mem.
Specifically, return the name by which it is known.  This is technically
more restrictive than `org-mem--truename-maybe' by only returning if the
file has been scanned, but in practice they may often be identical."
  (cl-assert (stringp file))
  (or (and (gethash file org-mem--truename<>entries) file)
      (and (setq file (org-mem--truename-maybe file))
           (gethash file org-mem--truename<>entries)
           file)))


;;; Optional: Roam aliases and refs

;; This used to come with `org-mem-roamy-db-mode', but bundling it here:
;; - allows a nicer namespace.
;; - frees Org-node users from needing to enable that mode at all.

;; Despite their names, properties ROAM_ALIASES and ROAM_REFS are not
;; only used by Org-roam, but can be seen as an emerging standard concept.
;; They need special handling because:

;; 1. These properties' values should be transformed from string to list via
;;    bespoke methods, not the generic `org-entry-get-multivalued-property'.
;; 2. The refs and aliases fished out of above lists should be cached, because
;;    they may be consulted a lot (just like vanilla IDs and titles), and it
;;    lets us check for collisions.

(defvar org-mem--id<>roam-refs (make-hash-table :test 'equal)
  "1:1 table mapping an ID to a list of ROAM_REFS substrings.")

(defvar org-mem--roam-ref<>id (make-hash-table :test 'equal)
  "1:1 table mapping a ROAM_REFS member to the nearby ID property.")

;; REVIEW: Efficient since there are usually not many refs in total,
;; but not clean since it is possible for a ref (what we call a "link target"
;; elsewhere) to occur twice with different types --- for example when you
;; have a "http://gnu.org" somewhere and "https://gnu.org" somewhere else.
;;
;; Does not matter for org-node which only uses the table to prettify
;; completions, but since refs may not be unique, the implied contract is
;; false, so other use cases better think carefully.
(defvar org-mem--roam-ref<>type (make-hash-table :test 'equal)
  "1:1 table mapping a ROAM_REFS member to its link type if any.")

(defun org-mem-roam-reflink-p (link)
  "Non-nil if target of LINK is known to exist in some ROAM_REFS.
Relies on up-to-date table `org-mem--roam-ref<>id', so do not trust it
during the hook `org-mem-forget-entry-functions' or similar."
  (and (org-mem-link-p link)
       (gethash (org-mem-link-target link) org-mem--roam-ref<>id)))

(defun org-mem-entry-roam-aliases (entry)
  "Alternative titles for ENTRY, taken from property ROAM_ALIASES."
  (when-let* ((aliases (org-mem-entry-property "ROAM_ALIASES" entry)))
    (split-string-and-unquote aliases)))

(defun org-mem-entry-roam-refs (entry)
  "Link-targets found in ROAM_REFS in ENTRY, if ENTRY has an ID.
These link-targets are determined by `org-mem--split-roam-refs-field'."
  (gethash (org-mem-entry-id entry) org-mem--id<>roam-refs))

(defun org-mem-all-roam-refs ()
  "All ROAM_REFS."
  (hash-table-keys org-mem--roam-ref<>id))

(defun org-mem-all-roam-reflinks ()
  "All links targeting some existing ROAM_REFS."
  (cl-loop for ref being each hash-key of org-mem--roam-ref<>id
           append (gethash ref org-mem--target<>links)))

;; REVIEW: Should we maybe stash roam-refs in the entry object itself,
;;         so we do not need to depend on ID?
(defun org-mem-entry-by-roam-ref (ref)
  "The entry that has a ROAM_REFS property containing REF, and an ID."
  (org-mem-entry-by-id (gethash ref org-mem--roam-ref<>id)))

(defalias 'org-mem-links-to-roam-ref 'org-mem-links-to-target
  "All links to REF.")

(defun org-mem-roam-reflinks-to-entry (entry)
  "All links that point to a substring of ENTRY\\='s ROAM_REFS."
  (cl-loop for ref in (org-mem-entry-roam-refs entry)
           append (org-mem-links-to-roam-ref ref)))

(defun org-mem-roam-reflinks-to-id (id)
  (if-let* ((entry (org-mem-entry-by-id id)))
      (org-mem-roam-reflinks-to-entry entry)
    (error "ID not known: %s" id)))

(defun org-mem-roam-reflinks-into-file (file)
  "Reflinks from anywhere, leading into somewhere in FILE."
  (mapcan #'org-mem-roam-reflinks-to-entry
          (org-mem-entries-in-file file)))

(defun org-mem-roam-reflinks-into-files (files)
  "Reflinks from anywhere, leading into somewhere in FILES."
  (mapcan #'org-mem-roam-reflinks-to-entry
          (org-mem-entries-in-files files)))

(defun org-mem--record-roam-aliases-and-refs (entry)
  "Add ENTRY\\='s ROAM_ALIASES and ROAM_REFS to tables."
  (when-let* ((id (org-mem-entry-id entry)))
    (dolist (alias (org-mem-entry-roam-aliases entry))
      ;; Include aliases in the collision-checks
      (when-let* ((other-id (gethash alias org-mem--title<>id)))
        (unless (string= id other-id)
          (push (list (format-time-string "%H:%M") alias id other-id)
                org-mem--title-collisions)))
      (puthash alias id org-mem--title<>id))
    (when-let* ((refs (org-mem--split-roam-refs-field
                       (org-mem-entry-property "ROAM_REFS" entry))))
      (puthash id refs org-mem--id<>roam-refs)
      (dolist (ref refs)
        (puthash ref id org-mem--roam-ref<>id)))))

(defun org-mem--forget-roam-aliases-and-refs (entry)
  "Remove ENTRY\\='s ROAM_ALIASES and ROAM_REFS from dedicated tables."
  (dolist (ref (org-mem-entry-roam-refs entry))
    (remhash (gethash ref org-mem--roam-ref<>id) org-mem--id<>roam-refs)
    (remhash ref org-mem--roam-ref<>id))
  (dolist (alias (org-mem-entry-roam-aliases entry))
    (remhash alias org-mem--title<>id)))

(defun org-mem--split-roam-refs-field (roam-refs)
  "Extract valid components of a ROAM-REFS field.
What is valid?  See \"org-mem-test.el\"."
  (when roam-refs
    (with-current-buffer (if (eq (current-buffer) org-mem-scratch)
                             org-mem-scratch
                           (setq org-mem-scratch
                                 (get-buffer-create " *org-mem-scratch*" t)))
      (erase-buffer)
      (insert roam-refs)
      (goto-char 1)
      (let (links beg end colon-pos)
        ;; Extract all [[bracketed links]]
        (while (search-forward "[[" nil t)
          (setq beg (match-beginning 0))
          (if (setq end (search-forward "]]" nil t))
              (progn
                (goto-char beg)
                (push (buffer-substring (+ 2 beg) (1- (search-forward "]")))
                      links)
                (delete-region beg end))
            (error "Missing close-bracket in ROAM_REFS property %s" roam-refs)))
        ;; Return merged list
        (cl-loop
         for link? in (append links (split-string-and-unquote (buffer-string)))
         ;; @citekey or &citekey
         if (string-match (rx (or bol (any ";:"))
                              (group (any "@&")
                                     (+ (not (any " ;]")))))
                          link?)
         ;; Replace & with @
         collect (let ((path (substring (match-string 1 link?) 1)))
                   (puthash path nil org-mem--roam-ref<>type)
                   (concat "@" path))
         ;; Some sort of uri://path
         else when (setq colon-pos (string-search ":" link?))
         collect (let ((path (string-replace
                              "%20" " "
                              (substring link? (1+ colon-pos)))))
                   ;; Remember the uri: prefix
                   (puthash path
                            (substring link? 0 colon-pos)
                            org-mem--roam-ref<>type)
                   ;; .. but the actual ref is just the //path
                   path))))))

(add-hook 'org-mem-record-entry-functions
          #'org-mem--record-roam-aliases-and-refs -10)

(add-hook 'org-mem-forget-entry-functions
          #'org-mem--forget-roam-aliases-and-refs -10)


;;; Optional: Short names

;; These definitions are not used inside this file,
;; only convenience for end users (and quick prototyping).
;; Up to them to write code readably.

(defalias 'org-mem-active-timestamps-int            #'org-mem-entry-active-timestamps-int)
(defalias 'org-mem-active-timestamps                #'org-mem-entry-active-timestamps)
(defalias 'org-mem-clocks                           #'org-mem-entry-clocks)
(defalias 'org-mem-clocks-int                       #'org-mem-entry-clocks-int)
(defalias 'org-mem-closed                           #'org-mem-entry-closed)
(defalias 'org-mem-closed-int                       #'org-mem-entry-closed-int)
(defalias 'org-mem-deadline                         #'org-mem-entry-deadline)
(defalias 'org-mem-deadline-int                     #'org-mem-entry-deadline-int)
(defalias 'org-mem-level                            #'org-mem-entry-level)
(defalias 'org-mem-lnum                             #'org-mem-entry-lnum)
(defalias 'org-mem-olpath                           #'org-mem-entry-olpath)
(defalias 'org-mem-olpath-with-self                 #'org-mem-entry-olpath-with-self)
(defalias 'org-mem-priority                         #'org-mem-entry-priority)
(defalias 'org-mem-property                         #'org-mem-entry-property)
(defalias 'org-mem-property-with-inheritance        #'org-mem-entry-property-with-inheritance)
(defalias 'org-mem-properties                       #'org-mem-entry-properties)
(defalias 'org-mem-properties-inherited             #'org-mem-entry-properties-inherited)
(defalias 'org-mem-roam-aliases                     #'org-mem-entry-roam-aliases)
(defalias 'org-mem-roam-refs                        #'org-mem-entry-roam-refs)
(defalias 'org-mem-scheduled                        #'org-mem-entry-scheduled)
(defalias 'org-mem-scheduled-int                    #'org-mem-entry-scheduled-int)
(defalias 'org-mem-stats-cookies                    #'org-mem-entry-stats-cookies)
(defalias 'org-mem-subtree-p                        #'org-mem-entry-subtree-p)
(defalias 'org-mem-tags                             #'org-mem-entry-tags)
(defalias 'org-mem-tags-inherited                   #'org-mem-entry-tags-inherited)
(defalias 'org-mem-tags-local                       #'org-mem-entry-tags-local)
(defalias 'org-mem-text                             #'org-mem-entry-text)
(defalias 'org-mem-todo-state                       #'org-mem-entry-todo-state)
(defalias 'org-mem-olpath-with-file-title             #'org-mem-entry-olpath-with-file-title)
(defalias 'org-mem-olpath-with-file-title-or-basename #'org-mem-entry-olpath-with-file-title-or-basename)
(defalias 'org-mem-olpath-with-self-with-file-title   #'org-mem-entry-olpath-with-self-with-file-title)
(defalias 'org-mem-olpath-with-self-with-file-title-or-basename #'org-mem-entry-olpath-with-self-with-file-title-or-basename)

(defalias 'org-mem-target         #'org-mem-link-target)
(defalias 'org-mem-nearby-id      #'org-mem-link-nearby-id)
(defalias 'org-mem-type           #'org-mem-link-type)
(defalias 'org-mem-citation-p     #'org-mem-link-citation-p)

;;; Short names, with polymorphism

(cl-defgeneric org-mem-pos (entry/link)
  "Char position of ENTRY/LINK."
  (:method ((xx org-mem-entry)) (org-mem-entry-pos xx))
  (:method ((xx org-mem-link)) (org-mem-link-pos xx)))

(cl-defgeneric org-mem-file (entry/link)
  "Abbreviated truename of file where ENTRY/LINK found."
  (:method ((xx org-mem-entry)) (org-mem-entry-file xx))
  (:method ((xx org-mem-link)) (org-mem-link-file xx)))

(cl-defgeneric org-mem-file-truename (entry/link)
  "Truename of file where ENTRY/LINK found."
  (:method ((xx org-mem-entry)) (org-mem-entry-file-truename xx))
  (:method ((xx org-mem-link)) (org-mem-link-file-truename xx)))

(cl-defgeneric org-mem-id (entry/file)
  "ID property of ENTRY/FILE - if file name, the file-level ID."
  (:method ((xx org-mem-entry)) (org-mem-entry-id xx))
  (:method ((xx string)) (org-mem-file-id-strict xx)))

(cl-defgeneric org-mem-title (entry-or-file)
  "Heading title, or file title or basename of ENTRY-OR-FILE.
Like `org-mem-entry-title', this always returns a string."
  (:method ((xx org-mem-entry)) (org-mem-entry-title xx))
  (:method ((xx string)) (org-mem-file-title-or-basename xx)))

(cl-defgeneric org-mem-title-maybe (entry/file)
  "Heading title, or file #+title if ENTRY/FILE is a file name."
  (:method ((xx org-mem-entry)) (org-mem-entry-title-maybe xx))
  (:method ((xx string)) (org-mem-file-title-strict xx)))

(defun org-mem-entries-in (file/files)
  "All entries in FILE/FILES."
  (funcall (if (listp file/files)
               #'org-mem-entries-in-files
             #'org-mem-entries-in-file)
           file/files))


;;; Core logic

(defvar org-mem-initial-scan-hook nil
  "Hook called after the first full scan.")

(defvar org-mem-pre-full-scan-functions nil
  "Hook passed the list of parse-results, before a full reset.")

(defvar org-mem-post-full-scan-functions nil
  "Hook passed the list of parse-results, after a full reset.")

(defvar org-mem-record-file-functions nil
  "Hook passed (FILE ATTRS LINES PTMAX) after adding that info to tables.")

(defvar org-mem-record-entry-functions nil
  "Hook passed one `org-mem-entry' object after adding it to tables.")

(defvar org-mem-record-link-functions nil
  "Hook passed one `org-mem-link' object after adding it to tables.")

(defvar org-mem-pre-targeted-scan-functions nil
  "Hook passed the list of parse-results, before a partial reset.
This occurs before scanning a targeted single file or set of files,
hence the name.  Contrast `org-mem-pre-full-scan-functions'.")

(defvar org-mem-post-targeted-scan-functions nil
  "Hook passed the list of parse-results, after a partial reset.
This occurs after scanning a targeted single file or set of files,
hence the name.  Contrast `org-mem-post-full-scan-functions'.")

(defvar org-mem-forget-file-functions nil
  "Hook passed a file name after removing its info from tables.")

(defvar org-mem-forget-entry-functions nil
  "Hook passed one forgotten `org-mem-entry' object.")

(defvar org-mem-forget-link-functions nil
  "Hook passed one forgotten `org-mem-link' object.")

(defvar org-mem--problems nil)
(defvar org-mem--title-collisions nil)
(defvar org-mem--id-collisions nil)
(defvar org-mem--time-elapsed 1.0)
(defvar org-mem--next-message nil)
(defvar org-mem--time-at-begin-full-scan nil)

(defun org-mem-reset (&optional takeover msg called-interactively)
  "Reset cache, and then if CALLED-INTERACTIVELY, print statistics.
For arguments TAKEOVER and MSG, see `org-mem--scan-full'."
  (interactive "i\ni\np")
  (when called-interactively
    (setq org-mem--next-message t)
    (setq takeover t))
  (org-mem--scan-full takeover msg)
  ;; TODO: Deprecate any particular return value
  (or (and called-interactively
           (org-mem-tip-if-empty))
      msg))

(defun org-mem--scan-full (&optional takeover msg)
  "Arrange a full scan, if one is not already ongoing.
With TAKEOVER t, stop any already ongoing scan to start a new one.

Argument MSG is an optional message to print.  If provided, it also
overrides a default message printed when `org-mem-do-cache-text' is t."
  (when (or takeover (not (el-job-is-busy 'org-mem)))
    (setq org-mem--time-at-begin-full-scan (current-time))
    (when msg
      (message "%s" msg)
      (redisplay t))
    (let ((files (org-mem--list-files-from-fs)))
      (when files
        (el-job-launch :id 'org-mem
                       :if-busy 'takeover
                       :inject-vars (append (org-mem--mk-work-vars) org-mem-inject-vars)
                       :load-features (append '(org-mem-parser) org-mem-load-features)
                       :inputs files
                       :funcall-per-input #'org-mem-parser--parse-file
                       :callback #'org-mem--finalize-full-scan)
        ;; While the subprocesses are parsing each file, let main process
        ;; spend this time caching the raw file contents.
        ;; It may seem that the subprocesses could just send the raw content
        ;; along with other parse-results, but that doubles reset time on my
        ;; machine because it involves `print'ing and `read'ing large strings.
        (when org-mem-do-cache-text
          (if msg (unless (equal msg (current-message))
                    (message "%s" msg))
            (message "Org-mem doing some work in main process..."))
          (redisplay t)
          (with-temp-buffer
            (let ((fn (lambda (files)
                        (dolist (file files)
                          (erase-buffer)
                          (insert-file-contents file)
                          (puthash file (buffer-string) org-mem--truename<>content)))))
              (eval `(let ,org-mem-inject-vars
                       (funcall ,fn ',files))
                    t)))
          (message nil))))))

(defvar org-mem--caused-retry nil)
(defun org-mem--finalize-full-scan (parse-results _job)
  "Handle PARSE-RESULTS from `org-mem--scan-full'."
  (run-hook-with-args 'org-mem-pre-full-scan-functions parse-results)
  (mapc #'clrhash (hash-table-values org-mem--key<>subtable))
  (clrhash org-mem--title<>id)
  (clrhash org-mem--id<>entry)
  (clrhash org-mem--truename<>metadata)
  (clrhash org-mem--truename<>entries)
  (clrhash org-mem--internal-entry-id<>links)
  (clrhash org-mem--target<>links)
  (setq org-mem--title-collisions nil)
  (seq-let (bad-paths file-data entries links problems) parse-results
    (when bad-paths
      (org-mem--invalidate-file-names bad-paths))
    (with-current-buffer
        (setq org-mem-scratch (get-buffer-create " *org-mem-scratch*" t))
      (dolist (fdata file-data)
        (puthash (car fdata) fdata org-mem--truename<>metadata)
        (run-hook-with-args 'org-mem-record-file-functions fdata))
      (mapc #'org-mem--record-entry entries)
      (mapc #'org-mem--record-link links))

    (setq org-mem--time-elapsed
          (float-time (time-since org-mem--time-at-begin-full-scan)))
    (when org-mem--next-message
      (setq org-mem--next-message
            (format
             "Org-mem saw %d files, %d headings, %d links (%d IDs, %d ID-links) in %.2fs"
             (hash-table-count org-mem--truename<>metadata)
             (seq-count #'org-mem-entry-subtree-p (org-mem-all-entries))
             (length (org-mem-all-links))
             (hash-table-count org-mem--id<>entry)
             (length (org-mem-all-id-links))
             org-mem--time-elapsed)))
    (run-hook-with-args 'org-mem-post-full-scan-functions parse-results)
    (when org-mem--next-message
      (message "%s" org-mem--next-message))
    (setq org-mem--next-message nil)

    (while org-mem-initial-scan-hook
      (funcall (pop org-mem-initial-scan-hook)))
    (when bad-paths
      ;; Scan again, but guard against repeating forever.
      (unless (seq-intersection bad-paths org-mem--caused-retry)
        (setq org-mem--caused-retry (append bad-paths org-mem--caused-retry))
        (org-mem--scan-full)))
    (when (and org-mem--title-collisions org-mem-do-warn-title-collisions)
      (message "Some IDs share title, see M-x org-mem-list-title-collisions"))
    (when problems
      (when (and org-mem--problems
                 (string< (caar problems)
                          (caar org-mem--problems)))
        ;; Daily cleanup; last problem's HH:MM looks like the future.
        (setq org-mem--problems nil))
      (setq org-mem--problems (append problems org-mem--problems))
      (message "Scan had problems, see M-x org-mem-list-problems"))))

(defun org-mem--record-link (link)
  "Add info related to LINK to various tables."
  (push link (gethash (org-mem-link-target link) org-mem--target<>links))
  (push link (gethash (org-mem-link--internal-entry-id link)
                      org-mem--internal-entry-id<>links))
  (run-hook-with-args 'org-mem-record-link-functions link))

(defun org-mem--record-entry (entry)
  "Add info related to ENTRY to various tables."
  (let ((id (org-mem-entry-id entry))
        (truename (org-mem-entry-file-truename entry))
        (title (org-mem-entry-title-maybe entry)))
    ;; NOTE: Puts entries in correct order because we're called by
    ;; `org-mem--finalize-full-scan' looping over entries in reverse order.
    (push entry (gethash truename org-mem--truename<>entries))
    (when id
      (org-mem--maybe-snitch-to-org-id entry)
      (when title
        (let ((other-id (gethash title org-mem--title<>id)))
          (when (and other-id (not (string= id other-id)))
            (push (list (format-time-string "%H:%M") title id other-id)
                  org-mem--title-collisions)))
        ;; REVIEW: Should we fallback on file name, i.e. use
        ;; `org-mem-entry-title' instead of `org-mem-entry-title-maybe'?
        ;; It would affect function `org-mem-id-by-title'.
        (puthash title id org-mem--title<>id))
      (puthash id entry org-mem--id<>entry))
    (run-hook-with-args 'org-mem-record-entry-functions entry)))

(defun org-mem--maybe-snitch-to-org-id (entry)
  "Add applicable ENTRY data to `org-id-locations'.
No-op if Org has not loaded."
  (when (and org-mem-do-sync-with-org-id
             (org-mem-entry-id entry)
             (featurep 'org-id)
             (org-mem--try-ensure-org-id-table-p))
    (puthash (org-mem-entry-id entry)
             (org-mem-entry-file entry)
             org-id-locations)))

;; This is non-essential, as org-id works as intended anyway,
;; but other libraries may assume that `org-id-files' is kept up to date.
(defun org-mem--reset-org-id-files (_)
  "Set `org-id-files' to reflect the set of files in `org-id-locations'.
Or no-op, if `org-mem-do-sync-with-org-id' is nil."
  (when (and org-mem-do-sync-with-org-id
             (featurep 'org-id)
             (org-mem--try-ensure-org-id-table-p))
    (setq org-id-files (delete-dups (hash-table-values org-id-locations)))))

(add-hook 'org-mem-post-full-scan-functions #'org-mem--reset-org-id-files)

(defun org-mem--mk-work-vars ()
  "Make alist of variables needed by `org-mem-parser--parse-file'."
  (let ((custom-plain-re (org-mem--mk-plain-re org-mem-seek-link-types))
        (org-link-bracket-re ;; Blob generated by `org-link-make-regexps'.
         "\\[\\[\\(\\(?:[^][\\]\\|\\\\\\(?:\\\\\\\\\\)*[][]\\|\\\\+[^][]\\)+\\)]\\(?:\\[\\([^z-a]+?\\)]\\)?]"))
    (list
     (cons '$bracket-re org-link-bracket-re)
     (cons '$plain-re custom-plain-re)
     (cons '$merged-re (concat org-link-bracket-re "\\|" custom-plain-re))
     (cons '$inlinetask-min-level (bound-and-true-p org-inlinetask-min-level))
     (cons '$nonheritable-tags (bound-and-true-p org-tags-exclude-from-inheritance))
     (cons '$use-tag-inheritance
           (if (boundp 'org-use-tag-inheritance)
               (default-value 'org-use-tag-inheritance)
             t))
     (cons '$default-todo-re
           (let ((default (if (boundp 'org-todo-keywords)
                              (default-value 'org-todo-keywords)
                            '((sequence "TODO" "DONE")))))
             (org-mem-parser--make-todo-regexp
              (string-join (if (stringp (car default))
                               default
                             (apply #'append (mapcar #'cdr default)))
                           " "))))
     ;; These two are unused as yet.
     (cons '$structures-to-ignore (list "src" "comment" "example"))
     (cons '$drawers-to-ignore
           (delete-dups
            (list (or (and (boundp 'org-super-links-backlink-into-drawer)
                           (stringp org-super-links-backlink-into-drawer)
                           org-super-links-backlink-into-drawer)
                      "BACKLINKS")
                  "BACKLINKS"
                  "LOGBOOK"))))))

;; Modified from part of `org-link-make-regexps'
;; I do not understand `rx-let'... may be wrong/unoptimal.
(defun org-mem--mk-plain-re (link-types)
  "Build a moral equivalent to `org-link-plain-re', to match LINK-TYPES."
  (let ((non-space-bracket "[^][ \t\n()<>]"))
    (rx-let ((types-regexp (regexp (regexp-opt link-types t)))
             (parenthesis (seq (any "<([")
		               (0+ (or (regexp non-space-bracket)
			               (seq (any "<([")
			                    (0+ (regexp non-space-bracket))
			                    (any "])>"))))
		               (any "])>"))))
      (rx (seq word-start
               types-regexp
               ":"
               (group (1+ (or (regexp non-space-bracket)
                              parenthesis))
	              (or (regexp "[^[:punct:][:space:]\n]")
                          ?- ?/ parenthesis)))))))


;;; File discovery subroutines

;; Org-mem uses file truenames to be able to merge and de-duplicate file lists
;; from different sources, as well as recognize a user-given file name even if
;; it was not given in truename form.

;; However, `file-truename' is a very expensive Elisp function - 2 full seconds
;; to call it 2,000 times on my machine, which is unacceptable.
;; Thus, some smart workarounds are called for.

(defvar org-mem--wild-filename<>truename (make-hash-table :test 'equal)
  "1:1 table mapping a wild file name to its truename.
See helper `org-mem--truename-maybe'.")

(defun org-mem--truename-maybe (wild-file &optional _)
  "For a WILD-FILE that exists locally, return its truename, else nil.
Caches any non-nil result, so can return a name that is no longer true.
However, even if that becomes the case, it should usually correspond to
known org-mem objects \(because they would be stale as well\).

This function provides a reliable way to find org-mem objects by file
name.  The buffer-local variable `buffer-file-truename' actually gives
you an abbreviated truename, and \"unabbreviating\" it thru
`expand-file-name' is not reliable on account of e.g. buffer-env
changing the meaning of \"~\" or \"~USER\", or runtime changes to
`directory-abbrev-alist'."
  (or (gethash wild-file org-mem--wild-filename<>truename)
      (and (stringp wild-file)
           (not (file-remote-p wild-file))
           (let (file-name-handler-alist)
             (cl-assert (file-name-absolute-p wild-file))
             (if (file-exists-p wild-file)
                 (let* ((truename (file-truename wild-file))
                        (abbr-true (org-mem--fast-abbrev truename)))
                   (puthash abbr-true truename org-mem--wild-filename<>truename)
                   (puthash wild-file truename org-mem--wild-filename<>truename))
               (remhash wild-file org-mem--wild-filename<>truename))))))

(defun org-mem--invalidate-file-names (bad)
  "Scrub bad file names BAD in the tables that can pollute a reset.
Notably, invalidate part of the cache used by `org-mem--truename-maybe'.
If `org-mem-do-sync-with-org-id' t, also scrub `org-id-locations'."
  ;; Example situation: File WILD is a symlink that changed destination.
  ;; So cached TRUE led to a nonexistent file in the last scan.
  ;; Now invalidate it so we cache a correct TRUE next time.
  (maphash (lambda (wild true)
             (when (member true bad)
               (push wild bad)))
           org-mem--wild-filename<>truename)
  (dolist (bad bad)
    (remhash bad org-mem--wild-filename<>truename))
  (when (and org-mem-do-sync-with-org-id
             (org-mem--try-ensure-org-id-table-p))
    (setq org-id-locations
          (org-id-alist-to-hash
           (cl-loop for cell in (org-id-hash-to-alist org-id-locations)
                    unless (member (car cell) bad)
                    collect cell)))))

(defun org-mem--try-ensure-org-id-table-p ()
  "Coerce `org-id-locations' into hash table form, return nil on fail."
  (require 'org-id)
  (and org-id-track-globally
       (or (hash-table-p org-id-locations)
           (ignore-errors
             (setq org-id-locations
                   (org-id-alist-to-hash org-id-locations)))
           ;; No error because some things can still work.
           (progn (message "org-mem: Strange org-id bug, maybe restart Emacs")
                  nil))))


;;; File discovery

(defun org-mem--check-user-settings ()
  "Signal if user options are set to illegal or inefficient values."
  (unless (or org-mem-watch-dirs org-mem-do-sync-with-org-id)
    (user-error "At least one setting must be non-nil: `org-mem-watch-dirs' or `org-mem-do-sync-with-org-id'"))
  (dolist (dir org-mem-watch-dirs)
    (when (file-remote-p dir)
      (user-error "Option `org-mem-watch-dirs' has remote directories"))
    (let (file-name-handler-alist)
      (when (not (file-name-absolute-p dir))
        (user-error "Option `org-mem-watch-dirs' has relative directory names"))
      (dolist (other-dir (mapcar #'file-name-as-directory
                                 (remove dir org-mem-watch-dirs)))
        (when (and (string-prefix-p dir other-dir)
                   ;; NOTE: Remove these clauses if we stop filtering
                   ;; dot/underscore in `org-mem--dir-files-recursive'.
                   (not (eq ?. (aref other-dir (length (file-name-as-directory dir)))))
                   (not (eq ?_ (aref other-dir (length (file-name-as-directory dir))))))
          (message "Option `org-mem-watch-dirs' has redundant subdirectories"))))))

;; Benchmarks.  Try with (setq org-mem-watch-dirs nil) too!

;; (clrhash org-mem--wild-filename<>truename)
;; (benchmark-call #'org-mem--list-files-from-fs)  => 0.043 s
;; (benchmark-call #'org-mem--list-files-from-fs)  => 0.004 s
;; (benchmark-call #'org-roam-list-files)          => 4.145 s

(defvar org-mem--dir<>bare-files (make-hash-table :test 'equal))
(defvar org-mem--dedup-tbl (make-hash-table :test 'equal))
(defun org-mem--list-files-from-fs ()
  "Look for Org files in `org-mem-watch-dirs'.

If user option `org-mem-do-sync-with-org-id' is t,
include files from `org-id-locations' in the result.

Return the file truenames only.
This means you cannot cross-correlate the results with file names in
`org-id-locations', even if that was a discovery source.

Excludes symlinks, remote files, files that do not exist, and duplicate
names.  Uses caching where reasonable, on the assumption that
`org-mem-parser--parse-file' will cause `org-mem--invalidate-file-names'
to run on cached names that turned out to be invalid."
  (clrhash org-mem--dedup-tbl)
  (with-temp-buffer ;; No buffer-env
    (org-mem--check-user-settings)
    (let (file-name-handler-alist)
      ;; NOTE: It is possible to have a true dir name /home/org/,
      ;; then a symlink subdir /home/org/current/ -> /home/org/2025/.
      ;; Fortunately, `org-mem--dir-files-recursive' would not explore
      ;; /home/org/current/.
      ;; That leaves only leaf nodes (files) as possible symlinks.
      (dolist (dir (delete-dups
                    (mapcar #'file-truename
                            (seq-filter #'file-exists-p org-mem-watch-dirs))))
        (dolist (wild (org-mem--dir-files-recursive dir
                                                    org-mem-suffixes
                                                    org-mem-exclude))
          (let ((cached (gethash wild org-mem--wild-filename<>truename)))
            (if cached (puthash cached t org-mem--dedup-tbl)
              (let* ((true (if (file-symlink-p wild) (file-truename wild) wild))
                     (abtrue (org-mem--fast-abbrev true)))
                (puthash true t org-mem--dedup-tbl)
                (puthash wild true org-mem--wild-filename<>truename)
                (puthash abtrue true org-mem--wild-filename<>truename)))))))
    ;; Maybe check org-id-locations.
    (when org-mem-do-sync-with-org-id
      (when (featurep 'org)
        (require 'org-id)
        (unless org-id-track-globally
          (error "If `org-mem-do-sync-with-org-id' is t, `org-id-track-globally' must also be t"))
        (when (and org-id-locations-file (null org-id-locations))
          (org-id-locations-load))
        (when (org-mem--try-ensure-org-id-table-p)
          (clrhash org-mem--dir<>bare-files)
          (dolist (file (delete-dups
                         (nconc (seq-filter #'stringp
                                            (if (symbolp org-id-extra-files)
                                                (symbol-value org-id-extra-files)
                                              org-id-extra-files))
                                (hash-table-values org-id-locations))))
            (when (cl-loop for exclude in org-mem-exclude
                           never (string-search exclude file))
              (let ((cached (gethash file org-mem--wild-filename<>truename)))
                (if cached (puthash cached t org-mem--dedup-tbl)
                  (unless (file-remote-p file)
                    (let (file-name-handler-alist)
                      (when (file-exists-p file)
                        (push (file-name-nondirectory file)
                              (gethash (file-name-directory file)
                                       org-mem--dir<>bare-files)))))))))
          ;; PERF: Use interim table `org-mem--dir<>bare-files' so we
          ;;       can limit calling `file-truename' to once per directory.
          (maphash
           (lambda (dir bare-files)
             (let* ((file-name-handler-alist nil)
                    (true-dir (file-truename dir)))
               (dolist (bare-file bare-files)
                 (let* ((wild (concat dir bare-file))
                        (true (concat true-dir bare-file))
                        (_ (when (file-symlink-p true)
                             (setq true (file-truename true))))
                        (abtrue (org-mem--fast-abbrev true)))
                   (puthash true t org-mem--dedup-tbl)
                   (puthash wild true org-mem--wild-filename<>truename)
                   (puthash abtrue true org-mem--wild-filename<>truename)))))
           org-mem--dir<>bare-files)))))
  (hash-table-keys org-mem--dedup-tbl))

;; REVIEW: We can get rid of this.  In past benchmarks, it only seemed 3-5x
;; faster than `directory-files-recursively' (for our use case, with EXCLUDES etc).
;; In real numbers on my machine, that's adding 0.05s to one `org-mem-reset'
;; which takes about 1.90s total.
;; It was cooler back in org-node before 2.2.0 when the total was only ~0.90s
;; due to collecting less data.
;; Starting to see why a lot of software gets slower as it gets more
;; sophisticated!  Not only due to the sophistication, but other optimizations
;; look relatively less worth the LoC burden.
(defun org-mem--dir-files-recursive (dir suffixes excludes)
  "Faster, purpose-made variant of `directory-files-recursively'.
Return a list of all files under directory DIR, its
sub-directories, sub-sub-directories and so on, with provisos:

- Don\\='t enter directories that are symlinks.
- Don\\='t enter directories whose name start with dot or underscore.
- Don\\='t enter directories where some substring of the full name
  matches one of strings EXCLUDES literally.
- Don\\='t collect any file where some substring of the non-directory
  name matches one of strings EXCLUDES literally.
- Collect only files that end in one of SUFFIXES literally.
- Don\\='t sort final results in any particular order.

Does not modify the match data."
  (let (result)
    (dolist (file (file-name-all-completions "" dir))
      (if (directory-name-p file)
          (unless (or (string-prefix-p "." file)
                      (string-prefix-p "_" file))
            (setq file (file-name-concat dir file))
            (unless (or (cl-loop for substr in excludes
                                 thereis (string-search substr file))
                        (file-symlink-p (directory-file-name file)))
              (setq result (nconc result (org-mem--dir-files-recursive
        		                  file suffixes excludes)))))
        (when (cl-loop for suffix in suffixes
                       thereis (string-suffix-p suffix file))
          (unless (cl-loop for substr in excludes
                           thereis (string-search substr file))
            (push (file-name-concat dir file) result)))))
    result))


;;; Assorted tools for downstream packages

(defun org-mem-await (message n-secs)
  "Wait for up to N-SECS for any current org-mem subprocesses to finish.
Return t on finish, or nil if N-SECS elapsed without finishing.
If there was no subprocess at work, return t.

String MESSAGE is printed in the echo area during the wait, to help
inform the user why Emacs hangs.

As an obsolete calling convention, MESSAGE can also be a symbol
corresponding to your package name."
  (when (symbolp message)
    (setq message (format "%S waiting for org-mem..." message)))
  (el-job-await 'org-mem-targeted n-secs message)
  (el-job-await 'org-mem n-secs message))

;; REVIEW: Mixed feelings about including this tool, but it's the obvious tool
;; to use with `org-mem-entry-text' to generate backlink previews, for
;; example, and it is apparently rare to realize the perf impact of
;; opening many Org buffers.
;; Maybe if Org doesn't fix or can't fix the startup perf, they can ship an
;; "org-scratch" function like this?  It belongs upstream.
(defun org-mem-org-mode-scratch (&optional bufname)
  "Get or create a hidden `org-mode' buffer.
Ignore `org-mode-hook' and startup options.

Like a temp buffer, but does not clean up.
You should probably use `erase-buffer' in case it already contains text.
BUFNAME defaults to \" *org-mem-org-mode-scratch*\"."
  (require 'org)
  (setq bufname (or bufname " *org-mem-org-mode-scratch*"))
  (or (get-buffer bufname)
      (let ((org-inhibit-startup t)
            (org-agenda-files nil)
            (org-element-cache-persistent nil))
        (with-current-buffer (get-buffer-create bufname t)
          (delay-mode-hooks
            (org-mode))
          (setq-local org-element-cache-persistent nil)
          (current-buffer)))))

(defun org-mem-fontify-like-org (string)
  "Return STRING with text properties from fontifying it in `org-mode'."
  (with-current-buffer (org-mem-org-mode-scratch)
    (erase-buffer)
    (insert string)
    (font-lock-ensure)
    (buffer-string)))

(defun org-mem-tip-if-empty ()
  (when (hash-table-empty-p org-mem--wild-filename<>truename)
    (let ((msg (if org-mem-do-sync-with-org-id
                   (if (and (not (featurep 'org-id))
                            (not org-mem-watch-dirs))
                       "org-mem: No org-ids found because Org not loaded"
                     (format "org-mem: No org-ids found.  If you know they exist, try M-x %S."
                             (if (fboundp 'org-roam-update-org-id-locations)
                                 'org-roam-update-org-id-locations
                               'org-id-update-id-locations)))
                 "No files found under `org-mem-watch-dirs'")))
      (message msg)
      msg)))


;;; End-user tool

(defun org-mem-forget-id-locations-recursively (dir)
  "Remove all references in `org-id-locations' to any files under DIR.

Note that if DIR descends from a member of `org-mem-watch-dirs',
this action may make no practical impact unless you also add DIR to
`org-mem-exclude'.
This is because with `org-mem-do-sync-with-org-id' t, they simply get
added again on next scan.

Tip: In case of unsolvable problems, eval this to thoroughly wipe
org-id-locations:

\(progn
  (delete-file org-id-locations-file)
  (setq org-id-locations nil)
  (setq org-id--locations-checksum nil)
  (setq org-agenda-text-search-extra-files nil)
  (setq org-id-files nil)
  (setq org-id-extra-files nil))"
  (interactive "DForget all IDs recursively in directory: ")
  (require 'org-id)
  (let ((files (org-mem--dir-files-recursive dir org-mem-suffixes nil)))
    (when files
      (setq files (append files (seq-keep #'org-mem--truename-maybe files)))
      (message "Forgetting all IDs in directory %s..." dir)
      (redisplay t)
      (maphash (lambda (id file)
                 (when (member file files)
                   (remhash id org-mem--id<>entry)
                   (remhash id org-id-locations)))
               org-id-locations)
      ;; Bonus, probably unnecessary
      (dolist (file files)
        (dolist (entry (gethash file org-mem--truename<>entries))
          (remhash (org-mem-entry--internal-id entry)
                   org-mem--internal-entry-id<>links))
        (remhash file org-mem--truename<>entries)
        (remhash file org-mem--truename<>metadata))
      (setq org-id-files (cl-nset-difference org-id-files files :test #'equal))
      (org-id-locations-save)
      (message "Forgetting all IDs in directory %s...done" dir)
      (org-mem--scan-full))))


(defvar org-mem--bump-int 12 "Not a version number, but bumped sometimes.")
(defmacro org-mem--def-whiny-alias (old new when removed-by)
  "Define function OLD as effectively an alias for NEW.
Also, calling OLD will emit a deprecation warning the first time.
String WHEN says when it was deprecated and REMOVED-BY when it
may be removed from the package."
  `(let (checked-once)
     (defun ,(cadr old) (&rest args)
       (declare (obsolete ,(cadr new) ,when))
       (unless checked-once
         (setq checked-once t)
         (lwarn ,old :warning "Obsolete since %s, will be removed by %s; use `%s' instead. (Check your initfiles)"
                ,when ,removed-by ,new))
       (apply ,new args))))

(org-mem--def-whiny-alias 'org-mem-link-dest                         #'org-mem-link-target                             "0.8.0 (2025-05-15)" "2025-11-30")
(org-mem--def-whiny-alias 'org-mem-dest                              #'org-mem-target                                  "0.8.0 (2025-05-15)" "2025-11-30")
(org-mem--def-whiny-alias 'org-mem-x-fontify-like-org                #'org-mem-fontify-like-org                        "0.10.0 (2025-05-18)" "2025-11-30")
(org-mem--def-whiny-alias 'org-mem-block                             #'org-mem-await                                   "0.12.0 (2025-05-22)" "2025-11-30")
(org-mem--def-whiny-alias 'org-mem--abbr-truename                    #'org-mem--truename-maybe                         "0.12.0 (2025-05-22)" "2025-11-30")
(org-mem--def-whiny-alias 'org-mem-entry-olpath-with-title           #'org-mem-entry-olpath-with-file-title            "0.13.1 (2025-05-28)" "2025-11-30")
(org-mem--def-whiny-alias 'org-mem-entry-olpath-with-title-with-self #'org-mem-entry-olpath-with-self-with-file-title  "0.13.1 (2025-05-28)" "2025-11-30")
(org-mem--def-whiny-alias 'org-mem-entry-olpath-with-self-with-title #'org-mem-entry-olpath-with-self-with-file-title  "0.13.1 (2025-05-28)" "2025-11-30")
(org-mem--def-whiny-alias 'org-mem-olpath-with-title                 #'org-mem-olpath-with-file-title                  "0.13.1 (2025-05-28)" "2025-11-30")
(org-mem--def-whiny-alias 'org-mem-olpath-with-title-with-self       #'org-mem-olpath-with-self-with-file-title        "0.13.1 (2025-05-28)" "2025-11-30")
(org-mem--def-whiny-alias 'org-mem-olpath-with-self-with-title       #'org-mem-olpath-with-self-with-file-title        "0.13.1 (2025-05-28)" "2025-11-30")
(org-mem--def-whiny-alias 'org-mem-file-mtime-int                    #'org-mem-file-mtime-floor                        "0.14.0 (2025-05-30)" "2025-11-30")
(org-mem--def-whiny-alias 'org-mem-entries-with-active-timestamps    #'org-mem-all-entries-with-active-timestamps      "0.14.0 (2025-05-30)" "2025-11-30")
(org-mem--def-whiny-alias 'org-mem-files-with-active-timestamps      #'org-mem-all-files-with-active-timestamps        "0.14.0 (2025-05-30)" "2025-11-30")
(org-mem--def-whiny-alias 'org-mem-heading-lvl                       #'org-mem-level                                   "0.21.0 (2025-10-01)" "2026-01-30")
(defvar org-mem--file<>metadata :renamed)
(defvar org-mem--file<>entries  :renamed)

(define-obsolete-function-alias 'org-mem-entry-olpath-with-file-title-with-self #'org-mem-entry-olpath-with-self-with-file-title  "0.22.0")
(define-obsolete-function-alias 'org-mem-olpath-with-file-title-with-self       #'org-mem-olpath-with-self-with-file-title        "0.22.0")

(provide 'org-mem)

;;; org-mem.el ends here
