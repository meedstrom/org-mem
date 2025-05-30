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
;; Package-Version: 0.14.1
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
(defvar org-element-cache-persistent)
(defvar org-inhibit-startup)
(defvar org-agenda-files)
(declare-function org-id-locations-load "org-id")
(declare-function org-id-alist-to-hash "org-id")
(declare-function org-id-hash-to-alist "org-id")
(declare-function org-id-locations-save "org-id")
(define-obsolete-variable-alias 'org-mem--file<>metadata 'org-mem--truename<>metadata "0.12.6 (May 2025)")
(define-obsolete-variable-alias 'org-mem--file<>entries  'org-mem--truename<>entries  "0.12.6 (May 2025)")
(define-obsolete-variable-alias 'org-mem-watch-dirs-exclude 'org-mem-exclude          "0.13.0 (May 2025)")

(defgroup org-mem nil "Fast info from a large amount of Org file contents."
  :group 'org)

;; REVIEW: I wonder if it may be worth making `org-mem-entry-text' into a
;; function that actually does `insert-file-contents' in a hidden buffer, then
;; keeps one such buffer alive for every file accessed so far.  Because slow
;; resets run counter to the package's philosophy.
;; FWIW, this only makes the reset so slow because interprocess communication
;; implies the child must do `prin1' on the whole text string before sending
;; it to the parent.  If it could send a raw string, it'd probably be fast.
;; Come to think, since the parent process is doing nothing while the children
;; are working, perhaps it could get the texts itself.
;; Into an "org-mem--file<>content" table, at least, not bothering to carve
;; them up by entry until later.
(defcustom org-mem-do-cache-text nil
  "Whether to also cache text contents of all entries.
Likely to slow down `org-mem-reset'.

This makes the raw text available via accessor `org-mem-entry-text'."
  :type 'boolean
  :package-version '(org-mem . "0.9.0"))

(defcustom org-mem-do-warn-title-collisions t
  "Whether to print a message when two ID-nodes have the same title.

To check manually, type \\[org-mem-list-title-collisions]."
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
  To check such a directory, add its full path explicitly.
- Subdirectories that are symlinks.
- Anything matching `org-mem-exclude'.

Can be left at nil, if `org-mem-do-sync-with-org-id' is t.
Benefits of configuring it anyway:

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
    ".sync-conflict-"
    ".#"
    ".pam/"
    "/backup")
  "Literal substrings of file paths that should not be scanned.
Aside from this variable, some filters are hard-coded:

- We only scan files that end in precisely \".org\" or \".org_archive\"
  - Thus backups ending in ~, # or similar are excluded in any case
- We exclude symlinks

Main reason to configure this is to prevent counting back-ups
and autosave files as duplicate ID locations,
especially inside `org-mem-watch-dirs'.

You can also speed up `org-mem-reset' a bit by excluding directories
found inside `org-mem-watch-dirs' with a very large amount of files \(on
the order of 100,000), such as the infamous \"node_modules\"."
  :type '(repeat string)
  :package-version '(org-mem . "0.13.0"))

(defcustom org-mem-seek-link-types
  '("http" "https" "id" "file")
  "Which types of plain \(un-bracketed\) links to look for."
  :type '(repeat string)
  :package-version '(org-mem . "0.2.0"))

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
  "1:1 table mapping a heading, file-title or alias to an ID.")

(defvar org-mem--id<>entry (make-hash-table :test 'equal)
  "1:1 table mapping an ID to an `org-mem-entry' record.")

(defvar org-mem--truename<>entries (make-hash-table :test 'equal)
  "1:N table mapping a file name to a sorted list of `org-mem-entry' records.
Sorted by field `org-mem-entry-pos'.")

(defvar org-mem--target<>links (make-hash-table :test 'equal)
  "1:N table mapping a link target to a list of `org-mem-link' records.
The list represents all links from anywhere that have that exact target.

A target is a citekey or the path component of an Org link \(see Group 2
in `org-link-plain-re').  In practice, it is often an org-id like
\"57707152-9c05-43f5-9f88-5d4f3a0d019a\", an URI path like
\"//www.gnu.org\", or a citekey like \"@ioannidis2005\".

NOTE! A future version may omit the sigil @ in citekeys.")

(defvar org-mem--internal-entry-id<>links (make-hash-table :test 'eq)
  "1:N table mapping internal entry ID to list of `org-mem-link' records.")

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

(defvar org-mem--truename<>metadata (make-hash-table :test 'equal)
  "1:1 table mapping a file name to a list of assorted data.

Users have no reason to inspect this table, prefer stable API
in `org-mem-file-mtime' and friends.")

(defun org-mem--get-file-metadata (file/entry/link)
  "Return list of assorted data if FILE/ENTRY/LINK known, else error."
  (let ((wild-file (if (stringp file/entry/link)
                       file/entry/link
                     (if (org-mem-entry-p file/entry/link)
                         (org-mem-entry-file-truename file/entry/link)
                       (org-mem-link-file-truename file/entry/link)))))
    (or (gethash wild-file org-mem--truename<>metadata)
        (gethash (org-mem--truename-maybe wild-file) org-mem--truename<>metadata)
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

(cl-defstruct (org-mem-link (:constructor nil) (:copier nil))
  (file-truename      () :read-only t :type string)
  (pos                () :read-only t :type integer)
  (type               () :read-only t :type string)
  (target             () :read-only t :type string)
  (description        () :read-only t :type string)
  (citation-p         () :read-only t :type boolean)
  (nearby-id          () :read-only t :type string)
  (-internal-entry-id () :read-only t :type integer))

(cl-defstruct (org-mem-entry (:constructor nil) (:copier nil))
  (file-truename  () :read-only t :type string)
  (lnum           () :read-only t :type integer)
  (pos            () :read-only t :type integer)
  (title-maybe    () :read-only t :type string)
  (level          () :read-only t :type integer)
  (id             () :read-only t :type string)
  (closed         () :read-only t :type string)
  (crumbs         () :read-only t :type list)
  (deadline       () :read-only t :type string)
  (priority       () :read-only t :type string)
  (properties     () :read-only t :type list)
  (scheduled      () :read-only t :type string)
  (tags-inherited () :read-only t :type list)
  (tags-local     () :read-only t :type list)
  (todo-state     () :read-only t :type string)
  (-internal-id   () :read-only t :type integer)
  (text           () :read-only t :type string)
  (active-timestamps () :read-only t :type list)
  (clocks-int     () :read-only t :type list))


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
    (mapcar #'org-mem--fast-abbrev (hash-table-keys org-mem--truename<>metadata))))

(defun org-mem-all-file-truenames ()
  "Truename of all Org files.
When in doubt, you should prefer `org-mem-all-files', because
`directory-abbrev-alist' exists for a reason.
However, org-mem uses truenames internally. See `org-mem-file-known-p'."
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
  (seq-filter #'org-mem-entry-active-timestamps (org-mem-all-entries)))

(defun org-mem-all-files-with-active-timestamps ()
  (with-memoization (org-mem--table 0 'org-mem-all-files-with-active-timestamps)
    (cl-loop for file in (org-mem-all-files)
             when (seq-find #'org-mem-entry-active-timestamps
                            (org-mem-entries-in-file file))
             collect file)))

(defun org-mem-all-entries-with-dangling-clock ()
  (cl-loop for entry in (org-mem-all-entries)
           when (seq-find (##length= % 1)
                          (org-mem-entry-clocks-int entry))
           collect entry))

(defun org-mem-entry-by-id (id)
  "The entry with unique :ID: property equal to ID."
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
represents the content before the first heading.
Note 2025-05-13: The last fact may change in the future."
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
  (cl-loop for link in (gethash path org-mem--target<>links)
           when (equal type (org-mem-link-type link))
           collect link))

(defun org-mem-id-links-to-entry (entry)
  "All ID-links that point to ENTRY."
  (and-let* ((id (org-mem-entry-id entry)))
    (org-mem-id-links-to-id id)))

(defun org-mem-links-to-target (target)
  (cl-assert (stringp target))
  (gethash target org-mem--target<>links))

(defun org-mem-id-links-to-id (id)
  "All ID-links targeting ID."
  (with-memoization (org-mem--table 14 id)
    (when-let* ((links (org-mem-links-to-target id)))
      ;; Vast majority of people, this filter is safe to skip.  But it's
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

(defun org-mem-links-to-entry (_entry)
  "(Unimplemented)"
  (error "Unimplemented"))

(defun org-mem-links-to-file (_file)
  "(Unimplemented)"
  (error "Unimplemented"))


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

(defun org-mem-entry-olpath (entry)
  "Outline path to ENTRY."
  (with-memoization (org-mem--table 25 entry)
    (mapcar #'cl-fourth (cdr (reverse (cdr (org-mem-entry-crumbs entry)))))))

(defun org-mem-entry-olpath-with-self (entry)
  "Outline path, including ENTRY\\='s own heading."
  (with-memoization (org-mem--table 26 entry)
    (mapcar #'cl-fourth (cdr (reverse (org-mem-entry-crumbs entry))))))

(defun org-mem-entry-olpath-with-self-with-file-title (entry &optional filename-fallback)
  "Outline path, including file #+title, and ENTRY\\='s own heading.
With FILENAME-FALLBACK, use file basename if there is no #+title."
  (let ((olp (mapcar #'cl-fourth (reverse (org-mem-entry-crumbs entry))))
        file-name-handler-alist) ;; perf
    (when (null (car olp))
      (pop olp)
      (when filename-fallback
        (push (file-name-nondirectory (org-mem-entry-file-truename entry))
              olp)))
    olp))

(defalias 'org-mem-entry-olpath-with-file-title-with-self
  #'org-mem-entry-olpath-with-self-with-file-title)

(defun org-mem-entry-olpath-with-file-title (entry &optional filename-fallback)
  "Outline path to ENTRY, including file #+title.
With FILENAME-FALLBACK, use file basename if there is no #+title."
  (let ((olp (mapcar #'cl-fourth (reverse (cdr (org-mem-entry-crumbs entry)))))
        file-name-handler-alist) ;; perf
    (when (null (car olp))
      (pop olp)
      (when filename-fallback
        (push (file-name-nondirectory (org-mem-entry-file-truename entry))
              olp)))
    olp))

(defun org-mem-entry-title (entry)
  "Like `org-mem-entry-title-maybe' but always return a string.
In the case that ENTRY is a file-level entry with no title, return the
file base name."
  (or (org-mem-entry-title-maybe entry)
      (progn (cl-assert (not (org-mem-entry-subtree-p entry)))
             (let (file-name-handler-alist)
               (file-name-nondirectory (org-mem-entry-file-truename entry))))))

(defun org-mem-entry-property (prop entry)
  "Value of property PROP in ENTRY."
  (cdr (assoc (upcase prop) (org-mem-entry-properties entry))))

;; TODO: It would surely be useful to be able to get inherited tags even where
;;       it is not allowed.  Currently `org-mem-entry-tags-inherited' is just
;;       nil in that case, but maybe a separate field?
(defun org-mem-entry-tags (entry)
  "ENTRY tags, with inheritance if allowed at ENTRY."
  (delete-dups (append (org-mem-entry-tags-inherited entry)
                       (org-mem-entry-tags-local entry))))

(defun org-mem-entry-clocks (entry)
  "Alist \((START END DURATION) ...) representing clock lines in ENTRY.
Any dangling clock line is represented as just \(START)."
  (with-memoization (org-mem--table 24 entry)
    (cl-loop for (start end mins) in (org-mem-entry-clocks-int entry)
             if end
             collect (list (format-time-string "%FT%H:%M" start)
                           (format-time-string "%FT%H:%M" end)
                           mins)
             else collect (format-time-string "%FT%H:%M" start))))

(defun org-mem-entry-dangling-clocks (entry)
  "List \(START1 START2 ...) representing unfinished clocks in ENTRY.
See also `org-mem-all-entries-with-dangling-clock'."
  (cl-loop for clock in (org-mem-entry-clocks-int entry)
           when (length= clock 1)
           collect (format-time-string "%FT%H:%M" (car clock))))


;;; Link info

(defun org-mem-link-file (link)
  "Abbreviated truename of file where LINK is.
See more info at `org-mem-entry-file'."
  (org-mem--fast-abbrev (org-mem-link-file-truename link)))

(defun org-mem-id-link-p (link)
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

(defun org-mem-file-attributes (file/entry/link)
  "The `file-attributes' list for file at FILE/ENTRY/LINK."
  (nth 1 (org-mem--get-file-metadata file/entry/link)))

(defun org-mem-file-line-count (file/entry/link)
  "Count of lines in whole file at FILE/ENTRY/LINK."
  (let ((x (nth 2 (org-mem--get-file-metadata file/entry/link))))
    (if (> x 0) x
      (error "org-mem-file-line-count: Value not yet stored for file %s%s"
             file/entry/link
             "\nLikely due to scan errors, type M-x org-mem-list-problems"))))

(defun org-mem-file-ptmax (file/entry/link)
  "Count of characters in whole file at FILE/ENTRY/LINK.
Often close to but not exactly the size in bytes due to text encoding."
  (let ((x (nth 3 (org-mem--get-file-metadata file/entry/link))))
    (if (> x 0) x
      (error "org-mem-file-ptmax: Value not yet stored for file %s%s"
             file/entry/link
             "\nLikely due to scan errors, type M-x org-mem-list-problems"))))

(defun org-mem-file-coding-system (file/entry/link)
  "Detected coding system of file at FILE/ENTRY/LINK."
  (nth 4 (org-mem--get-file-metadata file/entry/link)))

(defun org-mem-file-size (file/entry/link)
  "Size of file at FILE/ENTRY/LINK, in bytes."
  (file-attribute-size (org-mem-file-attributes file/entry/link)))

(defun org-mem-file-mtime (file/entry/link)
  "Modification time for file at FILE/ENTRY/LINK."
  (file-attribute-modification-time (org-mem-file-attributes file/entry/link)))

(defun org-mem-file-mtime-floor (file/entry/link)
  "Modification time for file at FILE/ENTRY/LINK, as floor integer."
  (time-convert (org-mem-file-mtime file/entry/link) 'integer))

;; TODO: Upstream `time-convert' docstring should say it takes the floor...
(defun org-mem-file-mtime-int (file/entry/link)
  "Modification time for file at FILE/ENTRY/LINK, as ceiling integer."
  (declare (obsolete org-mem-file-mtime-floor "2025-05-30"))
  (ceiling (float-time (org-mem-file-mtime file/entry/link))))

;; Above getters accept a link as input, and the below could too
;; but that'd take extra LoC.  Mainly wanted equivalents to
;; `org-roam-node-file-title' and `org-roam-node-file-mtime', and got 'em.

(defun org-mem-file-title-or-basename (file/entry)
  "Value of #+title in file at FILE/ENTRY; fall back on file basename.
Unlike `org-mem-entry-file-title-strict' which may return nil,
this always returns a string."
  (or (org-mem-file-title-strict file/entry)
      (file-name-nondirectory
       (if (stringp file/entry) file/entry
         (org-mem-entry-file-truename file/entry)))))

(defun org-mem-file-title-topmost (file/entry)
  "Topmost title in file at FILE/ENTRY, be that a heading or a #+title.
Can refer to a different entry than `org-mem-file-id-topmost', in the
case that there exists a file-level ID but no #+title:, or vice versa."
  ;; docstrings are hard
  ;; "Value of #+title in file at FILE/ENTRY; fall back on topmost heading."
  (let ((entries (org-mem-entries-in-file
                  (if (stringp file/entry) file/entry
                    (org-mem-entry-file-truename file/entry)))))
    (or (org-mem-entry-title-maybe (car entries))
        (ignore-errors (org-mem-entry-title (cadr entries))))))

(defun org-mem-file-title-strict (file/entry)
  "Value of #+title setting in file at FILE/ENTRY, if any."
  (org-mem-entry-title-maybe
   (car (org-mem-entries-in-file
         (if (stringp file/entry) file/entry
           (org-mem-entry-file-truename file/entry))))))

(defun org-mem-file-id-topmost (file/entry)
  "ID from file properties or topmost subtree in file at FILE/ENTRY."
  (let ((entries (org-mem-entries-in-file
                  (if (stringp file/entry) file/entry
                    (org-mem-entry-file-truename file/entry)))))
    (or (org-mem-entry-id (car entries))
        (ignore-errors (org-mem-entry-id (cadr entries))))))

(defun org-mem-file-id-strict (file/entry)
  "File-level ID property in file at FILE/ENTRY, if any."
  (org-mem-entry-id (car (org-mem-entries-in-file
                          (if (stringp file/entry) file/entry
                            (org-mem-entry-file-truename file/entry))))))

;; REVIEW: If someone thinks this scheme is silly, you're probably on to something.
;;         PR welcome.  IDK what, but I sense room for simplification.
(defun org-mem-file-known-p (file)
  "Return non-nil when FILE is known to org-mem.
Specifically, return the name by which it is known.  This is technically
more restrictive than `org-mem--truename-maybe' by only returning if the
file has been scanned, but in practice they may be identical."
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
;; elsewhere) to occur twice with different types.
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

;; TODO: Should we maybe stash roam-refs in the entry object itself,
;;       so we do not need to depend on ID?
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

(defalias 'org-mem-active-timestamps                #'org-mem-entry-active-timestamps)
(defalias 'org-mem-clocks                           #'org-mem-entry-clocks)
(defalias 'org-mem-clocks-int                       #'org-mem-entry-clocks-int)
(defalias 'org-mem-closed                           #'org-mem-entry-closed)
(defalias 'org-mem-deadline                         #'org-mem-entry-deadline)
(defalias 'org-mem-heading-lvl                      #'org-mem-entry-level) ;; feels more legible
(defalias 'org-mem-level                            #'org-mem-entry-level)
(defalias 'org-mem-lnum                             #'org-mem-entry-lnum)
(defalias 'org-mem-olpath                           #'org-mem-entry-olpath)
(defalias 'org-mem-olpath-with-self                 #'org-mem-entry-olpath-with-self)
(defalias 'org-mem-priority                         #'org-mem-entry-priority)
(defalias 'org-mem-property                         #'org-mem-entry-property)
(defalias 'org-mem-properties                       #'org-mem-entry-properties)
(defalias 'org-mem-roam-aliases                     #'org-mem-entry-roam-aliases)
(defalias 'org-mem-roam-refs                        #'org-mem-entry-roam-refs)
(defalias 'org-mem-scheduled                        #'org-mem-entry-scheduled)
(defalias 'org-mem-subtree-p                        #'org-mem-entry-subtree-p)
(defalias 'org-mem-tags                             #'org-mem-entry-tags)
(defalias 'org-mem-tags-inherited                   #'org-mem-entry-tags-inherited)
(defalias 'org-mem-tags-local                       #'org-mem-entry-tags-local)
(defalias 'org-mem-text                             #'org-mem-entry-text)
(defalias 'org-mem-todo-state                       #'org-mem-entry-todo-state)
(defalias 'org-mem-olpath-with-file-title           #'org-mem-entry-olpath-with-file-title)
(defalias 'org-mem-olpath-with-file-title-with-self #'org-mem-entry-olpath-with-file-title-with-self)
(defalias 'org-mem-olpath-with-self-with-file-title #'org-mem-entry-olpath-with-self-with-file-title)

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

(cl-defgeneric org-mem-title (entry/file)
  "Heading title, or file title or basename if ENTRY/FILE is a file name.
Like `org-mem-entry-title', it always returns a string."
  (:method ((xx org-mem-entry)) (org-mem-entry-title xx))
  (:method ((xx string)) (org-mem-file-title-or-basename xx)))

(cl-defgeneric org-mem-title-maybe (entry/file)
  "Heading title, or file #+title if ENTRY/FILE is a file name."
  (:method ((xx org-mem-entry)) (org-mem-entry-title-maybe xx))
  (:method ((xx string)) (org-mem-file-title-strict xx)))

(defun org-mem-entries-in (file/files)
  "All entries in FILE/FILES."
  (funcall (if (listp file/files) #'org-mem-entries-in-files
             #'org-mem-entries-in-file)
           file/files))

(defun org-mem-roam-reflinks-to (_)
  "(Unimplemented)"
  (error "Unimplemented"))

(defun org-mem-links-to (_)
  "(Unimplemented)"
  (error "Unimplemented"))

(defun org-mem-id-links-to (_)
  "(Unimplemented)"
  (error "Unimplemented"))


;;; Core logic

(defvar org-mem-initial-scan-hook nil
  "Hook called after the first full scan, possibly also second scan.
The second scan occurs only in some cases where symlinks had to be
resolved, and then it occurs immediately after the first.")

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

(defun org-mem-reset (&optional interactively)
  "Reset cache, and if called INTERACTIVELY, print statistics."
  (interactive "p")
  (when interactively
    (setq org-mem--next-message t)
    (when current-prefix-arg
      (clrhash org-mem--wild-filename<>truename)))
  (org-mem--scan-full))

(defun org-mem--scan-full (&optional takeover)
  "Arrange a full scan, if one is not already ongoing.
With TAKEOVER t, stop any already ongoing scan to start a new one."
  (when (or takeover (not (el-job-is-busy 'org-mem)))
    (setq org-mem--time-at-begin-full-scan (current-time))
    (let ((result (el-job-launch
                   :id 'org-mem
                   :if-busy 'takeover
                   :inject-vars (org-mem--mk-work-vars)
                   :load-features '(org-mem-parser)
                   :inputs #'org-mem--list-files-from-fs
                   :funcall-per-input #'org-mem-parser--parse-file
                   :callback #'org-mem--finalize-full-scan)))
      (when (eq result 'inputs-were-empty)
        (if org-mem-do-sync-with-org-id
            (message "No org-ids found.  If you know they exist, try M-x %S."
                     (if (fboundp 'org-roam-update-org-id-locations)
                         'org-roam-update-org-id-locations
                       'org-id-update-id-locations))
          (message "No files found under `org-mem-watch-dirs'"))))))

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
      (dolist (entry entries)
        (org-mem--record-entry entry))
      (dolist (link links)
        (org-mem--record-link link)))
    (setq org-mem--time-elapsed
          (float-time (time-since org-mem--time-at-begin-full-scan)))
    (when org-mem--next-message
      (setq org-mem--next-message
            (format
             "Org-mem saw %d files, %d headings, %d links (%d IDs, %d ID-links) in %.2fs"
             (length (org-mem-all-files))
             (seq-count #'org-mem-entry-subtree-p (org-mem-all-entries))
             (length (org-mem-all-links))
             (hash-table-count org-mem--id<>entry)
             (length (org-mem-all-id-links))
             org-mem--time-elapsed)))
    (run-hook-with-args 'org-mem-post-full-scan-functions parse-results)
    (message "%s" org-mem--next-message)
    (setq org-mem--next-message nil)
    (run-hooks 'org-mem-initial-scan-hook)
    (if bad-paths
        ;; Scan again to catch symlink targets, but guard against repeating.
        (unless (seq-intersection bad-paths org-mem--caused-retry)
          (setq org-mem--caused-retry (append bad-paths org-mem--caused-retry))
          (org-mem--scan-full))
      (setq org-mem-initial-scan-hook nil))
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
  (let ((id    (org-mem-entry-id entry))
        (truename  (org-mem-entry-file-truename entry))
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

(defun org-mem--mk-work-vars ()
  "Make alist of variables needed by `org-mem-parser--parse-file'."
  (let ((custom-plain-re (org-mem--mk-plain-re org-mem-seek-link-types))
        (org-link-bracket-re ;; Mmm, copy-pasta.
         (rx "[["
	     (group (one-or-more (or (not (any "[]\\"))
		                     (and "\\" (zero-or-more "\\\\") (any "[]"))
		                     (and (one-or-more "\\") (not (any "[]"))))))
	     "]"
	     (opt "[" (group (+? anything)) "]")
	     "]")))
    (list
     (cons '$bracket-re org-link-bracket-re)
     (cons '$plain-re custom-plain-re)
     (cons '$merged-re (concat org-link-bracket-re "\\|" custom-plain-re))
     (cons '$do-cache-text org-mem-do-cache-text)
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

;; TODO: PR? It's useful.
;; Modified from part of `org-link-make-regexps'
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

(defvar org-mem--wild-filename<>truename (make-hash-table :test 'equal)
  "1:1 table mapping a wild file name to its abbreviated truename.
See helper `org-mem--truename-maybe'.")

(defvar org-mem--first-run t
  "Hack preventing the use of `file-truename' at org-mem init.
Results are often correct anyway, and file-names found to be bad will be
fixed by an automatic re-scan.

As `file-truename' can be quite slow in some environments, it would be a
bad idea to execute it for every individual file on the first scan.")

;; A design requirement is don't touch Tramp files -- neither analyze them,
;; nor scrub them from org-id-locations or the like.
;; Having this function return nil is one way to do that.
(defun org-mem--truename-maybe (wild-file &optional expand-on-first-run)
  "For non-Tramp WILD-FILE that exists, return its truename, else nil.
Caches any non-nil result, so can return a name that is no longer true.
However, it should usually correspond to a known org-mem object.

This function provides a reliable way to find org-mem objects by file
name.  The buffer-local variable `buffer-file-truename' actually gives
you an abbreviated truename, and \"unabbreviating\" it thru
`expand-file-name' is not reliable on account of e.g. buffer-env
changing the meaning of \"~\" or \"~USER\", or runtime changes to
`directory-abbrev-alist'.

Optional boolean EXPAND-ON-FIRST-RUN comes into play only when
`org-mem--first-run' is still t, and means to use `expand-file-name'
with a nil `file-name-handler-alist', rather than accept WILD-FILE
as-is.  This is a relatively performant way to convert most
file-names to probable true names."
  (or (gethash wild-file org-mem--wild-filename<>truename)
      (and (stringp wild-file)
           (not (org-mem--tramp-file-p wild-file))
           (if (file-exists-p wild-file)
               (let ((truename
                      (if org-mem--first-run
                          (if expand-on-first-run
                              (let (file-name-handler-alist)
                                ;; Names in org-id-locations are absolute,
                                ;; else this would be risky.
                                (expand-file-name wild-file))
                            wild-file)
                        (file-truename wild-file))))
                 (puthash wild-file truename org-mem--wild-filename<>truename))
             (remhash wild-file org-mem--wild-filename<>truename)))))

(declare-function tramp-tramp-file-p "tramp")
(declare-function org-mem--tramp-file-p "org-mem")
(let (loaded)
  (defun org-mem--tramp-file-p (file)
    "Pass FILE to `tramp-tramp-file-p' if Tramp loaded, else return nil.

The reasoning is that if the user has not done something in this session
to cause Tramp to load, the input FILE is unlikely to be a Tramp path.
If nevertheless it is, org-mem may have problems, but these problems
should go away after Tramp does load and `org-mem-reset' runs again."
    (and (or loaded
             (and (featurep 'tramp)
                  (prog1 (setq loaded t)
                    (clrhash org-mem--wild-filename<>truename))))
         (tramp-tramp-file-p file))))

(defun org-mem--invalidate-file-names (bad)
  "Scrub bad file names BAD in the tables that can pollute a reset.
Notably, invalidate part of the cache used by `org-mem--truename-maybe'.
If `org-mem-do-sync-with-org-id' t, also scrub `org-id-locations'."
  (dolist (bad bad)
    (remhash bad org-mem--wild-filename<>truename))
  ;; Example situation: File WILD is a symlink that changed destination.
  ;; So cached TRUE led to a nonexistent file in the last scan.
  ;; Now invalidate it so we cache a correct TRUE next time.
  (maphash (lambda (wild true)
             (when (member true bad)
               (push wild bad)
               (remhash wild org-mem--wild-filename<>truename)))
           org-mem--wild-filename<>truename)
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

;; (benchmark-call #'org-mem--list-files-from-fs)  => 0.026 s
;; (benchmark-call #'org-roam-list-files)          => 4.141 s
(defvar org-mem--dedup-tbl (make-hash-table :test 'equal))
(defun org-mem--list-files-from-fs ()
  "Look for Org files in `org-mem-watch-dirs'.

If user option `org-mem-do-sync-with-org-id' is t,
include files from `org-id-locations' in the result.

Return the file truenames only, like `org-mem--truename-maybe'.
This means you cannot cross-correlate the results with file names in
`org-id-locations', even though that was a discovery source."
  (unless (or org-mem-watch-dirs org-mem-do-sync-with-org-id)
    (error "At least one setting must be non-nil: `org-mem-watch-dirs' or `org-mem-do-sync-with-org-id'"))
  (clrhash org-mem--dedup-tbl)
  (let ((file-name-handler-alist nil)) ;; perf
    (dolist (dir (delete-dups (mapcar #'file-truename org-mem-watch-dirs)))
      ;; NOTE: It is possible to have a true dir name /home/org/,
      ;; then a symlink subdir /home/org/current/ -> /home/org/2025/.
      ;; And while `org-mem-parser--parse-file' does check `file-symlink-p' on
      ;; individual files, that does not catch the subdir being a symlink.
      ;; Fortunately, `org-mem--dir-files-recursive' will not enter
      ;; /home/org/current/ in the first place.
      (dolist (file (nconc (org-mem--dir-files-recursive
                            dir ".org_archive" org-mem-exclude)
                           (org-mem--dir-files-recursive
                            dir ".org" org-mem-exclude)))
        (puthash (org-mem--truename-maybe file) t org-mem--dedup-tbl)))
    ;; Maybe check org-id-locations.
    (when org-mem-do-sync-with-org-id
      (when (and (null org-mem-watch-dirs)
                 (not (featurep 'org))
                 (y-or-n-p "Option org-mem-watch-dirs unconfigured, load Org to find org-id-locations?"))
        (require 'org))
      ;; I wish for Christmas: a better org-id API...
      ;; Must be why org-roam decided to wrap around rather than fight it.
      (when (featurep 'org)
        (require 'org-id)
        (unless org-id-track-globally
          (error "If `org-mem-do-sync-with-org-id' is t, `org-id-track-globally' must also be t"))
        (when (and org-id-locations-file (null org-id-locations))
          (org-id-locations-load))
        (dolist (file (if (symbolp org-id-extra-files)
                          (symbol-value org-id-extra-files)
                        org-id-extra-files))
          (when (cl-notany (##string-search % file) org-mem-exclude)
            (puthash (org-mem--truename-maybe file t) t org-mem--dedup-tbl)))
        (when (org-mem--try-ensure-org-id-table-p)
          (cl-loop
           for file being each hash-value of org-id-locations do
           (when (cl-notany (##string-search % file) org-mem-exclude)
             (puthash (org-mem--truename-maybe file t) t org-mem--dedup-tbl)))))))
  (setq org-mem--first-run nil)
  (remhash nil org-mem--dedup-tbl)
  (hash-table-keys org-mem--dedup-tbl))

;; REVIEW: We can get rid of this.  In past benchmarks, it only seemed 3-5x
;; faster than `directory-files-recursively' (for our use case, with EXCLUDES etc).
;; In real numbers on my machine, that's adding 0.05s to one `org-mem-reset'
;; which takes about 1.90s total.
;; It was cooler back in org-node before 2.2.0 when the total was only ~0.90s
;; due to collecting less data.
;; Starting to see why it is that all software gets slower as it gets more
;; sophisticated!  Not only due to the sophistication, but other optimizations
;; look relatively less worth the LoC burden.
(defun org-mem--dir-files-recursive (dir suffix excludes)
  "Faster, purpose-made variant of `directory-files-recursively'.
Return a list of all files under directory DIR, its
sub-directories, sub-sub-directories and so on, with provisos:

- Don\\='t enter directories that are symlinks.
- Don\\='t enter directories whose name start with dot or underscore.
- Don\\='t enter directories where some substring of the full name
  matches one of strings EXCLUDES literally.
- Don\\='t collect any file where some substring of the non-directory
  name matches one of strings EXCLUDES literally.
- Collect only files that end in SUFFIX literally.
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
        		                  file suffix excludes)))))
        (when (string-suffix-p suffix file)
          (unless (cl-loop for substr in excludes
                           thereis (string-search substr file))
            (push (file-name-concat dir file) result)))))
    result))


;;; Assorted tools for downstream packages

(defun org-mem-await (who n-secs)
  "Wait for up to N-SECS for any current org-mem subprocesses to finish.
Return t on finish, or nil if N-SECS elapsed without finishing.
If there was no running process, return t.

Symbol WHO is included in the echo area message during the wait, to help
inform the user why Emacs hangs.  If in doubt, pass your package name."
  (cl-assert (symbolp who))
  (el-job-await 'org-mem n-secs (format "%s waiting for org-mem..." who)))

(defun org-mem-delete (pred tbl)
  "Delete rows in hash table TBL that satisfy PRED\(KEY VALUE)."
  (declare (obsolete nil "2025-05-25"))
  (message "`org-mem-delete' will be removed, use `ht-reject!'")
  (maphash (##if (funcall pred %1 %2) (remhash %1 tbl)) tbl) nil)

;; REVIEW: Mixed feelings about including this tool, but it's the obvious tool
;; to use with `org-mem-entry-text' to generate backlink previews, for
;; example, and it is apparently rare to realize the perf impact of
;; opening many Org buffers.
;; Maybe if Org doesn't fix or can't fix the startup perf, they can ship an
;; "org-scratch" function like this?
(defun org-mem-org-mode-scratch (&optional bufname)
  "Get or create a hidden `org-mode' buffer.
Also enable `org-mode', but ignore `org-mode-hook' and startup options.

Like a temp buffer, but does not clean up.  You should probably use
`erase-buffer' in case it already contains text.
BUFNAME defaults to \" *org-mem-org-mode-scratch*\"."
  (require 'org)
  (let ((bufname (or bufname " *org-mem-org-mode-scratch*"))
        (org-inhibit-startup t)
        (org-agenda-files nil)
        (org-element-cache-persistent nil))
    (or (get-buffer bufname)
        (with-current-buffer (get-buffer-create bufname t)
          (delay-mode-hooks (org-mode))
          (setq-local org-element-cache-persistent nil)
          (current-buffer)))))

(defun org-mem-fontify-like-org (string)
  "Return STRING with text properties from fontifying it in `org-mode'."
  (with-current-buffer (org-mem-org-mode-scratch)
    (erase-buffer)
    (insert string)
    (font-lock-ensure)
    (buffer-string)))


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
  (let ((files (nconc (org-mem--dir-files-recursive dir ".org_archive" nil)
                      (org-mem--dir-files-recursive dir ".org" nil))))
    (when files
      (setq files (nconc files (mapcar #'org-mem--truename-maybe files)))
      (message "Forgetting all IDs in directory %s..." dir)
      (redisplay t)
      (maphash (lambda (id file)
                 (when (member file files)
                   (remhash id org-mem--id<>entry)
                   (remhash id org-id-locations)))
               org-id-locations)
      ;; Bonus
      (dolist (file files)
        (dolist (entry (gethash file org-mem--truename<>entries))
          (remhash (org-mem-entry--internal-id entry)
                   org-mem--internal-entry-id<>links))
        (remhash file org-mem--truename<>entries)
        (remhash file org-mem--truename<>metadata))
      (org-id-locations-save)
      (message "Forgetting all IDs in directory %s...done" dir)
      (org-mem--scan-full))))


(defvar org-mem--bump-int 5 "Not a version number, but bumped sometimes.")
(define-obsolete-function-alias 'org-mem-link-dest           #'org-mem-link-target       "0.8.0 (2025-05-15)")
(define-obsolete-function-alias 'org-mem-dest                #'org-mem-target            "0.8.0 (2025-05-15)")
(define-obsolete-function-alias 'org-mem-x-fontify-like-org  #'org-mem-fontify-like-org  "0.10.0 (2025-05-18)")
(define-obsolete-function-alias 'org-mem-block               #'org-mem-await             "0.12.0 (2025-05-22)")
(define-obsolete-function-alias 'org-mem--abbr-truename      #'org-mem--truename-maybe   "0.12.0 (2025-05-22)")
(define-obsolete-function-alias 'org-mem-entry-olpath-with-title-with-self #'org-mem-entry-olpath-with-file-title-with-self  "0.13.1 (2025-05-28)")
(define-obsolete-function-alias 'org-mem-entry-olpath-with-self-with-title #'org-mem-entry-olpath-with-self-with-file-title  "0.13.1 (2025-05-28)")
(define-obsolete-function-alias 'org-mem-entry-olpath-with-title           #'org-mem-entry-olpath-with-file-title            "0.13.1 (2025-05-28)")
(define-obsolete-function-alias 'org-mem-olpath-with-title                 #'org-mem-olpath-with-file-title                  "0.13.1 (2025-05-28)")
(define-obsolete-function-alias 'org-mem-olpath-with-title-with-self       #'org-mem-olpath-with-file-title-with-self        "0.13.1 (2025-05-28)")
(define-obsolete-function-alias 'org-mem-olpath-with-self-with-title       #'org-mem-olpath-with-self-with-file-title        "0.13.1 (2025-05-28)")
(define-obsolete-function-alias 'org-mem-olpath-with-self-with-title       #'org-mem-olpath-with-self-with-file-title        "0.13.1 (2025-05-28)")
(define-obsolete-function-alias 'org-mem-entries-with-active-timestamps    #'org-mem-all-entries-with-active-timestamps "0.14.0 (2025-05-30)")
(define-obsolete-function-alias 'org-mem-files-with-active-timestamps      #'org-mem-all-files-with-active-timestamps   "0.14.0 (2025-05-30)")

(provide 'org-mem)

;;; org-mem.el ends here
