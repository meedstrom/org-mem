;;; org-mem.el --- Fast info from a large number of Org file contents -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

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
;; Package-Version: 0.31.2
;; Package-Requires: ((emacs "29.1") (el-job "2.7.3") (llama "1.0") (truename-cache "0.1.2"))

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

(define-obsolete-variable-alias 'org-mem--bump-int 'org-mem-internal-version "2026-01-27 (after 0.26.4)")
(defconst org-mem-internal-version 38 "Not a version number, but bumped sometimes.")

(require 'cl-lib)
(require 'subr-x)
(require 'llama)
(require 'el-job)
(require 'truename-cache)
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
(declare-function org-before-first-heading-p "org")
(declare-function org-entry-beginning-position "org")
(declare-function org-entry-end-position "org")
(declare-function org-entry-get "org")
(unless (and (boundp 'el-job-internal-version)
             (>= el-job-internal-version 107))
  (display-warning 'org-mem "Update to el-job 2.7.3+ to use this version of org-mem"))

(defgroup org-mem nil "Fast info from a large amount of Org file contents."
  :group 'org)

(defcustom org-mem-do-cache-text nil
  "Whether to also cache text contents of all entries.

This makes the raw text available via accessor `org-mem-entry-text'.

May slow Emacs init if set to t before enabling `org-mem-updater-mode',
so you might consider setting it afterwards, instead.
If so, the text would only be available from the first time that
`org-mem-updater--reset-timer' fires."
  :type 'boolean
  :package-version '(org-mem . "0.9.0"))

(defcustom org-mem-do-warn-title-collisions t
  "Whether to print a message when two ID-nodes have the same title."
  :type 'boolean
  :package-version '(org-mem . "0.2.0"))

(defcustom org-mem-do-look-everywhere nil
  "Whether to infer directories from as many sources as possible.

This means that any Org file found amongst...

- `recentf-list'
- `org-agenda-text-search-extra-files'
- `org-id-locations'
- `org-id-extra-files'
- `org-agenda-files'
- currently open Org buffers

... will have its directory added to the set of directories that org-mem
will scan.  They won\\='t be scanned recursively.

If Org has not yet loaded, only `recentf-list' is used."
  :type 'boolean
  :package-version '(org-mem . "0.32.0"))

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

(defcustom org-mem-exclude
  '("/logseq/bak/"
    "/logseq/version-files/"
    "/node_modules/"
    ".sync-conflict-" ;; Match e.g. "~/org/foo.sync-conflict-234234.org"
    "/backup" ;; Match e.g. "~/backups-2024/"
    "/.#")
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
this expression \(with the \"inspector\" package\):

\(inspector-inspect
 \(seq-remove (lambda (link)
               \(member (org-mem-link-type link) org-mem-seek-link-types))
             \(org-mem-all-links)))"
  :type '(repeat string)
  :package-version '(org-mem . "0.7.0"))

(defcustom org-mem-suffixes '(".org" ".org_archive")
  "File name suffixes to consider valid for scanning the file."
  :type '(repeat string)
  :package-version '(org-mem . "0.21.0"))

(defcustom org-mem-load-features nil
  "List of features to load in child process.
These features must be discoverable on `load-path'."
  :type '(repeat symbol)
  :package-version '(org-mem . "0.21.0"))

(defcustom org-mem-inject-vars '((file-name-handler-alist . nil))
  "Alist of variable-value pairs to set in child process."
  :type '(alist :key-type symbol :value-type sexp)
  :package-version '(org-mem . "0.21.0"))

(defcustom org-mem-eval-forms nil
  "Quoted forms to eval once in child process, before starting parse."
  :type '(repeat sexp))

(defcustom org-mem-ignore-regions-regexps
  '(("^[ \t]*:ROAM_REFS:" . "$")
    ("^[ \t]*:BACKLINKS:" . "$")
    ("^[ \t]*:BACKLINKS:[ \t]*$" . "^[ \t]*:END:[ \t]*$")   ; Cf. `org-clock-drawer-end-re'
    ("^[ \t]*\\(?:CLOSED\\|DEADLINE\\|SCHEDULED\\):" . "$") ; Cf. `org-planning-line-re'
    ("^[ \t]*#\\+begin_src" . "^[ \t]*#\\+end_src")         ; Cf. `org-babel-src-block-regexp'
    ("^[ \t]*#\\+begin_example" . "^[ \t]*#\\+end_example")
    ("^[ \t]*#\\+begin_comment" . "^[ \t]*#\\+end_comment")
    ("^[ \t]*#\\+transclude:" . "$"))
  "Alist of regular expressions matching boundaries of regions to avoid.
These regions will not be scanned for links nor active timestamps."
  :type '(alist :key-type regexp :value-type regexp)
  :package-version '(org-mem . "0.23.0"))


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

(defvar org-mem--target<>old-links (make-hash-table :test 'equal)
  "Previous state of `org-mem--target<>links'.
For downstream use.")

(defvar org-mem--pseudo-id<>links (make-hash-table :test 'equal)
  "1:N table mapping entry pseudo-ID to list of `org-mem-link' records.
The list represents all links found in that entry,
but not in the children of that entry.")

(defvar org-mem--key<>subtable (make-hash-table :test 'eq)
  "Big bag of memoized values, smelling of freshly cut cabbage.")

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
                       (truename-cache-get-existed-p file/entry/link)
                     (if (org-mem-entry-p file/entry/link)
                         (org-mem-entry-file-truename file/entry/link)
                       (if (org-mem-link-p file/entry/link)
                           (org-mem-link-file-truename file/entry/link)
                         (error "org-mem: FILE/ENTRY/LINK is nil"))))))
    (gethash wild-file org-mem--truename<>metadata)))

(cl-defstruct org-mem-link
  (file-truename      ""  :read-only t :type string)
  (pos                -1  :read-only t :type integer)
  (type               nil :read-only t :type string-or-nil)
  (target             ""  :read-only t :type string)
  (description        nil :read-only t :type string-or-nil)
  (citation-p         nil :read-only t :type boolean       :documentation "Whether target is a citekey e.g. @key2 in [cite:@key1;@key2;@key3].")
  (nearby-id          nil :read-only t :type string-or-nil :documentation "Inherited ID in entry where link found.")
  (supplement         nil :read-only t :type string-or-nil)
  (entry-pseudo-id    -1  :read-only t :type integer))

(cl-defstruct org-mem-entry
  (file-truename         ""  :read-only t :type string)
  (lnum                  -1  :read-only t :type integer        :documentation "Line number at heading.")
  (pos                   -1  :read-only t :type integer        :documentation "Char position at beginning of heading.")
  (title-maybe           nil :read-only t :type string-or-nil  :documentation "See `org-mem-entry-title'.")
  (level                 -1  :read-only t :type integer        :documentation "Number of stars in heading, unaffected by `org-odd-levels-only'.")
  (id                    nil :read-only t :type string-or-nil  :documentation "Value of ID property.")
  (active-timestamps-int nil :read-only t :type list           :documentation "See `org-mem-entry-active-timestamps'.")
  (clocks-int            nil :read-only t :type list           :documentation "See `org-mem-entry-clocks'.")
  (closed-int            nil :read-only t :type integer-or-nil :documentation "See `org-mem-entry-closed'.")
  (crumbs                nil :read-only t :type list)
  (deadline-int          nil :read-only t :type integer-or-nil :documentation "See `org-mem-entry-deadline'.")
  (priority              nil :read-only t :type string-or-nil)
  (properties-inherited  nil :read-only t :type list)
  (properties-local      nil :read-only t :type list)
  (scheduled-int         nil :read-only t :type integer-or-nil :documentation "See `org-mem-entry-scheduled'.")
  (stats-cookies         nil :read-only t :type list)
  (tags-inherited        nil :read-only t :type list)
  (tags-local            nil :read-only t :type list)
  (todo-state            nil :read-only t :type string-or-nil)
  (pseudo-id             -1  :read-only t :type integer        :documentation "Generated by `org-mem-parser--mk-id'.  Not `eq'-safe, use `=', `eql' or `equal'.")
  (keywords              nil :read-only t :type list))


;;; To find objects to operate on

;; This is the kind of thing I try not to have in org-mem, because look at all
;; the special-cases needed.  This one seems too useful to leave out, though.
(defun org-mem-entry-at-point (&optional actually-file)
  "Return entry object near point in the current unmodified buffer.
Only works if the buffer file has previously been scanned by org-mem.

Optional argument ACTUALLY-FILE is for use in non-file-visiting
buffers that presumably hold a copy of some file\\='s content,
and then it should be the name of that file."
  (require 'org)
  (when (buffer-modified-p)
    (message "org-mem-entry-at-point: Results not guaranteed in a modified buffer"))
  (unless (derived-mode-p 'org-mode)
    (error "org-mem-entry-at-point: Buffer must be in org-mode"))
  (let ((file (or actually-file (buffer-file-name (buffer-base-buffer))))
        (id (org-entry-get nil "ID")))
    (or (and id (org-mem-entry-by-id id))
        (unless file
          (error "org-mem-entry-at-point: Use in a file-visiting buffer or pass ACTUALLY-FILE"))
        ;; May be better than `org-mem-entry-at-pos-in-file' if buffer modified.
        (org-mem-entry-by-pseudo-id
         (org-mem-parser--mk-id (file-attributes file)
                                (buffer-substring (if (org-before-first-heading-p)
                                                      (point-min)
                                                    (org-entry-beginning-position))
                                                  (org-entry-end-position))))
        (org-mem-entry-at-pos-in-file (point) file))))

(defun org-mem-all-ids ()
  "All org-ids known to org-mem.
If `org-mem-do-sync-with-org-id' is nil, the output may NOT overlap
perfectly with `org-id-locations'."
  (with-memoization (org-mem--table 0 'org-mem-all-ids)
    (hash-table-keys org-mem--id<>entry)))

;; Aaand this is the form most likely to match `org-roam-list-files' output...
(defun org-mem-all-files-expanded ()
  "All Org files, with tilde expansion applied."
  (with-memoization (org-mem--table 0 'org-mem-all-files-expanded)
    (mapcar #'truename-cache-get-dir-abbrev (org-mem-all-file-truenames))))

(defun org-mem-all-files ()
  "Abbreviated truenames of all Org files."
  (with-memoization (org-mem--table 0 'org-mem-all-files)
    (mapcar #'truename-cache-get-full-abbrev (org-mem-all-file-truenames))))

(defun org-mem-all-file-truenames ()
  "Truename of all Org files.
When in doubt, you should prefer `org-mem-all-files', because
`directory-abbrev-alist' exists for a reason.

Org-mem uses truenames internally, but if some public function does
not work with alternative names, that should be considered a bug."
  (with-memoization (org-mem--table 0 'org-mem--truename<>metadata)
    (hash-table-keys org-mem--truename<>metadata)))

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
  (with-memoization (org-mem--table 31 id)
    (and id (gethash id org-mem--id<>entry))))

(defun org-mem-entry-by-pseudo-id (pseudo-id)
  "The entry that has internal PSEUDO-ID.
See `org-mem-parser--mk-id'."
  (with-memoization (org-mem--table 54 pseudo-id)
    (cl-loop for entry in (org-mem-all-entries)
             when (equal pseudo-id (org-mem-entry-pseudo-id entry))
             return entry)))

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
  (with-memoization (org-mem--table 32 file)
    (cl-assert (stringp file))
    (gethash (org-mem-file-known-p file) org-mem--truename<>entries)))

(defalias 'org-mem-file-entries #'org-mem-entries-in-file)

(defun org-mem-entries-in-files (files)
  "Combined list of entries from all of FILES."
  (with-memoization (org-mem--table 33 files)
    (cl-loop for file in (delete-dups (mapcar #'org-mem-file-known-p files))
             when (stringp file)
             append (gethash file org-mem--truename<>entries))))

(defun org-mem-file-by-id (id)
  "The file that contains an :ID: property matching ID."
  (with-memoization (org-mem--table 35 file)
    (let ((entry (and id (gethash id org-mem--id<>entry))))
      (and entry (org-mem-entry-file entry)))))

(defun org-mem-id-nodes-in-files (files)
  "All ID-nodes in FILES."
  (with-memoization (org-mem--table 15 files)
    (let ((files (delete-dups
                  (seq-keep #'org-mem-file-known-p (ensure-list files)))))
      (seq-filter (##member (org-mem-entry-file-truename %) files)
                  (org-mem-all-id-nodes)))))

(defun org-mem-links-with-type-and-path (type path)
  "Links with components TYPE and PATH, see `org-link-plain-re'."
  (with-memoization (org-mem--table 23 (list type path))
    (cl-loop for link in (gethash path org-mem--target<>links)
             when (equal type (org-mem-link-type link))
             collect link)))

(defun org-mem-id-links-to-entry (entry)
  "All ID-links that point to ENTRY."
  (with-memoization (org-mem--table 34 entry)
    (let ((id (org-mem-entry-id entry)))
      (and id (org-mem-id-links-to-id id)))))

(defun org-mem-links-to-target (target)
  "All link objects with link target equal to TARGET."
  (with-memoization (org-mem--table 36 target)
    (cl-assert (stringp target))
    (gethash target org-mem--target<>links)))

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
  (with-memoization (org-mem--table 37 title)
    (and title (gethash (org-mem-id-by-title title) org-mem--id<>entry))))

(defun org-mem-id-by-title (title)
  "The ID that currently corresponds to TITLE.
TITLE is either a heading, a file title, or an alias.

Assumes unique titles.  If two IDs exist with same title, it is
undefined which ID is returned.  User can prevent this from becoming a
problem with the help of option `org-mem-do-warn-title-collisions'."
  (with-memoization (org-mem--table 38 title)
    (and title (gethash title org-mem--title<>id))))

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
  "All links found inside ENTRY, ignoring descendant entries."
  (with-memoization (org-mem--table 39 entry)
    (and entry (gethash (org-mem-entry-pseudo-id entry)
                        org-mem--pseudo-id<>links))))


;;; Entry info

(defun org-mem-entry-file (entry)
  "Abbreviated truename of file where ENTRY is.
Better than `org-mem-entry-file-truename' when users may see the name.
When in doubt, prefer this, but it should not matter what form of file
name you input to the org-mem API."
  (with-memoization (org-mem--table 40 entry)
    (truename-cache-get-full-abbrev (org-mem-entry-file-truename entry))))

(defun org-mem-entry-subtree-p (entry)
  "Non-nil if ENTRY is a subtree, nil if a \"file-level node\"."
  (not (= 0 (org-mem-entry-level entry))))

(defun org-mem-entry-text (entry)
  "Full unfontified text content of ENTRY.
Requires `org-mem-do-cache-text' t.
Excludes text of child entries."
  (with-memoization (org-mem--table 28 entry)
    (let ((content (gethash (org-mem-entry-file-truename entry)
                            org-mem--truename<>content))
          (next (org-mem-next-entry entry)))
      (and content
           (substring content
                      (- (org-mem-entry-pos entry) 1)
                      (and next (- (org-mem-entry-pos next) 1)))))))

(defun org-mem-entry-children (entry)
  "Ordered list of all descendant entries to ENTRY."
  (with-memoization (org-mem--table 55 entry)
    (cl-loop
     as next = (org-mem-next-entry entry) then (org-mem-next-entry next)
     while (and next (> (org-mem-entry-level next)
                        (org-mem-entry-level entry)))
     collect next)))

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

(defun org-mem-entry-olpath-with-file-title (entry &optional filename-fallback)
  "Outline path to ENTRY, including file #+title.
Optional argument FILENAME-FALLBACK is deprecated,
use `org-mem-entry-olpath-with-file-title-or-basename' instead."
  (when filename-fallback
    (error "Argument FILENAME-FALLBACK is deprecated"))
  (with-memoization (org-mem--table 27 entry)
    (let ((olp (mapcar #'cl-fourth (reverse (cdr (org-mem-entry-crumbs entry)))))
          file-name-handler-alist)
      ;; The car of `olp' is the potentially nil file title.
      (when (null (car olp))
        (pop olp))
      olp)))

(defun org-mem-entry-olpath-with-file-title-or-basename (entry)
  "Outline path to ENTRY, including file #+title.
Use file basename if there is no #+title.

Basename means `file-name-nondirectory', not `file-name-base'."
  (with-memoization (org-mem--table 30 entry)
    (let ((olp (mapcar #'cl-fourth (reverse (cdr (org-mem-entry-crumbs entry)))))
          file-name-handler-alist)
      ;; The car of `olp' is the potentially nil file title.
      (when (null (car olp))
        (pop olp)
        (push (file-name-nondirectory (org-mem-entry-file-truename entry))
              olp))
      olp)))

(defun org-mem-entry-olpath-with-self-with-file-title (entry &optional filename-fallback)
  "Outline path, including file #+title, and ENTRY\\='s own heading.
Optional argument FILENAME-FALLBACK is deprecated,
use `org-mem-entry-olpath-with-self-with-file-title-or-basename' instead.

If ENTRY is itself a file-level entry, the return value is still a list
of zero or one strings, not two."
  (when filename-fallback
    (error "Argument FILENAME-FALLBACK is deprecated"))
  (with-memoization (org-mem--table 24 entry)
    (let ((olp (mapcar #'cl-fourth (reverse (org-mem-entry-crumbs entry))))
          file-name-handler-alist)
      ;; The car of `olp' is the potentially nil file title.
      (when (null (car olp))
        (pop olp))
      olp)))

(defun org-mem-entry-olpath-with-self-with-file-title-or-basename (entry)
  "Outline path, including file #+title, and ENTRY\\='s own heading.
Use file basename if there is no #+title.

Basename means `file-name-nondirectory', not `file-name-base'.

If ENTRY is itself a file-level entry, the return value is still a list
of one string, not two."
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
file basename \(file name sans directory component\).

I.e. `file-name-nondirectory', not `file-name-base'.  In org-mem jargon,
\"basename\" refers to what you get from the POSIX command \"basename\"
with one argument, which should be familiar if you are not too mired in
Emacs Lisp."
  (with-memoization (org-mem--table 41 entry)
    (or (org-mem-entry-title-maybe entry)
        (progn (cl-assert (not (org-mem-entry-subtree-p entry)))
               (let (file-name-handler-alist)
                 (file-name-nondirectory (org-mem-entry-file-truename entry)))))))

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
  (with-memoization (org-mem--table 42 (list prop entry))
    (cdr (assoc (upcase prop) (org-mem-entry-properties entry)))))

(defun org-mem-entry-property-with-inheritance (prop entry)
  "Value of property PROP in ENTRY."
  ;; NOTE: A value can be nil (i.e. not string "nil" but symbol nil), but
  ;;       should still override any inherited value, so remember to use
  ;;       `assoc' correctly with that in mind.
  (with-memoization (org-mem--table 43 (list prop entry))
    (cdr (or (assoc (upcase prop) (org-mem-entry-properties entry))
             (assoc (upcase prop) (org-mem-entry-properties-inherited entry))))))

(defun org-mem-entry-tags (entry)
  "ENTRY tags, with inheritance if allowed at ENTRY.
Combines `org-mem-entry-tags-local' and `org-mem-entry-tags-inherited'."
  (with-memoization (org-mem--table 44 entry)
    (delete-dups (append (org-mem-entry-tags-inherited entry)
                         (org-mem-entry-tags-local entry)))))

(define-inline org-mem--iso8601 (int-time)
  "Translate INT-TIME into a string \"yyyy-mm-ddThh:mm\"."
  (inline-quote (format-time-string "%FT%H:%M" ,int-time)))

(defun org-mem-entry-closed (entry)
  "CLOSED-timestamp of ENTRY, suitable for `iso8601-parse'."
  (with-memoization (org-mem--table 45 entry)
    (let ((ts (org-mem-entry-closed-int entry)))
      (and ts (org-mem--iso8601 ts)))))

(defun org-mem-entry-deadline (entry)
  "DEADLINE-timestamp of ENTRY, suitable for `iso8601-parse'.
WARNING: If the timestamp is expressed as a diary-sexp,
such as <%%\(memq (calendar-day-of-week date) \\='(1 2 3 4 5)))>,
this returns nil!"
  (with-memoization (org-mem--table 46 entry)
    (let ((ts (org-mem-entry-deadline-int entry)))
      (and ts (org-mem--iso8601 ts)))))

(defun org-mem-entry-scheduled (entry)
  "SCHEDULED-timestamp of ENTRY, suitable for `iso8601-parse'.
WARNING: If the timestamp is expressed as a diary-sexp,
such as <%%\(memq (calendar-day-of-week date) \\='(1 2 3 4 5)))>,
this returns nil!"
  (with-memoization (org-mem--table 47 entry)
    (let ((ts (org-mem-entry-scheduled-int entry)))
      (and ts (org-mem--iso8601 ts)))))

(defun org-mem-entry-active-timestamps (entry)
  "Active timestamps in ENTRY, suitable for `iso8601-parse'.
Excludes such timestamps in DEADLINE or SCHEDULED, since there is
`org-mem-entry-deadline' &c for that."
  (with-memoization (org-mem--table 48 entry)
    (mapcar #'org-mem--iso8601 (org-mem-entry-active-timestamps-int entry))))

(defun org-mem-entry-clocks (entry)
  "Alist \((START END MINUTES) ...) representing clock lines in ENTRY.
START and END are ISO8601 time strings.
MINUTES is an integer amount of minutes.
To get all three as Lisp time values, use `org-mem-entry-clocks-int'.

Any dangling clock line is represented as just \(START)."
  (with-memoization (org-mem--table 49 entry)
    (cl-loop for (start end secs) in (org-mem-entry-clocks-int entry)
             if end collect (list (org-mem--iso8601 start)
                                  (org-mem--iso8601 end)
                                  (/ secs 60))
             else collect (list (org-mem--iso8601 start)))))

(defun org-mem-entry-dangling-clocks (entry)
  "List \(START1 START2 ...) representing clocks in ENTRY with no end.
See also `org-mem-all-entries-with-dangling-clock'."
  (with-memoization (org-mem--table 50 entry)
    (cl-loop for clock in (org-mem-entry-clocks-int entry)
             when (length= clock 1)
             collect (org-mem--iso8601 (car clock)))))


;;; Link info

(defun org-mem-link-file (link)
  "Abbreviated truename of file where LINK is.
See more info at `org-mem-entry-file'."
  (with-memoization (org-mem--table 51 link)
    (truename-cache-get-full-abbrev (org-mem-link-file-truename link))))

(defun org-mem-link-entry (link)
  "The entry that contains LINK."
  (with-memoization (org-mem--table 52 link)
    (org-mem-entry-at-pos-in-file (org-mem-link-pos link)
                                  (org-mem-link-file link))))

(defun org-mem-id-link-p (link)
  "Return t if LINK is an `org-mem-link' object of type \"id\"."
  (with-memoization (org-mem--table 53 link)
    (and (org-mem-link-p link)
         (equal (org-mem-link-type link) "id"))))


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
The uid and gid are integers, see ID-FORMAT in `file-attributes'."
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

(defun org-mem-file-known-p (file)
  "Return non-nil when FILE is known to org-mem.
Specifically, return the name by which it is known.
This is more restrictive than `truename-cache-get-existed-p'
by only returning non-nil if the file has been scanned by org-mem."
  (cl-assert (stringp file))
  (or (and (gethash file org-mem--truename<>entries)
           file)
      (and (setq file (truename-cache-get-existed-p file))
           (if (gethash file org-mem--truename<>entries)
               file
             (truename-cache-invalidate file)
             nil))))

(defun org-mem-file-keywords (file)
  "Alist of all #+keywords in FILE."
  (with-memoization (org-mem--table 56 file)
    (org-mem-parser--merge-same-keywords
     (seq-mapcat #'org-mem-entry-keywords (org-mem-entries-in-file file)))))


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
during the hook `org-mem--forget-entry-functions' or similar."
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
  (seq-mapcat #'org-mem-roam-reflinks-to-entry
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
    (with-current-buffer (get-buffer-create " *org-mem-scratch*" t)
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

(add-hook 'org-mem--record-entry-functions
          #'org-mem--record-roam-aliases-and-refs -10)

(add-hook 'org-mem--forget-entry-functions
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

(cl-defgeneric org-mem-pseudo-id (entry/link)
  "Pseudo-ID of ENTRY/LINK.
See `org-mem-parser--mk-id'."
  (:method ((xx org-mem-entry)) (org-mem-entry-pseudo-id xx))
  (:method ((xx org-mem-link)) (org-mem-link-entry-pseudo-id xx)))

(cl-defgeneric org-mem-id (entry/file)
  "ID property of ENTRY/FILE - if file name, the file-level ID."
  (:method ((xx org-mem-entry)) (org-mem-entry-id xx))
  (:method ((xx string)) (org-mem-file-id-strict xx)))

(cl-defgeneric org-mem-title (file/entry)
  "Heading title, or file title or basename of ENTRY/FILE.
Like `org-mem-entry-title', this always returns a string."
  (:method ((xx org-mem-entry)) (org-mem-entry-title xx))
  (:method ((xx string)) (org-mem-file-title-or-basename xx)))

(cl-defgeneric org-mem-title-maybe (entry/file)
  "Heading title, or file #+title if ENTRY/FILE is a file name."
  (:method ((xx org-mem-entry)) (org-mem-entry-title-maybe xx))
  (:method ((xx string)) (org-mem-file-title-strict xx)))

(defun org-mem-entries-in (file/files/entry)
  "All entries in FILE/FILES/ENTRY."
  (funcall (if (org-mem-entry-p file/files/entry)
               #'org-mem-entry-children
             (if (listp file/files/entry)
                 #'org-mem-entries-in-files
               #'org-mem-entries-in-file))
           file/files/entry))


;;; Core logic

(defvar org-mem-initial-scan-hook nil
  "Hook called after the first full scan.")

(defvar org-mem-pre-full-scan-functions nil
  "Hook passed the list of parse-results, before a full reset.")

(defvar org-mem-post-full-scan-functions nil
  "Hook passed the list of parse-results, after a full reset.")

(defvar org-mem--record-file-functions nil
  "Hook passed (FILE ATTRS LINES PTMAX) after adding that info to tables.")

(defvar org-mem--record-entry-functions nil
  "Hook passed one `org-mem-entry' object after adding it to tables.")

(defvar org-mem--record-link-functions nil
  "Hook passed one `org-mem-link' object after adding it to tables.")

(defvar org-mem-pre-targeted-scan-functions nil
  "Hook passed the list of parse-results, before a partial reset.
This occurs before scanning a targeted single file or set of files,
hence the name.  Contrast `org-mem-pre-full-scan-functions'.")

(defvar org-mem-post-targeted-scan-functions nil
  "Hook passed the list of parse-results, after a partial reset.
This occurs after scanning a targeted single file or set of files,
hence the name.  Contrast `org-mem-post-full-scan-functions'.")

(defvar org-mem--forget-file-functions nil
  "Hook passed a file name after removing its info from tables.")

(defvar org-mem--forget-entry-functions nil
  "Hook passed one forgotten `org-mem-entry' object.")

(defvar org-mem--problems nil)
(defvar org-mem--title-collisions nil)
(defvar org-mem--id-collisions nil)
(defvar org-mem--time-elapsed 1.0)
(defvar org-mem--next-message nil)
(defvar org-mem--time-at-begin-full-scan nil)
(defvar org-mem--resets-debug-log nil)
(defvar org-mem--reset-msg nil)

(defun org-mem-reset (&optional takeover msg called-interactively)
  "Reset cache, and then if CALLED-INTERACTIVELY, print statistics.
For arguments TAKEOVER and MSG, see `org-mem--scan-full'."
  (interactive "i\ni\np")
  (push (list (format-time-string "%FT%T.%3N") takeover msg called-interactively)
        org-mem--resets-debug-log)
  ;; Temporary because we break downstream in 123d77a
  (when (and (featurep 'org-node)
             (not (boundp 'org-node-internal-version)))
    (display-warning 'org-mem "You probably want to update org-node, not just org-mem"))
  (when called-interactively
    (truename-cache-reset)
    (setq org-mem--next-message t)
    (setq takeover t))
  (org-mem--scan-full takeover msg)
  (when called-interactively
    (org-mem-tip-if-empty))
  msg)

(defun org-mem--scan-full (&optional takeover msg)
  "Arrange a full scan, if one is not already ongoing.
With TAKEOVER t, stop any already ongoing scan to start a new one.

Argument MSG is an optional message to print.  If provided, it also
overrides a default message printed when `org-mem-do-cache-text' is t."
  (org-mem--check-user-settings)
  (when (or takeover (not (el-job-ng-busy-p 'org-mem)))
    (setq org-mem--time-at-begin-full-scan (current-time))
    (when msg
      (setq org-mem--reset-msg msg)
      (message "%s" msg)
      (redisplay t))
    (let ((files (mapcar #'car (org-mem--truenames-and-attrs))))
      (when files
        (el-job-ng-run :id 'org-mem
                       :inject-vars (append (org-mem--mk-work-vars)
                                            (el-job-ng-vars org-mem-inject-vars))
                       :require (cons 'org-mem-parser org-mem-load-features)
                       :eval org-mem-eval-forms
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
          (redisplay)
          (let ((content-cacher
                 (lambda (files)
                   (with-temp-buffer
                     (dolist (file files)
                       (erase-buffer)
                       (insert-file-contents file)
                       (puthash file (buffer-string) org-mem--truename<>content))))))
            (eval `(let ,(cl-loop for (var . val)
                                  in (el-job-ng-vars org-mem-inject-vars)
                                  if (listp val)
                                  collect `(,var ',val)
                                  else collect `(,var ,val))
                     (funcall ,content-cacher ',files))
                  t))
          (message nil)
          (redisplay))))))

(defvar org-mem--caused-retry nil)
(defun org-mem--finalize-full-scan (parse-results)
  "Handle PARSE-RESULTS from `org-mem--scan-full'."
  (run-hook-with-args 'org-mem-pre-full-scan-functions parse-results)
  (mapc #'clrhash (hash-table-values org-mem--key<>subtable))
  (clrhash org-mem--title<>id)
  (clrhash org-mem--id<>entry)
  (clrhash org-mem--truename<>metadata)
  (clrhash org-mem--truename<>entries)
  (clrhash org-mem--pseudo-id<>links)
  (setq org-mem--title-collisions nil)
  (let ((bad-paths (cl-loop for (bad) in parse-results when bad collect bad))
        problems)
    (org-mem--invalidate-file-names bad-paths)
    ;; Build tables.
    (with-current-buffer (get-buffer-create " *org-mem-scratch*" t)
      (cl-loop for (_ problem file-data entries links) in parse-results do
               (when problem (push problem problems))
               (puthash (car file-data) file-data org-mem--truename<>metadata)
               (run-hook-with-args 'org-mem--record-file-functions file-data)
               (dolist (entry entries)
                 (org-mem--record-entry entry)
                 (run-hook-with-args 'org-mem--record-entry-functions entry))
               (dolist (link links)
                 (push link (gethash (org-mem-link-entry-pseudo-id link)
                                     org-mem--pseudo-id<>links))
                 (run-hook-with-args 'org-mem--record-link-functions link))))
    (org-mem--rebuild-specially-indexed-tables)

    ;; Message on interactive reset.
    ;; Ensure the message can be edited by `org-mem-post-full-scan-functions'.
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
    (when (equal (current-message) org-mem--reset-msg)
      (if (string-suffix-p "..." org-mem--reset-msg)
          (message "%s" (concat org-mem--reset-msg " done"))
        (message nil)))

    ;; Other stuff.
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
      (puthash id entry org-mem--id<>entry)
      (when title
        (let ((other-id (gethash title org-mem--title<>id)))
          (when (and other-id (not (string= id other-id)))
            (push (list (format-time-string "%H:%M") title id other-id)
                  org-mem--title-collisions)))
        ;; REVIEW: Should we fallback on file name, i.e. use
        ;; `org-mem-entry-title' instead of `org-mem-entry-title-maybe'?
        ;; It would affect function `org-mem-id-by-title'.
        (puthash title id org-mem--title<>id)))))

(defun org-mem--rebuild-specially-indexed-tables ()
  "Rebuild table `org-mem--target<>links'.

A \"specially indexed\" table is one that cannot be trivially updated when
one file is re-scanned, since a given key in it may list values from
multiple files.  So rather than try to find the particular values to
remove, it's easiest to wipe and re-build."
  (setq org-mem--target<>old-links (copy-hash-table org-mem--target<>links))
  (clrhash org-mem--target<>links)
  (cl-loop
   for links being each hash-value of org-mem--pseudo-id<>links
   do (dolist (link links)
        (push link (gethash (org-mem-link-target link) org-mem--target<>links)))))

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
     (cons '$ignore-regions-regexps org-mem-ignore-regions-regexps))))

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

(defun org-mem--invalidate-file-names (bad-names)
  "Scrub bad file names BAD-NAMES in the truename cache.
If `org-mem-do-sync-with-org-id' t, also scrub `org-id-locations'."
  (let ((invalidated (seq-mapcat #'truename-cache-invalidate bad-names)))
    (when (and invalidated
               org-mem-do-sync-with-org-id
               (org-mem--try-ensure-org-id-table-p))
      (setq org-id-locations
            (org-id-alist-to-hash
             (cl-loop for cell in (org-id-hash-to-alist org-id-locations)
                      unless (member (car cell) invalidated)
                      collect cell))))))

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

(defun org-mem--truenames-and-attrs ()
  "Return an unsorted alist \((FILE1 . ATTR1) (FILE2 . ATTR2) ...\).
Each FILE is a truename and each ATTR an output of `file-attributes'.

These values are newly retrieved from the filesystem, so they might not
be associated with objects in existing org-mem tables.
For that, you may be looking for `org-mem-all-files'.

Affected by user options:
- `org-mem-watch-dirs'
- `org-mem-do-sync-with-org-id'
- `org-mem-do-look-everywhere'
- `org-mem-suffixes'
- `org-mem-exclude'"
  (cl-loop
   with exclude-re = (regexp-opt (cons "/_" (cons "/." org-mem-exclude)))
   with suffix-re = (rx (regexp (regexp-opt org-mem-suffixes)) eos)
   for cell
   in (truename-cache-collect-files-and-attributes
       :local-name-handlers nil
       :remote-name-handlers '(tramp-archive-file-name-handler
                               tramp-completion-file-name-handler
                               tramp-file-name-handler
                               tramp-autoload-file-name-handler)
       :keep-remotes nil
       :full-dir-deny (list exclude-re)
       :dirs-recursive org-mem-watch-dirs
       :infer-dirs-from
       (list
        (and org-mem-do-look-everywhere
             (seq-filter (##string-match-p suffix-re %)
                         (bound-and-true-p recentf-list)))
        (and org-mem-do-look-everywhere
             (featurep 'org)
             (append (org-files-list)
                     ;; Should've been a separate option
                     (if (stringp (car org-agenda-text-search-extra-files))
                         org-agenda-text-search-extra-files
                       (cdr org-agenda-text-search-extra-files))))
        (and (or org-mem-do-sync-with-org-id
                 org-mem-do-look-everywhere)
             (featurep 'org-id)
             (append (seq-filter #'stringp
                                 ;; Should've been a separate option
                                 (if (symbolp org-id-extra-files)
                                     (symbol-value org-id-extra-files)
                                   org-id-extra-files))
                     (and (org-mem--try-ensure-org-id-table-p)
                          (hash-table-values org-id-locations))))))
   when (and (string-match-p suffix-re (car cell))
             (not (string-match-p exclude-re (car cell))))
   collect cell))

(defun org-mem--check-user-settings ()
  "Signal if user options are set to illegal or inefficient values."
  (unless (or org-mem-watch-dirs
              org-mem-do-sync-with-org-id
              org-mem-do-look-everywhere)
    (user-error "At least one of these settings must be non-nil:
`org-mem-watch-dirs'
`org-mem-do-sync-with-org-id'
`org-mem-do-look-everywhere'"))
  (unless (compiled-function-p (symbol-function #'org-mem--truenames-and-attrs))
    (display-warning 'org-mem "Org-mem will be very slow unless compiled"))
  (unless (compiled-function-p (symbol-function #'truename-cache-collect-files-and-attributes))
    (display-warning 'org-mem "Org-mem will be very slow unless truename-cache is compiled"))
  (when (and org-mem-do-sync-with-org-id (featurep 'org-id) (not org-id-track-globally))
    (user-error "If org-mem-do-sync-with-org-id is t, org-id-track-globally must be t as well"))
  (when (seq-find #'file-remote-p org-mem-watch-dirs)
    (user-error "Must not contain remote directories: org-mem-watch-dirs")))


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
  (el-job-ng-await 'org-mem-updater n-secs message)
  (el-job-ng-await 'org-mem n-secs message))

;; REVIEW: Mixed feelings about including this tool, but it's the obvious tool
;; to use with `org-mem-entry-text' to generate backlink previews, for
;; example, and it is apparently rare to realize the perf impact of
;; opening many Org buffers.
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
  "If tables empty, print a helpful message."
  (when (hash-table-empty-p org-mem--truename<>metadata)
    (let ((msg (if org-mem-do-sync-with-org-id
                   (if (not (featurep 'org-id))
                       (if org-mem-watch-dirs
                           "org-mem: No files found in `org-mem-watch-dirs', and no org-ids because Org not loaded"
                         "org-mem: No org-ids found because Org not loaded")
                     (format "org-mem: No org-ids found.  If you know they exist, try M-x %S."
                             (if (fboundp 'org-roam-update-org-id-locations)
                                 'org-roam-update-org-id-locations
                               'org-id-update-id-locations)))
                 "org-mem: No files found under `org-mem-watch-dirs'")))
      (message msg)
      msg)))

(defun org-mem-translate-parse-results (ng-style-results)
  "Translate NG-STYLE-RESULTS into style from before org-mem 0.27.0.

This is a transition helper.  Org-mem 0.27.0 changed the style of
PARSE-RESULTS passed to `org-mem-pre-full-scan-functions' & co.

For reference, the new style is a list as long as the number of files,
each element being a list of 5 elements:

   \((BAD-PATH PROBLEM FILE-DATUM ENTRIES LINKS) ...)

This data represents one file.

Old style was single list of 5 lists, representing all files combined:

   \(BAD-PATHS FILE-DATA ENTRIES LINKS PROBLEMS)"
  (cl-loop
   for (badpath problem fdatum entries links) in ng-style-results
   when badpath collect badpath into badpaths
   when problem collect problem into problems
   when fdatum collect fdatum into fdata
   append entries into all-entries
   append links into all-links
   finally return (list badpaths fdata all-entries all-links problems)))



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
      (setq files (append files (seq-keep #'truename-cache-get-existed-p files)))
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
          (remhash (org-mem-entry-pseudo-id entry)
                   org-mem--pseudo-id<>links))
        (remhash file org-mem--truename<>entries)
        (remhash file org-mem--truename<>metadata))
      (setq org-id-files (cl-nset-difference org-id-files files :test #'equal))
      (org-id-locations-save)
      (message "Forgetting all IDs in directory %s...done" dir)
      (org-mem--scan-full))))


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

(defconst org-mem-watch-dirs-exclude :renamed-0.13.0)
(org-mem--def-whiny-alias 'org-mem-entry-olpath-with-file-title-with-self #'org-mem-entry-olpath-with-self-with-file-title  "0.22.0 (2025-10-01)" "2026-04-30")
(org-mem--def-whiny-alias 'org-mem-olpath-with-file-title-with-self       #'org-mem-olpath-with-self-with-file-title        "0.22.0 (2025-10-01)" "2026-04-30")
(defconst org-mem-forget-link-functions  :obsolete-0.24.0)

(defconst org-mem-forget-entry-functions :obsolete-0.29.0
  "Deprecated because dangerous.
Still exists under name `org-mem--forget-entry-functions',
but please use `org-mem-post-targeted-scan-functions'.")

(defconst org-mem-forget-file-functions  :obsolete-0.29.0
  "Deprecated because dangerous.
Still exists under name `org-mem--forget-file-functions',
but please use `org-mem-post-targeted-scan-functions'.")

(defconst org-mem-record-link-functions  :obsolete-0.29.0
  "Deprecated because dangerous.
Still exists under name `org-mem--record-link-functions',
but please use `org-mem-post-full-scan-functions'.")

(defconst org-mem-record-entry-functions :obsolete-0.29.0
  "Deprecated because dangerous.
Still exists under name `org-mem--record-entry-functions',
but please use `org-mem-post-full-scan-functions'.")

(defconst org-mem-record-file-functions  :obsolete-0.29.0
  "Deprecated because dangerous.
Still exists under name `org-mem--record-file-functions',
but please use `org-mem-post-full-scan-functions'.")

(define-obsolete-function-alias 'org-mem-entry-that-contains-link #'org-mem-link-entry "0.29.0 (2026-02-11)")

(defun org-mem--list-files-from-fs ()
  (declare (obsolete nil "2026-02-16"))
  (mapcar #'car (org-mem--truenames-and-attrs)))

(provide 'org-mem)

;;; org-mem.el ends here
