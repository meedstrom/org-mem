;;; indexed.el --- Cache metadata on all Org files -*- lexical-binding: t; -*-

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
;; URL:      https://github.com/meedstrom/org-node
;; Created:  2025-03-15
;; Keywords: text
;; Package-Version: 0.4.1
;; Package-Requires: ((emacs "29.1") (el-job "2.2.0") (llama "0.5.0") (emacsql "4.2.0"))

;;; Commentary:

;; An efficient cache of metadata about all your Org files.

;; Builds fast.

;; Provides two APIs:
;;  - regular elisp accessors such as `indexed-olpath', `indexed-pos' etc
;;  - an in-memory SQLite database that mimics the org-roam database

;;; Code:

;; TODO: Awareness of CUSTOM_ID, not just ID

;; TODO: Collect links even if there is no nearby-id (aka "origin"),
;;       bc file + pos can still locate it.

;; TODO: Collect internal Org [[links]] without type: prefix?  Would have to
;;       distinguish them from citations, which currently masquerade as link
;;       objects with TYPE nil.

;; TODO: Replace olpath in the struct with a list CRUMBS with more info,
;;       then calc olpath like
;; (defun indexed-olpath (entry)
;;   (mapcar #'caddr (indexed-crumbs entry)))

;; TODO: Collect clocks so you can check for dangling clocks at init
;;       without slowing down init

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'indexed-org-parser)
(require 'el-job)

(defvar org-id-locations)
(declare-function org-id-alist-to-hash "org-id")
(declare-function indexed-x-ensure-link-at-point-known "indexed-x")
(declare-function indexed-x--handle-save "indexed-x")
(declare-function indexed-x--handle-rename "indexed-x")
(declare-function indexed-x--handle-delete "indexed-x")

(define-obsolete-variable-alias 'indexed-pre-reset-functions 'indexed-pre-full-reset-functions "2025-03-18")
(define-obsolete-variable-alias 'indexed-post-reset-functions 'indexed-post-full-reset-functions "2025-03-18")
(define-obsolete-variable-alias 'indexed-x-pre-update-functions 'indexed-pre-incremental-update-functions "2025-03-18")
(define-obsolete-variable-alias 'indexed-x-post-update-functions 'indexed-post-incremental-update-functions "2025-03-18")
(define-obsolete-variable-alias 'indexed-x-forget-file-functions 'indexed-forget-file-functions "2025-03-18")
(define-obsolete-variable-alias 'indexed-x-forget-entry-functions 'indexed-forget-entry-functions "2025-03-18")
(define-obsolete-variable-alias 'indexed-x-forget-link-functions 'indexed-forget-link-functions "2025-03-18")

(defgroup indexed nil "Cache metadata on all Org files."
  :group 'text)

(defcustom indexed-warn-title-collisions t
  "Whether to print message when two org-ID nodes have the same title."
  :type 'boolean
  :package-version '(indexed . "0.2.0"))

(defcustom indexed-seek-link-types
  '("http" "https" "id")
  "Performance knob.
Users of org-ref would extend this to ~70 types, which may double
or triple the time it takes to run `indexed-reset'."
  :type '(repeat string)
  :package-version '(indexed . "0.2.0"))

(defcustom indexed-org-dirs '("~/org/")
  "List of directories to index.
Each directory is checked recursively \(looking in subdirectories,
sub-subdirectories etc) for files that end in \".org\".

Exceptions:

- Subdirectories starting with underscore or dot, such as \".emacs.d/\".
  To check these, add them explicitly.
- Subdirectories that are symlinks.
- Anything matching `indexed-org-dirs-exclude'.

See also `indexed-check-org-id-locations'."
  :type '(repeat directory)
  :package-version '(indexed . "0.2.0"))

(defcustom indexed-org-dirs-exclude
  '("/logseq/bak/"
    "/logseq/version-files/"
    "/node_modules/"
    ".sync-conflict-"
    ".#"
    "/backup")
  "Path substrings of files that should not be indexed.

It is not necessary to add rules here covering autosaves that end in ~
or # or .bak, since the workhorse `indexed--relist-org-files' only
considers files that end in precisely \".org\" anyway.

You can eke out a performance boost by excluding directories with a
humongous amount of files, such as the infamous \"node_modules\", even
if they contain no Org files.  However, directories that start with a
period or underscore are always ignored, so no need to add rules for
e.g. \"~/.local/\", \".git/\" or \"_site\" for that reason."
  :type '(repeat string)
  :package-version '(indexed . "0.2.0"))


;;; Lisp API

(defvar indexed--title<>id     (make-hash-table :test 'equal))
(defvar indexed--id<>entry     (make-hash-table :test 'equal))
(defvar indexed--id<>file      (make-hash-table :test 'equal)) ; Literally `org-id-locations'.
(defvar indexed--file<>data    (make-hash-table :test 'equal))
(defvar indexed--file<>entries (make-hash-table :test 'equal))
(defvar indexed--dest<>links   (make-hash-table :test 'equal))
(defvar indexed--origin<>links (make-hash-table :test 'equal))

(cl-defstruct (indexed-file-data (:constructor nil)
                                 (:copier nil))
  (file-name  () :read-only t :type string)
  (file-title () :read-only t :type string)
  (max-lines  () :read-only t :type integer)
  (mtime      () :read-only t :type integer)
  (ptmax      () :read-only t :type integer)
  (toplvl-id  () :read-only t :type string))

(cl-defstruct (indexed-org-link (:constructor indexed-org-link--make-obj)
                                (:copier nil))
  (dest      () :read-only t :type string)
  (file-name () :read-only t :type string)
  (nearby-id () :read-only t :type string)
  (pos       () :read-only t :type integer)
  (type      () :read-only t :type string))

(cl-defstruct (indexed-org-entry (:constructor indexed-org-entry--make-obj)
                                 (:copier nil))
  (closed         () :read-only t :type string)
  (deadline       () :read-only t :type string)
  (file-name      () :read-only t :type string)
  (heading-lvl    () :read-only t :type integer)
  (id             () :read-only t :type string)
  (lnum           () :read-only t :type integer)
  (olpath         () :read-only t :type list)
  (pos            () :read-only t :type integer)
  (priority       () :read-only t :type string)
  (properties     () :read-only t :type list)
  (scheduled      () :read-only t :type string)
  (tags-inherited () :read-only t :type list)
  (tags-local     () :read-only t :type list)
  (title          () :read-only t :type string)
  (todo-state     () :read-only t :type string))

(cl-defgeneric indexed-pos (entry/link)
  "Char position of ENTRY/LINK."
  (:method ((x indexed-org-entry)) (indexed-org-entry-pos x))
  (:method ((x indexed-org-link)) (indexed-org-link-pos x)))

(cl-defgeneric indexed-file-name (thing)
  "Name of file where THING is, or of file that identifies THING."
  (:method ((x indexed-org-link)) (indexed-org-link-file-name x))
  (:method ((x indexed-org-entry)) (indexed-org-entry-file-name x))
  (:method ((x indexed-file-data)) (indexed-file-data-file-name x)))

(cl-defgeneric indexed-file-data (thing)
  "Return file-data object for wherever THING is.
If THING is a file name, return the object for that file name."
  (:method ((x string))
           (or (gethash x indexed--file<>data)
               (error "File not indexed: %s" x)))
  (:method ((x indexed-org-entry))
           (or (gethash (indexed-org-entry-file-name x) indexed--file<>data)
               (error "File not indexed: %s" (indexed-org-entry-file-name x))))
  (:method ((x indexed-org-link))
           (or (gethash (indexed-org-link-file-name x) indexed--file<>data)
               (error "File not indexed: %s" (indexed-org-link-file-name x))))
  (:method ((x indexed-file-data)) x))

;; Short...
;; Could become generics if supporting other file types than Org.
(defalias 'indexed-title          #'indexed-org-entry-title)
(defalias 'indexed-olpath         #'indexed-org-entry-olpath)
(defalias 'indexed-deadline       #'indexed-org-entry-deadline)
(defalias 'indexed-heading-lvl    #'indexed-org-entry-heading-lvl)
(defalias 'indexed-priority       #'indexed-org-entry-priority)
(defalias 'indexed-properties     #'indexed-org-entry-properties)
(defalias 'indexed-lnum           #'indexed-org-entry-lnum)
(defalias 'indexed-id             #'indexed-org-entry-id)
(defalias 'indexed-scheduled      #'indexed-org-entry-scheduled)
(defalias 'indexed-tags-inherited #'indexed-org-entry-tags-inherited)
(defalias 'indexed-tags-local     #'indexed-org-entry-tags-local)
(defalias 'indexed-todo-state     #'indexed-org-entry-todo-state)
(defalias 'indexed-dest           #'indexed-org-link-dest)
(defalias 'indexed-origin         #'indexed-org-link-nearby-id) ;;XXX see readme
(defalias 'indexed-nearby-id      #'indexed-org-link-nearby-id)
(defalias 'indexed-type           #'indexed-org-link-type)

(defun indexed-entry-by-id (id)
  "The entry with ID."
  (gethash id indexed--id<>entry))

(defun indexed-entry-near-lnum-in-file (lnum file)
  "The entry around line-number LNUM in FILE."
  (cl-loop
   with last
   for entry in (gethash file indexed--file<>entries)
   if (<= lnum (indexed-lnum entry)) return (or last entry)
   else do (setq last entry)))

(defun indexed-entry-near-pos-in-file (pos file)
  "The entry around char-position POS in FILE."
  (cl-loop
   with last
   for entry in (gethash file indexed--file<>entries)
   if (<= pos (indexed-pos entry)) return (or last entry)
   else do (setq last entry)))

(defun indexed-entries-in (files)
  "All entries in FILES."
  (cl-loop for file in (ensure-list files)
           append (gethash file indexed--file<>entries)))

(defalias 'indexed-mtime
  (defun indexed-file-mtime (thing)
    (indexed-file-data-mtime (indexed-file-data thing))))

(defun indexed-file-title (thing)
  "From file where THING is, return value of #+title."
  (indexed-file-data-file-title (indexed-file-data thing)))

(defun indexed-file-title-or-basename (thing)
  "The #+title, fall back on file basename, where THING is."
  (or (indexed-file-title thing)
      (file-name-nondirectory (indexed-file-name thing))))

(defun indexed-heading-above (link)
  "Heading of entry where LINK is."
  (let ((entry (indexed-entry-near-pos-in-file (indexed-file-name link)
                                               (indexed-pos link))))
    (unless (= 0 (indexed-heading-lvl entry))
      (indexed-title entry))))

(defun indexed-id-nodes-in (files)
  "All ID-nodes in FILES."
  (setq files (ensure-list files))
  (cl-loop for entry being each hash-value of indexed--id<>entry
           when (member (indexed-file-name entry) files)
           collect entry))

(defun indexed-links-from (id)
  "All links found under the entry with ID."
  (gethash id indexed--origin<>links))

(defun indexed-id-by-title (title)
  "The ID that currently corresponds to TITLE."
  (gethash title indexed--title<>id))

(defun indexed-id-links-to (entry)
  "All ID-links that point to ENTRY."
  (gethash (indexed-id entry) indexed--dest<>links))

(defun indexed-id-node-by-title (title)
  "Among entries that have ID, find the one titled TITLE."
  (gethash (gethash title indexed--title<>id)
           indexed--id<>entry))

(defun indexed-olpath-with-self (entry)
  "Outline path, including ENTRY\\='s own heading."
  (declare (pure t) (side-effect-free t))
  (append (indexed-olpath entry)
          (list (indexed-title entry))))

(defalias 'indexed-olpath-with-title-with-self
  (defun indexed-olpath-with-self-with-title (entry &optional filename-fallback)
    "Outline path, including file #+title, and ENTRY\\='s own heading.
With FILENAME-FALLBACK, use file basename if there is no #+title."
    (declare (pure t) (side-effect-free t))
    (append (indexed-olpath-with-title entry filename-fallback)
            (list (indexed-title entry)))))

(defun indexed-olpath-with-title (entry &optional filename-fallback)
  "Outline path to ENTRY, including file #+title.
With FILENAME-FALLBACK, use file basename if there is no #+title."
  (if (/= 0 (indexed-heading-lvl entry))
      (let ((top (if filename-fallback
                     (indexed-file-title-or-basename entry)
                   (indexed-file-title entry))))
        (if top
            (cons top (indexed-olpath entry))
          (indexed-olpath entry)))
    nil))

(defun indexed-org-entries ()
  "All entries."
  (apply #'append (hash-table-values indexed--file<>entries)))

(defun indexed-org-files ()
  "All Org files that have been indexed."
  (hash-table-keys indexed--file<>data))

(defun indexed-org-id-nodes ()
  "All org-ID nodes.
An org-ID node is an entry with an ID."
  (hash-table-values indexed--id<>entry))

(defun indexed-org-links ()
  "All links."
  (cl-loop for links being each hash-value of indexed--dest<>links
           nconc (cl-loop for link in links
                          unless (null (indexed-org-link-type link))
                          collect link)))

(defun indexed-org-links-and-citations ()
  "All links and citations.
Citations are `indexed-org-link' objects where TYPE is nil and
the string DEST begins with \"@\".
(2025-03-18: This may change in the future!)"
  (hash-table-values indexed--dest<>links))

;; PROP used to be a keyword, so you could write (indexed-property :ID entry),
;; but decided to make `indexed-properties' consistent with
;; `org-entry-properties', so now it's (indexed-property "ID" entry).
(defun indexed-property (prop entry)
  "Value of property PROP in ENTRY."
  ;; (declare (pure t) (side-effect-free t)) ;; uncomment after deprec
  (if (stringp prop)
      (cdr (assoc prop (indexed-properties entry)))
    (message "Deprecation notice: call `indexed-property' with a string %s, not keyword"
             prop)
    (cdr (assoc (substring (symbol-name prop) 1) (indexed-properties entry)))))

(defun indexed-property-assert (prop entry)
  "Value of property PROP in ENTRY, throw error if nil."
  (or (indexed-property prop entry)
      (error "No property %s in entry %s" prop entry)))

(defun indexed-root-heading-to (entry)
  "Root heading in tree that contains ENTRY."
  (declare (pure t) (side-effect-free t))
  (car (indexed-olpath entry)))

(defun indexed-tags (entry)
  "ENTRY tags, with inheritance."
  (declare (pure t) (side-effect-free t))
  (delete-dups (append (indexed-tags-local entry)
                       (indexed-tags-inherited entry))))

(defun indexed-toptitle (file)
  "File #+title or topmost heading in FILE."
  (indexed-title (car (indexed-entries-in file))))


;;; Core logic

(defvar indexed-pre-full-reset-functions nil
  "Hook passed the list of parse-results, before a reset.")

(defvar indexed-post-full-reset-functions nil
  "Hook passed the list of parse-results, after a reset.")

(defvar indexed-record-file-functions nil
  "Hook passed one `indexed-file-data' object after recording it.")

(defvar indexed-record-entry-functions nil
  "Hook passed one `indexed-org-entry' object after recording it.")

(defvar indexed-record-link-functions nil
  "Hook passed one `indexed-org-link' object after recording it.")

(defvar indexed-pre-incremental-update-functions nil
  "Hook passed the list of parse-results, before an incremental update.")

(defvar indexed-post-incremental-update-functions nil
  "Hook passed the list of parse-results, after an incremental update.")

(defvar indexed-forget-file-functions nil
    "Hook passed one `indexed-file-data' object after forgetting it.")

(defvar indexed-forget-entry-functions nil
    "Hook passed one `indexed-org-entry' object after forgetting it.")

(defvar indexed-forget-link-functions nil
    "Hook passed one `indexed-org-link' object after forgetting it.")

(defvar indexed--timer (timer-create))
(defvar indexed--problems nil)
(defvar indexed--title-collisions nil)
(defvar indexed--id-collisions nil)
(defvar indexed--time-elapsed 1.0)
;; (defvar indexed--files-to-index (make-hash-table :test 'equal))

;; This mode keeps most logic in "indexed-x" because it's not necessary, you
;; could just call `indexed-reset' every 30 seconds or something equally
;; simplistic.
;;;###autoload
(define-minor-mode indexed-updater-mode
  "Keep cache up to date."
  :global t
  (if indexed-updater-mode
      (progn
        (require 'indexed-x)
        (add-hook 'after-save-hook #'indexed-x--handle-save)
        ;; (advice-add 'rename-file :after #'indexed-x--handle-rename)
        (advice-add 'delete-file :after #'indexed-x--handle-delete)
        (advice-add 'org-insert-link :after #'indexed-x-ensure-link-at-point-known)
        (indexed--activate-timer)
        (indexed--scan-full))
    (remove-hook 'after-save-hook #'indexed-x--handle-save)
    ;; (advice-remove 'rename-file #'indexed-x--handle-rename)
    (advice-remove 'delete-file #'indexed-x--handle-delete)
    (advice-remove 'org-insert-link #'indexed-x-ensure-link-at-point-known)
    (cancel-timer indexed--timer)))

(defun indexed--activate-timer (&rest _)
  "Adjust `indexed--timer' based on duration of last indexing.
If not running, start it."
  (let ((new-delay (* 25 (1+ indexed--time-elapsed))))
    (when (or (not (member indexed--timer timer-idle-list))
              ;; Don't enter an infinite loop -- idle timers can be a footgun.
              (not (> (float-time (or (current-idle-time) 0))
                      new-delay)))
      (cancel-timer indexed--timer)
      (setq indexed--timer
            (run-with-idle-timer new-delay t #'indexed--scan-full)))))

(defvar indexed--next-message nil)
(defun indexed-reset (&optional interactive)
  "Reset cache, and if INERACTIVE, print statistics."
  (interactive "p")
  (when interactive
    (setq indexed--next-message t))
  (indexed--scan-full))

(defvar indexed--time-at-begin-full-scan nil)
(defun indexed--scan-full ()
  "Arrange a full scan."
  (unless (el-job-is-busy 'indexed)
    (setq indexed--time-at-begin-full-scan (current-time))
    (el-job-launch :id 'indexed
                   :inject-vars (indexed--mk-work-vars)
                   :load-features '(indexed-org-parser)
                   :inputs #'indexed--relist-org-files
                   :funcall-per-input #'indexed-org-parser--parse-file
                   :callback #'indexed--finalize-full)))

;; To debug, do M-x edebug-defun on `indexed-org-parser--parse-file',
;; then eval:  (indexed--debug-parse-file "~/org/some-file.org")
(defun indexed--debug-parse-file (file)
  "Run `indexed-org-parser--parse-file' on FILE.
Set some variables it expects."
  (dolist (var (indexed--mk-work-vars))
    (set (car var) (cdr var)))
  (indexed-org-parser--parse-file file))

(defun indexed--finalize-full (parse-results _job)
  "Handle PARSE-RESULTS from `indexed--scan-full'."
  (run-hook-with-args 'indexed-pre-full-reset-functions parse-results)
  (clrhash indexed--title<>id)
  (clrhash indexed--id<>entry)
  (clrhash indexed--id<>file)
  (clrhash indexed--file<>data)
  (clrhash indexed--file<>entries)
  (clrhash indexed--origin<>links)
  (clrhash indexed--dest<>links)
  (setq indexed--title-collisions nil)
  (seq-let (_missing-files file-data entries links problems) parse-results
    ;; (dolist (goner missing-files)
    ;;   (remhash goner indexed--files-to-index))
    (dolist (fdata file-data)
      (puthash (indexed-file-name fdata) fdata indexed--file<>data)
      (run-hook-with-args 'indexed-record-file-functions fdata))
    (dolist (entry entries)
      (indexed--record-entry entry)
      (run-hook-with-args 'indexed-record-entry-functions entry))
    (dolist (link links)
      (indexed--record-link link)
      (run-hook-with-args 'indexed-record-link-functions link))
    (setq indexed--time-elapsed
          (float-time (time-since indexed--time-at-begin-full-scan)))
    (when indexed--next-message
      (setq indexed--next-message
            (format
             "indexed: Analyzed %d lines in %d entries (%d with ID) in %d files in %.2fs"
             (apply #'+ (mapcar #'indexed-file-data-max-lines
                                (hash-table-values indexed--file<>data)))
             (length (indexed-org-entries))
             (length (indexed-org-id-nodes))
             (length (indexed-org-files))
             (float-time (time-since indexed--time-at-begin-full-scan)))))
    (run-hook-with-args 'indexed-post-full-reset-functions parse-results)
    (message "%s" indexed--next-message)
    (setq indexed--next-message nil)
    ;; (when indexed--id-collisions
    ;;   (message "Saw same ID twice, see M-x indexed-list-id-collisions"))
    (when (and indexed--title-collisions indexed-warn-title-collisions)
      (message "Some IDs share title, see M-x indexed-list-title-collisions"))
    (when (setq indexed--problems problems)
      (message "Indexing had problems, see M-x indexed-list-problems"))))

(defun indexed--record-link (link)
  "Add info related to LINK to various tables."
  (push link (gethash (indexed-origin link) indexed--origin<>links))
  (push link (gethash (indexed-dest link)   indexed--dest<>links)))

(defun indexed--record-entry (entry)
  "Add info related to ENTRY to various tables."
  (let ((id   (indexed-id entry))
        (file (indexed-file-name entry))
        (title (indexed-title entry)))
    (push entry (gethash file indexed--file<>entries))
    (when id
      (let ((other-id (gethash title indexed--title<>id)))
        (when (and other-id (not (string= id other-id)))
          (push (list (format-time-string "%H:%M") title id other-id)
                indexed--title-collisions)))
      (when-let* ((other-entry (gethash id indexed--id<>entry)))
        ;; user error, or bug
        (push (list (format-time-string "%H:%M")
                    id
                    (indexed-title entry)
                    (indexed-title other-entry))
              indexed--id-collisions))
      (puthash id file indexed--id<>file)
      (puthash id entry indexed--id<>entry)
      (puthash title id indexed--title<>id))))


;;; Subroutines

(defcustom indexed-check-org-id-locations t
  "Whether to also index all files in `org-id-locations'."
  :type 'boolean
  :package-version '(indexed . "0.2.0"))

;; (benchmark-call #'indexed--relist-org-files)  => 0.006 s
;; (benchmark-call #'org-roam-list-files)        => 4.141 s
(defun indexed--relist-org-files ()
  "Query filesystem for Org files under `indexed-org-dirs'.

Return abbreviated truenames, to be directly comparable with file names
in `org-id-locations' and `buffer-file-truename'.

Note though that org-id would not necessarily have truenames."
  (cl-assert indexed-org-dirs)
  (let ((file-name-handler-alist nil)
        (table (make-hash-table :test 'equal)))
    (cl-loop
     for file in (indexed--abbrev-file-names
                  (cl-loop
                   for dir in (delete-dups
                               (mapcar #'file-truename indexed-org-dirs))
                   nconc (indexed--dir-files-recursive
                          dir ".org" indexed-org-dirs-exclude)))
     do (puthash file t table))
    (when (and indexed-check-org-id-locations
               (featurep 'org-id))
      (if (or (hash-table-p org-id-locations)
              (ignore-errors
                (setq org-id-locations (org-id-alist-to-hash org-id-locations))))
          (cl-loop for file being each hash-value of org-id-locations
                   do (puthash file t table))
        (message "indexed: Could not check org-id-locations")))
    (hash-table-keys table)))

;; TODO: Make it possible to list only the files in ~/.emacs.d/ but exclude
;;       all ~/.emacs.d/*/ subdirs.
;; (progn (ignore-errors (native-compile #'indexed--dir-files-recursive)) (benchmark-run 100 (indexed--dir-files-recursive org-roam-directory "org" '("logseq/"))))
(defun indexed--dir-files-recursive (dir suffix excludes)
  "Faster, purpose-made variant of `directory-files-recursively'.
Return a list of all files under directory DIR, its
sub-directories, sub-sub-directories and so on, with provisos:

- Don\\='t follow symlinks to other directories.
- Don\\='t enter directories whose name start with dot or underscore.
- Don\\='t enter directories where some substring of the path
  matches one of strings EXCLUDES literally.
- Don\\='t collect any file where some substring of the basename
  matches one of strings EXCLUDES literally.
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
              (setq result (nconc result (indexed--dir-files-recursive
        		                  file suffix excludes)))))
        (when (string-suffix-p suffix file)
          (unless (cl-loop for substr in excludes
                           thereis (string-search substr file))
            (push (file-name-concat dir file) result)))))
    result))

;; See also `consult--fast-abbreviate-file-name'.  This is faster (2024-04-16).
(defvar indexed--userhome nil)
(defun indexed--abbrev-file-names (paths)
  "Abbreviate all file paths in PATHS.
Much faster than `abbreviate-file-name', noticeably if you would have to
call it on many file paths at once.

May in some corner-cases give different results.  For instance, it
disregards file name handlers, affecting TRAMP.

PATHS can be a single path or a list, and are presumed to be absolute.

Tip: the inexactly named buffer-local variable `buffer-file-truename'
already contains an abbreviated truename."
  (unless indexed--userhome
    (setq indexed--userhome (file-name-as-directory (expand-file-name "~"))))
  ;; Assume a case-sensitive filesystem.
  ;; REVIEW: Not sure if it fails gracefully on NTFS/FAT/HFS+/APFS.
  (let ((case-fold-search nil))
    (if (listp paths)
        (cl-loop
         for path in paths
         do (setq path (directory-abbrev-apply path))
         if (string-prefix-p indexed--userhome path)
         ;; REVIEW: Sane in single-user mode Linux?
         collect (concat "~" (substring path (1- (length indexed--userhome))))
         else collect path)
      (setq paths (directory-abbrev-apply paths))
      (if (string-prefix-p indexed--userhome paths)
          (concat "~" (substring paths (1- (length indexed--userhome))))
        paths))))

(defun indexed--mk-work-vars ()
  "Make alist of variables needed by `indexed-org-parser--parse-file'."
  (let ((org-link-bracket-re
         ;; Copy-pasta. Mmm.
         "\\[\\[\\(\\(?:[^][\\]\\|\\\\\\(?:\\\\\\\\\\)*[][]\\|\\\\+[^][]\\)+\\)]\\(?:\\[\\([^z-a]+?\\)]\\)?]")
        (reduced-plain-re (indexed--mk-plain-re indexed-seek-link-types)))
    (list
     (cons '$bracket-re org-link-bracket-re)
     (cons '$plain-re reduced-plain-re)
     (cons '$merged-re (concat org-link-bracket-re "\\|" reduced-plain-re))
     (cons '$inlinetask-min-level (bound-and-true-p org-inlinetask-min-level))
     (cons '$nonheritable-tags (bound-and-true-p org-tags-exclude-from-inheritance))
     (cons '$default-todo-re
           (let ((default (default-value 'org-todo-keywords)))
             (indexed-org-parser--make-todo-regexp
              (string-join (if (stringp (car default))
                               default
                             (apply #'append (mapcar #'cdr default)))
                           " "))))
     (cons '$structures-to-ignore
           (list "src" "comment" "example"))
     (cons '$drawers-to-ignore
           (delete-dups
            (list (or (and (boundp 'org-super-links-backlink-into-drawer)
                           (stringp org-super-links-backlink-into-drawer)
                           org-super-links-backlink-into-drawer)
                      "BACKLINKS")
                  "BACKLINKS"
                  "LOGBOOK"))))))

;; TODO: PR?
;; Copied from part of `org-link-make-regexps'
(defun indexed--mk-plain-re (link-types)
  "Build a moral equivalent to `org-link-plain-re'.
Make it target only LINK-TYPES instead of all the cars of
`org-link-parameters'."
  (let* ((non-space-bracket "[^][ \t\n()<>]")
         (parenthesis
	  `(seq (any "<([")
		(0+ (or (regex ,non-space-bracket)
			(seq (any "<([")
			     (0+ (regex ,non-space-bracket))
			     (any "])>"))))
		(any "])>"))))
    (rx-to-string
     `(seq word-start
	   (regexp ,(regexp-opt link-types t))
	   ":"
           (group
	    (1+ (or (regex ,non-space-bracket)
		    ,parenthesis))
	    (or (regexp "[^[:punct:][:space:]\n]")
                ?- ?/ ,parenthesis))))))

(define-obsolete-function-alias 'indexed-roam-explore 'indexed-list-db-contents "2025-03-18")
(define-obsolete-function-alias 'indexed-id-nodes #'indexed-org-id-nodes "2025-03-18")
(define-obsolete-function-alias 'indexed-entries #'indexed-org-entries "2025-03-18")
(define-obsolete-function-alias 'indexed-files #'indexed-org-files "2025-03-18")
(define-obsolete-function-alias 'indexed-links #'indexed-org-links "2025-03-18")
(define-obsolete-function-alias 'indexed-todo #'indexed-todo-state "2025-03-18")
(define-obsolete-function-alias 'indexed-file #'indexed-file-name "2025-03-18")
(define-obsolete-function-alias 'indexed-mode #'indexed-updater-mode "2025-03-19")
(define-obsolete-function-alias 'indexed-update-on-save-mode #'indexed-updater-mode "2025-03-19")

(provide 'indexed)

;;; indexed.el ends here
