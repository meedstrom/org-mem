;;; indexed.el --- Cache metadata on all Org files -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Martin Edström

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

;; Author:   Martin Edström <meedstrom91@gmail.com>
;; URL:      https://github.com/meedstrom/org-node
;; Created:  2025-03-15
;; Keywords: text
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (el-job "2.2.0"))

;;; Commentary:

;; An efficient cache of metadata about all your Org files.

;; Builds fast.

;; Provides two APIs:
;;  - regular elisp accessors such as `indexed-olpath', `indexed-pos' etc
;;  - an in-memory SQLite database that mimics the org-roam database

;;; Code:

;; TODO: A special-mode buffer for exploring all indexed objects,
;;       same thought as `sqlite-mode-open-file'.

;; TODO: Awareness of CUSTOM_ID, not just ID

;; TODO: Collect links even if there is no nearby-id (aka "origin"),
;;       bc file + pos can still locate it

(require 'cl-lib)
(require 'subr-x)
(require 'indexed-org-parser)
(require 'el-job)

(defgroup indexed nil "Cache metadata on all Org files."
  :group 'text)

(defcustom indexed-warn-title-collisions t
  "Whether to print message when two org-ID nodes have the same title."
  :type 'boolean)

(defcustom indexed-seek-link-types
  '("http" "https" "id")
  "Performance knob.
Users of org-ref would extend this to ~70 types."
  :type '(repeat string))

(defcustom indexed-org-dirs '("~/org/")
  "List of directories to index."
  :type '(repeat directory))

(defcustom indexed-org-dirs-exclude
  '("/logseq/bak/"
    "/logseq/version-files/"
    "/node_modules/"
    ".sync-conflict-")
  "Path substrings of files that should not be indexed.

If you have accidentally let org-id add a directory of backup files, try
\\[org-node-forget-dir].

It is not necessary to exclude backups or autosaves that end in ~ or #
or .bak, since the workhorse `indexed--relist-org-files' only considers
files that end in precisely \".org\" anyway.

You can eke out a performance boost by excluding directories with a
humongous amount of files, such as the infamous \"node_modules\", even
if they contain no Org files.  However, directories that start with a
period or underscore are always ignored, so no need to specify
e.g. \"~/.local/\" or \".git/\" for that reason."
  :type '(repeat string))



;;; Lisp API

(defvar indexed--id<>entry (make-hash-table :test 'equal))
(defvar indexed--id<>file (make-hash-table :test 'equal)) ;; Literally `org-id-locations'.
(defvar indexed--origin<>links (make-hash-table :test 'equal))
(defvar indexed--dest<>links (make-hash-table :test 'equal))
(defvar indexed--title<>id (make-hash-table :test 'equal))
(defvar indexed--file<>data (make-hash-table :test 'equal))
(defvar indexed--file<>lnum.entry (make-hash-table :test 'equal)) ;; https://github.com/BurntSushi/ripgrep/discussions/3013

;; Hypothetical data types:

;; #s(entry    LNUM  POS   FILE     ID          TITLE   t      t      t       ...)
;; #s(file     LINES PTMAX t        TOPLVL-ID   TITLE   t      t      MTIME)
;; #s(link     t     POS   FILE     NEARBY-ID   DEST   TYPE)
;; #s(citation t     POS   FILE     NEARBY-ID   DEST)

;; prolly gonna regret this
;; but I lisp for a reason
(defsubst indexed-lnum (thing) "Line-number of THING." (aref thing 0))
(defsubst indexed-pos (thing) "Char position of THING." (aref thing 1))
(defsubst indexed-file (thing) "File name where THING is." (aref thing 2))
(defsubst indexed-id (entry/file) "Org-ID of ENTRY/FILE." (aref entry/file 3))
(defsubst indexed-title (entry/file) "Title of ENTRY/FILE." (aref entry/file 4))
(defsubst indexed-olpath (entry) "Outline path to ENTRY." (aref entry 8))
(defsubst indexed-heading-lvl (entry) "Number of stars in ENTRY heading." (aref entry 9))
(defsubst indexed-tags-inherited (entry) "Tags inherited by ENTRY." (aref entry 10))
(defsubst indexed-tags-local (entry) "Tags defined on ENTRY." (aref entry 11))
(defsubst indexed-deadline (entry) "Org DEADLINE date under ENTRY." (aref entry 12))
(defsubst indexed-priority (entry) "Priority of ENTRY." (aref entry 13))
(defsubst indexed-properties (entry) "Plist of properties for ENTRY." (aref entry 14))
(defsubst indexed-scheduled (entry) "Org SCHEDULED date under ENTRY." (aref entry 15))
(defsubst indexed-todo (entry) "TODO-state of ENTRY." (aref entry 16))
(defsubst indexed-mtime (file) "FILE\\='s last-modification time, integer." (aref file 7))
(defsubst indexed-origin (link) "Nearby ID where LINK was found." (aref link 3))
(defsubst indexed-dest (link) "Destination of LINK." (aref link 4))
(defsubst indexed-type (link) "LINK type." (aref link 5))

;; The proper way to do it...

;; (cl-defstruct (indexed-file-data (:constructor nil) (:copier nil)) lines ptmax toplvl-id file mtime)
;; (cl-defstruct (indexed-org-entry (:constructor nil) (:copier nil)) lnum pos file id title crumbs heading-lvl tags-inherited tags-local deadline priority properties scheduled todo)
;; (cl-defstruct (indexed-org-link (:constructor nil) (:copier nil)) pos file nearby-id dest type)
;; (cl-defgeneric indexed-pos ()
;;   (:method ((x indexed-org-entry)) (indexed-org-entry-pos x))
;;   (:method ((x indexed-org-link)) (indexed-org-link-pos x)))
;; (cl-defgeneric indexed-lnum ()
;;   (:method ((x indexed-file-data)) (indexed-file-data-lines x))
;;   (:method ((x indexed-org-entry)) (indexed-org-entry-lnum x)))
;; (cl-defgeneric indexed-file ()
;;   (:method ((x indexed-org-link)) (indexed-org-link-file x))
;;   (:method ((x indexed-org-entry)) (indexed-org-entry-file x))
;;   (:method ((x indexed-file-data)) (indexed-file-data-file x)))
;; (cl-defgeneric indexed-id ()
;;   (:method ((x indexed-org-link)) (indexed-org-link-origin x))
;;   (:method ((x indexed-org-entry)) (indexed-org-entry-id x))
;;   (:method ((x indexed-file-data)) (indexed-file-data-toplvl-id x)))
;; ...

(defun indexed-file-data (thing)
  "Return file-data object for wherever THING is."
  (if (vectorp thing)
      (gethash (indexed-file thing) indexed--file<>data)
    (gethash thing indexed--file<>data)))

(defun indexed-tags (entry)
  "ENTRY tags, with inheritance."
  (delete-dups (append (indexed-tags-local entry)
                       (indexed-tags-inherited entry))))

(defun indexed-file-title-or-basename (thing)
  "The #+title, fall back on file basename, where THING is."
  (or (indexed-file-title thing)
      (file-name-nondirectory (indexed-file thing))))

(defun indexed-olpath-with-self (entry)
  "Outline path, including ENTRY\\='s own heading."
  (append (indexed-olpath entry)
          (list (indexed-title entry))))

(defalias 'indexed-olpath-with-title-with-self
  (defun indexed-olpath-with-self-with-title (entry &optional filename-fallback)
    "Outline path, including file #+title, and ENTRY\\='s own heading.
With FILENAME-FALLBACK, use file basename if there is no #+title."
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

(defun indexed-entries-in (files)
  "All entries in FILES."
  (setq files (ensure-list files))
  (cl-loop for file in files
           append (cdr (gethash file indexed--file<>lnum.entry))))

(defun indexed-id-nodes-in (files)
  "All ID-nodes in FILES."
  (setq files (ensure-list files))
  (cl-loop for entry being each hash-value of indexed--id<>entry
           when (member (indexed-file entry) files)
           collect entry))

;; cannot make a plain all-links-to, bc there are many kinds of org links i
;; havent even considered parsing.  will be easier for `indexed-links-from' to
;; print em anyway as no need to resolve them.  thats a todo
(defun indexed-id-links-to (entry)
  "All ID-links that point to ENTRY.
See also `indexed-roam-reflinks-to' and `indexed-links-from'."
  (gethash (indexed-id entry) indexed--dest<>links))

(defun indexed-links-from (id)
  "All links found under the entry with ID."
  (gethash id indexed--origin<>links))

(defun indexed-entry-by-id (id)
  "The entry with ID."
  (gethash id indexed--id<>entry))

(defun indexed-entry-near-lnum-in-file (lnum file)
  "The entry at line-number LNUM in FILE."
  (cl-loop
   with last
   for (entry-lnum . entry) in (gethash file indexed--file<>lnum.entry)
   if (<= lnum entry-lnum) return (or last entry)
   else do (setq last entry)))

;; TODO: Refactor the hash table to just file<>entries.
(defun indexed-entry-near-pos-in-file (pos file)
  "The entry at char-position POS in FILE."
  (cl-loop
   with last
   for (_ . entry) in (gethash file indexed--file<>lnum.entry)
   if (<= pos (indexed-pos entry)) return (or last entry)
   else do (setq last entry)))

;; TODO: when we put a list CRUMBS with more info abt outline context
;; (defun indexed-olpath (entry)
;;   (mapcar #'caddr (indexed-crumbs entry)))

(defun indexed-property (prop entry)
  "Value of property PROP in ENTRY."
  (plist-get (indexed-properties entry) prop))

(defun indexed-property-assert (prop entry)
  "Value of property PROP in ENTRY, throw error if nil."
  (or (plist-get (indexed-properties entry) prop)
      (error "No property %s in entry %s" prop entry)))

(defun indexed-file-title (thing)
  "From file where THING is, return value of #+title."
  (indexed-title (indexed-file-data thing)))

;; IDK if useful, throwing stuff at a wall here.
(defun indexed-toptitle (file)
  "File #+title or topmost heading in FILE."
  (indexed-title (car (indexed-entries-in file))))

(defun indexed-root-heading-to (entry)
  "Root heading in tree that contains ENTRY."
  (car (indexed-olpath entry)))

(defun indexed-heading-above (link)
  "Heading of entry where LINK is.
Does not require LINK to have an origin ID."
  (let ((entry (indexed-entry-near-lnum-in-file (indexed-file link)
                                                (indexed-lnum link))))
    (unless (= 0 (indexed-heading-lvl entry))
      (indexed-title entry))))

;; It is a common need to iterate over all entries. This is easier than doing
;; (cl-loop for entry being each hash-value of indexed--id<>entry ...)
;; every time.
(defun indexed-id-nodes ()
  "All org-ID nodes.
An org-ID node is an entry with an ID."
  (hash-table-values indexed--id<>entry))

(defun indexed-entries ()
  "All entries."
  (cl-loop
   for lnum.entry being each hash-value of indexed--file<>lnum.entry
   append (cl-loop for (_lnum . entry) in lnum.entry
                   collect entry)))

(defun indexed-links ()
  "All links."
  (hash-table-values indexed--dest<>links))

(defun indexed-org-files ()
  "All Org files that have been indexed."
  (hash-table-keys indexed--file<>data))


;;; Core logic

(defvar indexed--timer (timer-create))
(defvar indexed--problems nil)
(defvar indexed--collisions nil)
(defvar indexed--time-elapsed 1.0)

(defvar indexed--pre-reset-hook nil)
(defvar indexed--post-reset-functions nil)
(defvar indexed-record-file-functions nil)
(defvar indexed-record-entry-functions nil)
(defvar indexed-record-link-functions nil)

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

;;;###autoload
(define-minor-mode indexed-mode
  "Re-index every now and then."
  :global t
  (if indexed-mode
      (progn (add-hook 'indexed--post-reset-functions #'indexed--activate-timer)
             (indexed--activate-timer)
             (indexed--scan-full))
    (cancel-timer indexed--timer)))

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

;; To debug: do M-x edebug-defun on `indexed-org-parser--parse-file',
;; then eval this.
;; (indexed--debug-parse-file "~/org/some-file.org")
(defun indexed--debug-parse-file (file)
  "Run `indexed-org-parser--parse-file' on FILE in correct environment."
  (dolist (var (indexed--mk-work-vars))
    (set (car var) (cdr var)))
  (indexed-org-parser--parse-file file))

(defun indexed--finalize-full (parse-results _job)
  "Handle PARSE-RESULTS from `indexed--scan-full'."
  (run-hooks 'indexed--pre-reset-hook)
  (clrhash indexed--id<>entry)
  (clrhash indexed--file<>data)
  (clrhash indexed--origin<>links)
  (clrhash indexed--dest<>links)
  (clrhash indexed--title<>id)
  (clrhash indexed--file<>lnum.entry)
  (clrhash indexed--id<>file)
  (setq indexed--collisions nil)
  (seq-let (_missing-files file-data entries links problems) parse-results
    (dolist (fdata file-data)
      (puthash (indexed-file fdata) fdata indexed--file<>data)
      (run-hook-with-args 'indexed-record-file-functions fdata))
    (dolist (entry entries)
      (let ((id   (indexed-id entry))
            (file (indexed-file entry))
            (lnum (indexed-lnum entry))
            (title (indexed-title entry)))
        (push (cons lnum entry) (gethash file indexed--file<>lnum.entry))
        (when id
          (let ((other-id (gethash title indexed--title<>id)))
            (when (and other-id (not (string= id other-id)))
              (push (list title id other-id) indexed--collisions)))
          (when (gethash id indexed--id<>entry)
            ;; Major user error!
            (message "Same ID found twice: %s" id))
          (puthash id entry indexed--id<>entry)
          (puthash title id indexed--title<>id)))
      (run-hook-with-args 'indexed-record-entry-functions entry))
    (dolist (link links)
      (push link (gethash (indexed-origin link) indexed--origin<>links))
      (push link (gethash (indexed-dest link)   indexed--dest<>links))
      (run-hook-with-args 'indexed-record-link-functions link))
    (setq indexed--time-elapsed (float-time (time-since indexed--time-at-begin-full-scan)))
    (run-hook-with-args 'indexed--post-reset-functions parse-results)
    (when (and indexed--collisions indexed-warn-title-collisions)
      (message "Some ID nodes share title, see M-x indexed-title-collisions"))
    (when (setq indexed--problems problems)
      (message "Indexing had problems, see M-x indexed-problems"))))


;;; Subroutines

;; (benchmark-call #'indexed--relist-org-files)  => 0.009714744 s
;; (benchmark-call #'org-roam-list-files)        => 1.488666741 s
(defun indexed--relist-org-files ()
  "Query filesystem for Org files under `indexed-org-dirs'.

Return abbreviated truenames, to be directly comparable with file names
in `org-id-locations' and `buffer-file-truename'.

Note though that org-id would not necessarily have truenames."
  (cl-assert indexed-org-dirs)
  (let ((file-name-handler-alist nil))
    (indexed--abbrev-file-names
     (cl-loop for dir in (delete-dups (mapcar #'file-truename indexed-org-dirs))
              nconc (indexed--dir-files-recursive
                     dir ".org" indexed-org-dirs-exclude)))))

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

(defvar indexed--userhome nil)
(defun indexed--abbrev-file-names (paths)
  "Abbreviate all file paths in PATHS.
Much faster than `abbreviate-file-name', especially if you would have to
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
     (cons '$global-todo-re
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

;; TODO PR
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


;;; Commands

(defun indexed-problems ()
  "List problems encountered while parsing."
  (interactive)
  (if indexed--problems
      (indexed--pop-to-tabulated-list
       :buffer "*indexing problems*"
       :format [("Time" 9 t) ("Scan choked near position" 27 t) ("Issue" 0 t)]
       :reverter #'indexed-problems
       :entries
       (cl-loop
        for ( time file pos signal ) in indexed--problems collect
        (list (+ (sxhash file) pos)
              (vector
               time
               (buttonize (format "%s:%d" (file-name-nondirectory file) pos)
                          `(lambda (_button)
                             (find-file ,file)
                             (goto-char ,pos)))
               (format "%s" signal)))))
    (message "Congratulations, no problems scanning %d entries in %d files!"
             (length (indexed-entries))
             (hash-table-count indexed--file<>data))))

;; TODO: Consider if/how below commands could be moved out of core

(defun indexed-title-collisions ()
  "Pop up a buffer listing title collisions between org-ID nodes."
  (interactive)
  (if indexed--collisions
      (indexed--pop-to-tabulated-list
       :buffer "*title collisions*"
       :format [("Non-unique name" 30 t) ("ID" 37 t) ("Other ID" 0 t)]
       :reverter #'indexed-title-collisions
       :entries (cl-loop
                 for row in indexed--collisions
                 collect (seq-let (msg id1 id2) row
                           (list
                            (sxhash row)
                            (vector msg
                                    (buttonize id1 'indexed--goto-id id1)
                                    (buttonize id2 'indexed--goto-id id2))))))
    (message "Congratulations, no title collisions! (among %d ID-nodes)"
             (hash-table-count indexed--title<>id))))

(defun indexed--goto-id (id)
  "Go to ID in unsophisticated way."
  (let ((entry (indexed-entry-by-id id)))
    (find-file (indexed-file entry))
    (goto-char (indexed-pos entry))))

(defun indexed-dead-id-links ()
  "List links that lead to no known ID."
  (interactive)
  (let ((dead-links
         (cl-loop for dest being each hash-key of indexed--dest<>links
                  using (hash-values links)
                  unless (gethash dest indexed--id<>entry)
                  append (cl-loop for link in links
                                  when (equal "id" (indexed-type link))
                                  collect (cons dest link)))))
    (message "%d dead links found" (length dead-links))
    (when dead-links
      (indexed--pop-to-tabulated-list
       :buffer "*dead links*"
       :format [("Location" 40 t) ("Unknown ID reference" 40 t)]
       :reverter #'indexed-dead-id-links
       :entries
       (cl-loop
        for (dest . link) in dead-links
        as origin-id = (indexed-origin link)
        as entry = (gethash origin-id indexed--id<>entry)
        collect
        (list (sxhash link)
              (vector (buttonize (indexed-title entry)
                                 `(lambda (_)
                                    (indexed--goto-id ,origin-id)
                                    (goto-char ,(indexed-pos link)))
                                 dest))))))))

;; wrapper so i dont have to remember the boilerplate,
;; nor update in many places
(cl-defun indexed--pop-to-tabulated-list (&key buffer format entries reverter)
  "Create, populate and display a `tabulated-list-mode' buffer.

BUFFER is a buffer or buffer name where the list should be created.
FORMAT is the value to which `tabulated-list-format' should be set.
ENTRIES is the value to which `tabulated-list-entries' should be set.

Optional argument REVERTER is a function to add buffer-locally to
`tabulated-list-revert-hook'."
  (unless (and buffer format)
    (user-error
     "indexed--pop-to-tabulated-list: Mandatory arguments are buffer, format, entries"))
  (when (null entries)
    (message "No entries to tabulate"))
  (pop-to-buffer (get-buffer-create buffer))
  (tabulated-list-mode)
  (setq tabulated-list-format format)
  (tabulated-list-init-header)
  (setq tabulated-list-entries entries)
  (when reverter (add-hook 'tabulated-list-revert-hook reverter nil t))
  (tabulated-list-print t))

(provide 'indexed)

;;; indexed.el ends here
