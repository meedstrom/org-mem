;;; indexed-roam.el --- Make data like org-roam does -*- lexical-binding: t; -*-

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

;;; Commentary:

;; A submodule to

;; 1. Also index ROAM_ALIASES and ROAM_REFS.
;; 2. Make a SQL database.

;; Activate `indexed-roam-mode'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sqlite)
(require 'sqlite-mode)
(require 'indexed)

;; TODO: Eliminate dependency
(require 'ol)


;;; Aliases and refs support

(defvar indexed-roam--work-buf nil)
(defvar indexed-roam--ref<>id (make-hash-table :test 'equal))
(defvar indexed-roam--id<>refs (make-hash-table :test 'equal))
(defvar indexed-roam--ref<>type (make-hash-table :test 'equal)) ;; REVIEW: weird

(defun indexed-roam-refs (entry)
  "Property ROAM_REFS in ENTRY, properly split."
  (gethash (indexed-id entry) indexed-roam--id<>refs))

(defun indexed-roam-reflinks-to (entry)
  "All links that point to a member of ENTRY\\='s ROAM_REFS."
  (cl-loop for ref in (indexed-roam-refs entry)
           append (gethash ref indexed--dest<>links)))

(defun indexed-roam-aliases (entry)
  "Property ROAM_ALIASES in ENTRY, properly split."
  (when-let* ((aliases (indexed-property :ROAM_ALIASES entry)))
    (split-string-and-unquote aliases)))

(defun indexed-roam--mk-lisp-tables (indexing-results)
  "Designed for `indexed--post-reset-functions'.
Loop over INDEXING-RESULTS to record aliases and refs."
  (clrhash indexed-roam--ref<>id)
  (clrhash indexed-roam--id<>refs)
  (with-current-buffer (setq indexed-roam--work-buf
                             (get-buffer-create " *indexed-roam*" t))
    (dolist (entry (nth 2 indexing-results))
      (indexed-roam--record-aliases entry)
      (when-let* ((id (indexed-id entry))
                  (refs (indexed-roam--split-refs-field
                         (indexed-property :ROAM_REFS entry))))
        (puthash id refs indexed-roam--id<>refs)
        (dolist (ref refs)
          (puthash ref id indexed-roam--ref<>id))))))

(defun indexed-roam--record-aliases (entry)
  "Add any ENTRY aliases to `indexed--title<>id'."
  (when-let* ((id (indexed-id entry)))
    (dolist (alias (indexed-roam-aliases entry))
      ;; Check collisions with aliases too
      (when-let* ((other-id (gethash alias indexed--title<>id)))
        (unless (string= id other-id)
          (push (list alias id other-id) indexed--collisions)))
      (puthash alias id indexed--title<>id))))

(defun indexed-roam--split-refs-field (roam-refs)
  "Split a ROAM-REFS field correctly.
What this means?  See indexed-test.el"
  (when roam-refs
    (cl-assert (eq (current-buffer) indexed-roam--work-buf))
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
          (error "Missing close-bracket in ROAM_REFS property")))
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
                 (puthash path nil indexed-roam--ref<>type)
                 (concat "@" path))
       ;; Some sort of uri://path
       else when (setq colon-pos (string-search ":" link?))
       collect (let ((path (string-replace
                            "%20" " "
                            (substring link? (1+ colon-pos)))))
                 ;; Remember the uri: prefix for pretty completions
                 (puthash path (substring link? 0 colon-pos)
                          indexed-roam--ref<>type)
                 ;; .. but the actual ref is just the //path
                 path)))))


;;; Database

(defvar indexed-roam--connection nil
  "A SQLite handle.")

(defun indexed-roam--mk-db (_)
  "Close current `indexed-roam--connection' and populate a new one."
  (interactive)
  (ignore-errors (sqlite-close indexed-roam--connection))
  (indexed-roam))

;;;###autoload
(defun indexed-roam (&optional sql &rest args)
  "Return the SQLite handle to the org-roam-like database.
Each call checks if it is alive, and renews if not.

If arguments SQL and ARGS provided, pass to `sqlite-select'."
  (cl-assert (member 'indexed-roam--mk-db indexed--post-reset-functions))
  (or (ignore-errors (sqlite-pragma indexed-roam--connection "im_still_here"))
      (setq indexed-roam--connection (indexed-roam--open-new-db)))
  (if sql
      (sqlite-select indexed-roam--connection sql args)
    indexed-roam--connection))

(defun indexed-roam--open-new-db (&optional loc)
  "Generate a new database and return a connection-handle to it.
Shape it according to org-roam schemata and pre-populate it with data.

Normally, this creates a diskless database.  With optional file path
LOC, write the database as a file to LOC."
  (let ((T (current-time))
        (name (or loc "diskless DB"))
        (db (sqlite-open loc)))
    (message "indexed-roam: Re-creating %s..." name)
    (indexed-roam--configure db)
    (indexed-roam--populate db (indexed-roam--mk-rows))
    (message "indexed-roam: Re-creating %s... done \(%.2fs\)"
             name
             (float-time (time-since T)))
    db))

(defun indexed-roam--configure (db)
  "Set up tables, schemata and PRAGMA settings in DB."
  (sqlite-execute db "PRAGMA user_version = 19;")
  (sqlite-execute db "PRAGMA foreign_keys = on;")
  ;; Note to devs: try M-x `indexed-roam--insert-schemata-atpt'
  (mapc
   (lambda (query) (sqlite-execute db query))
   '("CREATE TABLE files (
	file UNIQUE PRIMARY KEY,
	title,
	hash NOT NULL,
	atime NOT NULL,
	mtime NOT NULL
);"
     "CREATE TABLE nodes (
	id NOT NULL PRIMARY KEY,
	file NOT NULL,
	level NOT NULL,
	pos NOT NULL,
	todo,
	priority,
	scheduled text,
	deadline text,
	title,
	properties,
	olp,
	FOREIGN KEY (file) REFERENCES files (file) ON DELETE CASCADE
);"
     "CREATE TABLE aliases (
	node_id NOT NULL,
	alias,
	FOREIGN KEY (node_id) REFERENCES nodes (id) ON DELETE CASCADE
);"
     "CREATE TABLE citations (
	node_id NOT NULL,
	cite_key NOT NULL,
	pos NOT NULL,
	properties,
	FOREIGN KEY (node_id) REFERENCES nodes (id) ON DELETE CASCADE
);"
     "CREATE TABLE refs (
	node_id NOT NULL,
	ref NOT NULL,
	type NOT NULL,
	FOREIGN KEY (node_id) REFERENCES nodes (id) ON DELETE CASCADE
);"
     "CREATE TABLE tags (
	node_id NOT NULL,
	tag,
	FOREIGN KEY (node_id) REFERENCES nodes (id) ON DELETE CASCADE
);"
     "CREATE TABLE links (
	pos NOT NULL,
	source NOT NULL,
	dest NOT NULL,
	type NOT NULL,
	properties,
	FOREIGN KEY (source) REFERENCES nodes (id) ON DELETE CASCADE
);"))

  ;; That's it for theoretical compatibility with org-roam db version 19.
  ;; Now play with perf settings.
  ;; https://www.sqlite.org/pragma.html
  ;; https://www.sqlite.org/inmemorydb.html

  (sqlite-execute db "CREATE INDEX refs_node_id  ON refs    (node_id);")
  (sqlite-execute db "CREATE INDEX tags_node_id  ON tags    (node_id);")
  (sqlite-execute db "CREATE INDEX alias_node_id ON aliases (node_id);")
  (sqlite-execute db "PRAGMA cache_size = -40000;") ;; 40,960,000 bytes

  ;; Full disclosure, I have no idea what I'm doing
  (sqlite-execute db "PRAGMA mmap_size = 40960000;")
  (sqlite-execute db "PRAGMA temp_store = memory;")
  (sqlite-execute db "PRAGMA synchronous = off;")
  db)

;; This whole macro smells, but performs better than serial inserts
(defmacro indexed-roam--insert-en-masse (db table-sym n-cols)
  "Insert into DB the values of list named TABLE-SYM.
Insert into a table inside DB of the same name.

N-COLS must be the expected number of columns, and the list named
TABLE-SYM must be flat and divisible by N-COLS."
  (let ((template-row (concat "(" (string-join (make-list n-cols "?")
                                               ", ")
                              ")")))
    `(if ,table-sym
         (sqlite-execute
          ,db
          (concat ,(format "INSERT INTO %S VALUES " table-sym)
                  (string-join (make-list (length ,table-sym) ,template-row)
                               ", "))
          (apply #'nconc ,table-sym)))))

(defun indexed-roam--populate (db row-sets)
  "Populate DB with ROW-SETS, an output of `indexed-roam--mk-rows'."
  (seq-let (files nodes aliases citations refs tags links) row-sets
    (with-sqlite-transaction db
      (indexed-roam--insert-en-masse db files 5)
      (indexed-roam--insert-en-masse db nodes 11)
      (indexed-roam--insert-en-masse db aliases 2)
      (indexed-roam--insert-en-masse db citations 4)
      (indexed-roam--insert-en-masse db refs 3)
      (indexed-roam--insert-en-masse db tags 2)
      (indexed-roam--insert-en-masse db links 5))))

(defun indexed-roam--mk-rows (&optional specific-files)
  "Return info that org-roam can consume.

Specifically, return seven lists of rows, one for each SQL table
defined by `indexed-roam--configure'.

With SPECIFIC-FILES, only return data that involves those files."
  (let (file-rows
        node-rows
        alias-rows
        citation-rows
        ref-rows
        tag-rows
        link-rows
        (print-length nil)
        (seen-files (make-hash-table :test 'equal)))
    (cl-loop
     for entry in (indexed-id-nodes)
     as file = (indexed-file entry)
     when (or (not specific-files) (member file specific-files))
     do
     (unless (gethash file seen-files)
       (puthash file t seen-files)
       (push (indexed-roam--mk-file-row file) file-rows))
     (cl-symbol-macrolet ((..deadline     (indexed-deadline entry))
                          (..id           (indexed-id entry))
                          (..scheduled    (indexed-scheduled entry)))
       ;; See `org-roam-db-insert-aliases'
       (cl-loop for alias in (indexed-roam-aliases entry) do
                (push (list ..id alias) alias-rows))
       ;; See `org-roam-db-insert-tags'
       (cl-loop for tag in (indexed-tags entry) do
                (push (list ..id tag) tag-rows))
       ;; See `org-roam-db-insert-file-node' and `org-roam-db-insert-node-data'
       (push (list ..id
                   (indexed-file entry)
                   (indexed-heading-lvl entry)
                   (indexed-pos entry)
                   (indexed-todo entry)
                   (indexed-priority entry)
                   ;; HACK: efficient
                   (and ..scheduled
                        (concat (substring ..scheduled 1 11) "T12:00:00"))
                   (and ..deadline
                        (concat (substring ..deadline 1 11) "T12:00:00"))
                   (indexed-title entry)
                   ;; FIXME: Perf hotspot here. (30-70% of compute)
                   ;; Can we avoid prin1-to-string because we know exactly
                   ;; what data types these are?
                   (prin1-to-string (indexed-properties entry))
                   (prin1-to-string (indexed-olpath entry)))
             node-rows)
       ;; See `org-roam-db-insert-refs'
       (cl-loop for ref in (indexed-roam-refs entry) do
                (let ((type (gethash ref indexed-roam--ref<>type)))
                  (push (list ..id
                              ref
                              (or type "cite"))
                        ref-rows))))
     (dolist (link (append (indexed-id-links-to entry)
                           (indexed-roam-reflinks-to entry)))
       (let ((origin-node (gethash (indexed-origin link) indexed--id<>entry)))
         (if (not (indexed-pos link))
             (message "Null link pos in %s" link))
         (if (not origin-node)
             (message "Unknown ID: %s" (indexed-origin link))
           (if (indexed-type link)
               ;; See `org-roam-db-insert-link'
               (push (list (indexed-pos link)
                           (indexed-origin link)
                           (indexed-dest link)
                           (indexed-type link)
                           nil)
                     link-rows)
             ;; See `org-roam-db-insert-citation'
             (push (list (indexed-origin link)
                         (substring (indexed-dest link) 1)
                         (indexed-pos link)
                         nil)
                   citation-rows))))))
    (list
     file-rows node-rows alias-rows citation-rows ref-rows tag-rows link-rows)))

(defun indexed-roam--mk-file-row (file)
  "Return info about FILE."
  (let* ((data (indexed-file-data file))
         (lisp-mtime (prin1-to-string (seconds-to-time (indexed-mtime data)))))
    (list file
          (indexed-title data)
          ""         ; HACK: Hashing is slow, skip
          lisp-mtime ; HACK: org-roam doesn't use atime anyway
          lisp-mtime)))


;;; Optional

(defun indexed-roam--index-files (files)
  "Update current DB about nodes and links involving FILES.
Suitable on `org-node-rescan-functions'."
  ;; NOTE: There's a likely performance bug in Emacs sqlite.c.
  ;;       I have a yuge file, which takes 0.01 seconds to delete on the
  ;;       sqlite3 command line... but 0.53 seconds with `sqlite-execute'.
  ;;
  ;;       Aside from tracking down the bug, could we workaround by getting rid
  ;;       of all the CASCADE rules and pre-determine what needs to be deleted?
  ;;       It's not The Way to use a RDBMS, but it's a simple enough puzzle.
  (let ((db (indexed-roam))
        (rows (indexed-roam--mk-rows files)))
    (dolist (file files)
      (sqlite-execute db "DELETE FROM files WHERE file LIKE ?" (list file)))
    (indexed-roam--populate db rows)))

;; TODO: Maybe generalize: track a list of open DBs and explore any of them
(defun indexed-roam-explore (&optional db)
  "Explore contents of currently used SQLite DB.

With optional argument DB, explore that database connection
instead of default `indexed-roam--connection'."
  (interactive)
  (cl-assert (sqlite-available-p))
  (let ((db (or db (indexed-roam))))
    (unless (ignore-errors (sqlite-pragma db "im_still_here"))
      (error "indexed-roam-show-contents: Not a live DB connection: %s" db))
    (pop-to-buffer
     (get-buffer-create (format "*SQLite %.50s*" (prin1-to-string db))))
    (sqlite-mode)
    (setq-local sqlite--db db)
    (sqlite-mode-list-tables)))


;;; Dev tools

(defvar emacsql-type-map)
(defun indexed-roam--insert-schemata-atpt ()
  "Dev tool for printing `org-roam-db--table-schemata' as raw SQL.
Must load library \"org-roam\"."
  (interactive)
  (require 'org-roam)
  (require 'emacsql)
  (when (and (boundp 'org-roam-db--table-schemata)
             (fboundp 'emacsql-format)
             (fboundp 'emacsql-prepare))
    (cl-loop
     with emacsql-type-map = '((integer "INTEGER")
                               (float "REAL")
                               (object "TEXT")
                               (nil nil))
     with exp = (let ((exp* (emacsql-prepare [:create-table $i1 $S2])))
                  (cons (thread-last (car exp*)
                                     (string-replace "("  "(\n\t")
                                     (string-replace ")"  "\n)"))
                        (cdr exp*)))
     for (table schema) in org-roam-db--table-schemata
     do (insert " \n\""
                (string-replace ", " ",\n\t"
                                (emacsql-format exp table schema))
                ";\""))))


;;; Mode

;;;###autoload
(define-minor-mode indexed-roam-mode
  "Index extra data sought by org-roam and org-node et al."
  :global t
  :group 'indexed
  (if indexed-roam-mode
      (progn
        (add-hook 'indexed--post-reset-functions #'indexed-roam--mk-lisp-tables -95)
        (add-hook 'indexed--post-reset-functions #'indexed-roam--mk-db -91))
    (remove-hook 'indexed--post-reset-functions #'indexed-roam--mk-lisp-tables)
    (remove-hook 'indexed--post-reset-functions #'indexed-roam--mk-db)))

(provide 'indexed-roam)

;;; indexed-roam.el ends here
