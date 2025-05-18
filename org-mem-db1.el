;;; org-mem-db1.el --- Experimental variant SQL database -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Our own SQL DB, with schemata free to evolve independently of what org-roam
;; decides for theirs.

;; Notable differences:

;; - Probably not usable by EmacSQL.
;;   - Made with built-in `sqlite-select' in mind.

;; - In cases where we want to store a Lisp list literally, we use `prin1',
;;   but we do not `prin1' things that are already strings.
;;   - That means you don't have to contend with backslash-escaped quote
;;     characters.

;; - New table "properties".

;; - No "refs" or "aliases" tables.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sqlite)
(require 'llama)
(require 'org-mem)

(defvar org-mem-db1--connection nil
  "A SQLite handle.")

(defun org-mem-db1--re-make-db (&rest _)
  "Close current `org-mem-db1--connection' and populate a new one."
  (ignore-errors (sqlite-close org-mem-db1--connection))
  (org-mem-db1))

;;;###autoload
(define-minor-mode org-mem-db1-mode
  "Make available the `org-mem-db1' database."
  :global t
  :group 'org-mem
  (if org-mem-db1-mode
      (progn
        (add-hook
         'org-mem-post-full-scan-functions #'org-mem-db1--re-make-db)
        ;; (add-hook
        ;;  'org-mem-post-targeted-scan-functions #'org-mem-db1--update-db)
        (org-mem--scan-full))
    (remove-hook
     'org-mem-post-full-scan-functions #'org-mem-db1--re-make-db)
    ;; (remove-hook
    ;;  'org-mem-post-targeted-scan-functions #'org-mem-db1--update-db)
    ))


;;; Database

;;;###autoload
(defun org-mem-db1 (&optional sql &rest args)
  "Return the SQLite handle.
Each call checks if it is alive, and renews if not.

If arguments SQL and ARGS provided, pass to `sqlite-select'."
  (unless org-mem-db1-mode
    (error "Enable `org-mem-db1-mode' to use `org-mem-db1'"))
  (or (ignore-errors (sqlite-pragma org-mem-db1--connection "im_still_here"))
      (setq org-mem-db1--connection (org-mem-db1--open-new-db)))
  (if sql
      (sqlite-select org-mem-db1--connection sql args)
    org-mem-db1--connection))

(defun org-mem-db1--open-new-db (&optional loc)
  "Generate a new database and return a connection-handle to it.
Create tables and pre-populate them with data.

Normally, this creates a diskless database.  With optional file path
LOC, write the database as a file to LOC."
  (let ((T (current-time))
        (name (or loc "org-mem-db1 DB"))
        (db (sqlite-open loc)))
    (org-mem-db1--configure db)
    (org-mem-db1--populate db (org-mem-db1--mk-rows))
    (when org-mem--next-message
      (setq org-mem--next-message
            (concat org-mem--next-message
                    (format " +%.2fs writing %s"
                            (float-time (time-since T)) name))))
    db))

;; This needs more eyes, very sure the schemata can be much better designed
;; than this.

;; One thing in pipeline is auto-creating tables for each Org
;; property discovered, so we'd automatically get equivalents of org-roam's
;; aliases and refs tables, for example.

(defun org-mem-db1--configure (db)
  "Set up tables, schemata and PRAGMA settings in DB."
  (sqlite-execute db "PRAGMA user_version = 1;")
  (sqlite-execute db "PRAGMA foreign_keys = on;")
  (mapc
   (lambda (query) (sqlite-execute db query))
   '("CREATE TABLE files (
	file_name TEXT UNIQUE PRIMARY KEY,
	title TEXT,
	max_lines INTEGER NOT NULL,
	mtime INTEGER NOT NULL,
	ptmax INTEGER NOT NULL,
	toplvl_id TEXT
);"
     "CREATE TABLE entries (
	id TEXT NOT NULL PRIMARY KEY,
	file_name TEXT NOT NULL,
	heading_lvl INTEGER NOT NULL,
	pos INTEGER NOT NULL,
	todo_state INTEGER,
	priority TEXT,
	scheduled TEXT,
	deadline TEXT,
	title TEXT,
	olpath TEXT,
	FOREIGN KEY (file_name) REFERENCES files(file_name) ON DELETE CASCADE
);"
     "CREATE TABLE citations (
	node_id TEXT NOT NULL,
	cite_key TEXT NOT NULL,
	pos INTEGER NOT NULL,
	file_name TEXT NOT NULL,
	FOREIGN KEY (node_id) REFERENCES entries(id) ON DELETE CASCADE
);"
     "CREATE TABLE tags (
	node_id TEXT NOT NULL,
	tag TEXT,
	FOREIGN KEY (node_id) REFERENCES entries(id) ON DELETE CASCADE
);"
     "CREATE TABLE links (
	pos INTEGER NOT NULL,
	nearby_id TEXT,
	dest TEXT NOT NULL,
	type TEXT NOT NULL,
	file_name TEXT NOT NULL,
	PRIMARY KEY (file_name, pos),
	FOREIGN KEY (nearby_id) REFERENCES entries(id) ON DELETE CASCADE
);"
     "CREATE TABLE properties (
	node_id TEXT NOT NULL,
	property TEXT NOT NULL,
	value TEXT,
	FOREIGN KEY (node_id) REFERENCES entries(id) ON DELETE CASCADE
);"))

  ;; (sqlite-execute db "CREATE INDEX links_dest ON links(dest);")
  (sqlite-execute db "CREATE INDEX tags_node_id ON tags(node_id);")
  (sqlite-execute db "CREATE INDEX property_node_id ON properties(node_id);")
  (sqlite-execute db "PRAGMA cache_size = -40000;") ;; 40,960,000 bytes
  (sqlite-execute db "PRAGMA mmap_size = 81920000;")
  (sqlite-execute db "PRAGMA temp_store = memory;")
  (sqlite-execute db "PRAGMA synchronous = off;")
  db)

;; This whole macro smells, but performs better than serial inserts
(defmacro org-mem-db1--insert-en-masse (db table-sym n-cols)
  "Insert into DB the values of list named TABLE-SYM.
Assume there exists a table of same name in DB.

N-COLS must be the expected number of columns, and the value of
TABLE-SYM must be a list of lists of exactly N-COLS items."
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

(defun org-mem-db1--populate (db row-sets)
  "Populate DB with ROW-SETS, an output of `org-mem-db1--mk-rows'."
  (seq-let (files entries citations tags links properties) row-sets
    (with-sqlite-transaction db
      (org-mem-db1--insert-en-masse db files 6)
      (org-mem-db1--insert-en-masse db entries 10)
      (org-mem-db1--insert-en-masse db citations 4)
      (org-mem-db1--insert-en-masse db tags 2)
      (org-mem-db1--insert-en-masse db links 5)
      (org-mem-db1--insert-en-masse db properties 3))))

(defun org-mem-db1--mk-rows (&optional specific-files)
  "Return rows of data suitable for inserting into `org-mem-db1' DB.

Specifically, return seven lists of rows, one for each SQL table
defined by `org-mem-db1--configure'.

With SPECIFIC-FILES, only return data that involves those files."
  (let (file-rows
        entry-rows
        citation-rows
        tag-rows
        link-rows
        prop-rows)

    (cl-loop
     with seen-files = (make-hash-table :test 'equal)
     for entry in (org-mem-all-id-nodes)
     as file = (org-mem-entry-file entry)
     as id = (org-mem-entry-id entry)
     when (or (not specific-files) (member file specific-files))
     do (progn
          (unless (gethash file seen-files)
            (puthash file t seen-files)
            (push (org-mem-db1--mk-file-row file) file-rows))
          (cl-loop for tag in (org-mem-entry-tags entry) do
                   (push (list id tag) tag-rows))
          (push (list id
                      (org-mem-entry-file entry)
                      (org-mem-entry-level entry)
                      (org-mem-entry-pos entry)
                      (org-mem-entry-todo-state entry)
                      (org-mem-entry-priority entry)
                      (org-mem-entry-scheduled entry)
                      (org-mem-entry-deadline entry)
                      (org-mem-entry-title entry)
                      (prin1-to-string (org-mem-entry-olpath entry) nil '((length))))
                entry-rows)
          (cl-loop for (prop . val) in (org-mem-entry-properties entry)
                   do (push (list id prop val) prop-rows))))
    
    (cl-loop for link in (org-mem-all-links)
             as file-name = (org-mem-link-file link)
             when (or (not specific-files)
                      (member file-name specific-files))
             do (if (org-mem-link-type link)
                    (push (list (org-mem-link-pos link)
                                (org-mem-link-nearby-id link)
                                (org-mem-link-target link)
                                (org-mem-link-type link)
                                file-name)
                          link-rows)
                  (when (org-mem-link-nearby-id link)
                    (push (list (org-mem-link-nearby-id link)
                                (substring (org-mem-link-target link) 1)
                                (org-mem-link-pos link)
                                file-name)
                          citation-rows))))

    (list file-rows
          entry-rows
          citation-rows
          tag-rows
          link-rows
          prop-rows)))

(defun org-mem-db1--mk-file-row (file)
  "Return a row of meta-data about FILE, for the files-table."
  (list file
        (org-mem-file-title-strict file)
        (org-mem-file-line-count file)
        (org-mem-file-mtime-int file)
        (org-mem-file-ptmax file)
        (org-mem-file-id-strict file)))

(provide 'org-mem-db1)

;;; org-mem-db1.el ends here
