;;; indexed-orgdb.el --- Our own SQL database -*- lexical-binding: t; -*-

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
;; - In cases where we want to store a Lisp list literally (as of 2025-03-20,
;;   that's only the OLPATH), we use `prin1', but we do not `prin1' things that
;;   are already strings.
;;   - That means you don't have to contend with backslash-escaped quote
;;     characters.
;; - New table "properties".
;; - No "refs" or "aliases".

;;; Code:

(require 'indexed)
(require 'sqlite)

(defvar indexed-orgdb--connection nil
  "A SQLite handle.")

(defun indexed-orgdb--re-make-db (&rest _)
  "Close current `indexed-orgdb--connection' and populate a new one."
  (ignore-errors (sqlite-close indexed-orgdb--connection))
  (indexed-orgdb))

;;;###autoload
(define-minor-mode indexed-orgdb-mode
  "Make available the `indexed-orgdb' database."
  :global t
  :group 'indexed
  (if indexed-orgdb-mode
      (progn
        (add-hook
         'indexed-post-full-reset-functions #'indexed-orgdb--re-make-db)
        ;; (add-hook
        ;;  'indexed-post-incremental-update-functions #'indexed-orgdb--update-db)
        (indexed--scan-full))
    (remove-hook
     'indexed-post-full-reset-functions #'indexed-orgdb--re-make-db)
    ;; (remove-hook
    ;;  'indexed-post-incremental-update-functions #'indexed-orgdb--update-db)
    ))


;;; Database

;;;###autoload
(defun indexed-orgdb (&optional sql &rest args)
  "Return the SQLite handle.
Each call checks if it is alive, and renews if not.

If arguments SQL and ARGS provided, pass to `sqlite-select'."
  (unless indexed-orgdb-mode
    (error "Enable `indexed-orgdb-mode' to use `indexed-orgdb'"))
  (or (ignore-errors (sqlite-pragma indexed-orgdb--connection "im_still_here"))
      (setq indexed-orgdb--connection (indexed-orgdb--open-new-db)))
  (if sql
      (sqlite-select indexed-orgdb--connection sql args)
    indexed-orgdb--connection))

(defun indexed-orgdb--open-new-db (&optional loc)
  "Generate a new database and return a connection-handle to it.
Create tables and pre-populate them with data.

Normally, this creates a diskless database.  With optional file path
LOC, write the database as a file to LOC."
  (let ((T (current-time))
        (name (or loc "SQLite DB"))
        (db (sqlite-open loc)))
    (indexed-orgdb--configure db)
    (indexed-orgdb--populate db (indexed-orgdb--mk-rows))
    (when indexed--next-message
      (setq indexed--next-message
            (concat indexed--next-message
                    (format " +%.2fs writing %s"
                            (float-time (time-since T)) name))))
    db))

;; This needs more eyes, very sure the schemata can be much better designed
;; than this.

;; One thing I got in pipeline is auto-creating tables for each Org
;; property discovered, so we'd automatically get equivalents of org-roam's
;; aliases and refs tables, for example.

(defun indexed-orgdb--configure (db)
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
(defmacro indexed-orgdb--insert-en-masse (db table-sym n-cols)
  "Insert into DB the values of list named TABLE-SYM.
Insert into a table inside DB of the same name.

N-COLS must be the expected number of columns, and the list named
TABLE-SYM must only contain lists of exactly that many items."
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

(defun indexed-orgdb--populate (db row-sets)
  "Populate DB with ROW-SETS, an output of `indexed-orgdb--mk-rows'."
  (seq-let (files entries citations tags links properties) row-sets
    (with-sqlite-transaction db
      (indexed-orgdb--insert-en-masse db files 6)
      (indexed-orgdb--insert-en-masse db entries 10)
      (indexed-orgdb--insert-en-masse db citations 4)
      (indexed-orgdb--insert-en-masse db tags 2)
      (indexed-orgdb--insert-en-masse db links 5)
      (indexed-orgdb--insert-en-masse db properties 3))))

(defun indexed-orgdb--mk-rows (&optional specific-files)
  "Return rows of data suitable for inserting into `indexed-orgdb' DB.

Specifically, return seven lists of rows, one for each SQL table
defined by `indexed-orgdb--configure'.

With SPECIFIC-FILES, only return data that involves those files."
  (let (file-rows
        entry-rows
        citation-rows
        tag-rows
        link-rows
        prop-rows)

    (cl-loop
     with seen-files = (make-hash-table :test 'equal)
     for entry in (indexed-org-id-nodes)
     as file = (indexed-file-name entry)
     as id = (indexed-id entry)
     when (or (not specific-files) (member file specific-files))
     do (progn
          (unless (gethash file seen-files)
            (puthash file t seen-files)
            (push (indexed-orgdb--mk-file-row file) file-rows))
          (cl-loop for tag in (indexed-tags entry) do
                   (push (list id tag) tag-rows))
          (push (list id
                      (indexed-file-name entry)
                      (indexed-heading-lvl entry)
                      (indexed-pos entry)
                      (indexed-todo-state entry)
                      (indexed-priority entry)
                      (indexed-scheduled entry)
                      (indexed-deadline entry)
                      (indexed-title entry)
                      (prin1-to-string (indexed-olpath entry) nil '((length))))
                entry-rows)
          (cl-loop for (prop . val) in (indexed-properties entry)
                   do (push (list id prop val) prop-rows))))
    
    (cl-loop for link in (indexed-org-links)
             as file = (indexed-org-link-file-name link)
             when (or (not specific-files) (member file specific-files))
             do (if (indexed-type link)
                    (push (list (indexed-pos link)
                                (indexed-nearby-id link)
                                (indexed-dest link)
                                (indexed-type link)
                                file)
                          link-rows)
                  (when (indexed-nearby-id link)
                    (push (list (indexed-nearby-id link)
                                (substring (indexed-dest link) 1)
                                (indexed-pos link)
                                file)
                          citation-rows))))

    (list file-rows
          entry-rows
          citation-rows
          tag-rows
          link-rows
          prop-rows)))

(defun indexed-orgdb--mk-file-row (file)
  "Return a row representing FILE for the files-table."
  (let ((data (indexed-file-data file)))
    (list file
          (indexed-file-data-file-title data)
          (indexed-file-data-max-lines data)
          (indexed-file-data-mtime data)
          (indexed-file-data-ptmax data)
          (indexed-file-data-toplvl-id data))))

(provide 'indexed-orgdb)

;;; indexed-orgdb.el ends here
