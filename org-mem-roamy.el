;;; org-mem-roamy.el --- Make data like org-roam does -*- lexical-binding: t; -*-

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

;; https://github.com/org-roam/org-roam

;; Called "roamy" to visually distinguish from "roam" easier.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sqlite)
(require 'org-mem)
(require 'llama)
(declare-function eieio-oref "eieio-core")
(defvar org-mem-updater-mode)

(defcustom org-mem-roamy-do-overwrite-real-db nil
  "Whether to overwrite the database file at `org-roam-db-location'.

Note that a database connection is always available at `org-mem-roamy-db'.
When this setting is t, you should find that `org-roam-db-query' also
works as expected.  However:

1. Package \"org-roam\" must be installed.
2. Only one Emacs session can use the database.

Note that you can trivially rewrite your database queries from:

  \(org-roam-db-query QUERY...)

to:

  \(emacsql (org-mem-roamy-db) QUERY...)"
  :type 'boolean
  :group 'org-mem
  :package-version '(org-mem . "0.4.0"))

(defcustom org-mem-roamy-do-try-async t
  "Whether to use an asynchronous technique on saving a big file.

Currently only applies when `org-mem-roamy-do-overwrite-real-db' also t."
  :type 'boolean
  :group 'org-mem
  :package-version '(org-mem . "0.8.0"))

;;;###autoload
(define-minor-mode org-mem-roamy-db-mode
  "Instantiate an `org-mem-roamy-db' database and keep it updated.
May slow down \\[org-mem-reset].
See also user option `org-mem-roamy-do-overwrite-real-db'."
  :global t
  :group 'org-mem
  (if org-mem-roamy-db-mode
      (progn
        (add-hook 'org-mem-post-targeted-scan-functions 'org-mem-roamy--update-db)
        (add-hook 'org-mem-post-full-scan-functions 'org-mem-roamy--re-make-db)
        (org-mem--scan-full))
    (remove-hook 'org-mem-post-targeted-scan-functions 'org-mem-roamy--update-db)
    (remove-hook 'org-mem-post-full-scan-functions 'org-mem-roamy--re-make-db)))

(defvar org-mem-roamy--connection nil
  "An EmacSQL connection.")

(defun org-mem-roamy--re-make-db (&rest _)
  "Close current `org-mem-roamy--connection' and populate a new one."
  (when (and org-mem-roamy-do-overwrite-real-db
             (boundp 'org-roam-db-version)
             (> org-roam-db-version 20))
    (display-warning 'org-mem-roamy "
Org-roam bumped the DB version, and Org-mem has not caught up.
Setting `org-mem-roamy-do-overwrite-real-db' to nil for now.
Restore org-roam functionality with: (setq org-roam-db-update-on-save t)")
    (setq org-mem-roamy-do-overwrite-real-db nil))
  (if org-mem-roamy-do-overwrite-real-db
      (and (fboundp 'org-roam-db--close)
           (org-roam-db--close))
    (and (fboundp 'emacsql-close)
         (fboundp 'emacsql-live-p)
         org-mem-roamy--connection
         (emacsql-live-p org-mem-roamy--connection)
         (emacsql-close org-mem-roamy--connection)))
  (org-mem-roamy-db))

;; REVIEW: It may feel cleaner if the behavior of `org-mem-roamy-db' was not
;;         gated on `org-mem-roamy-do-overwrite-real-db' at all, and we
;;         potentially just have two separate, identical DBs.  But doing it
;;         this way allows org-roam extensions to just talk to
;;         `org-mem-roamy-db' and be confident that it is the same as talking
;;         to `org-roam-db', so they can drop the strict org-roam dependency.
;;
;;         Though... since our DB never needs to be written-to by an extension
;;         to keep things up to date (seeing as we wipe and rebuild it so
;;         much), it could still be sane to allow duplicating them.
;;
;;         So for code simplicity, we could turn this into a small dispatch
;;         that picks whichever connection the user configured org-mem to
;;         make, or pick at random if both are for some reason up (untypical
;;         case).  Still, that's *essentially* what's already happening (sans
;;         allowing both to be up), so just a matter of readable subroutines.
(defun org-mem-roamy-db ()
  "Return an EmacSQL connection.

If user option `org-mem-roamy-do-overwrite-real-db' is t, return the same
connection as that returned by `org-roam-db'.

If such a connection is not yet open, take the opportunity to delete
the database file on disk and write a new one, then connect to that."
  (unless org-mem-roamy-db-mode
    (error "Enable `org-mem-roamy-db-mode' to use `org-mem-roamy-db'"))
  (if (and (require 'emacsql nil t)
           (require 'emacsql-sqlite nil t)
           (fboundp 'emacsql-live-p)
           (fboundp 'emacsql-sqlite-open)
           (fboundp 'emacsql-sqlite-default-connection))
      (if (not (eq (emacsql-sqlite-default-connection)
                   'emacsql-sqlite-builtin-connection))
          (error "`org-mem-roamy-db-mode' requires built-in SQLite")
        (let ((T (current-time))
              conn name)

          (if org-mem-roamy-do-overwrite-real-db
              (if (and (require 'org-roam nil t)
                       (fboundp 'org-roam-db)
                       (fboundp 'org-roam-db--close)
                       (fboundp 'org-roam-db--get-connection)
                       (boundp 'org-roam-db-location))
                  (progn
                    (when (and org-mem-updater-mode
                               (bound-and-true-p org-roam-db-update-on-save))
                      (error "Both options should not be t: `org-mem-roamy-do-overwrite-real-db' and `org-roam-db-update-on-save'"))
                    (setq conn (org-roam-db--get-connection))
                    ;; FIXME
                    ;; (when org-mem--next-message
                    ;;   (cl-assert (null conn)))
                    (unless (and conn (emacsql-live-p conn))
                      ;; No live connection, take the chance to repopulate.
                      ;; Note that live connections sometimes get closed by
                      ;; `org-mem-roamy--re-make-db', such as when you turn on
                      ;; `org-mem-roamy-db-mode'.
                      ;; Delete file instead of using `org-roam-db-clear-all',
                      ;; b/c that takes a dozen seconds.
                      (cl-assert (file-name-absolute-p org-roam-db-location))
                      (when (file-exists-p org-roam-db-location)
                        (delete-file org-roam-db-location))
                      (setq conn (org-roam-db))
                      (setq name (and org-roam-db-location
                                      (file-name-nondirectory org-roam-db-location)))
                      (org-mem-roamy--populate-db-usably-for-emacsql
                       (eieio-oref conn 'handle)
                       (org-mem-roamy--mk-rows))))
                (error "Option `org-mem-roamy-do-overwrite-real-db' is t, but org-roam unavailable"))
            ;; Make our own diskless DB.
            (setq conn org-mem-roamy--connection)
            ;; FIXME
            ;; (when org-mem--next-message
            ;;   (cl-assert (null conn)))
            (unless (and conn (emacsql-live-p conn))
              (setq conn (emacsql-sqlite-open nil))
              (setq org-mem-roamy--connection conn)
              (org-mem-roamy--configure (eieio-oref conn 'handle))
              (org-mem-roamy--populate-db-usably-for-emacsql
               (eieio-oref conn 'handle)
               (org-mem-roamy--mk-rows))))

          (when org-mem--next-message
            ;; Print a benchmark if triggered by command `org-mem-reset'.
            (setq org-mem--next-message
                  (concat org-mem--next-message
                          (format " + %.2fs writing %s"
                                  (float-time (time-since T))
                                  (or name "org-mem-roamy-db")))))
          conn))
    (error "`org-mem-roamy-db' requires package \"emacsql\"")))

(defun org-mem-roamy--configure (db)
  "Set up tables, schemata and PRAGMA settings in DB."
  (sqlite-execute db "PRAGMA user_version = 20;")
  (sqlite-execute db "PRAGMA foreign_keys = on;")
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

  ;; That's it for compat with org-roam db.
  ;; Now play with perf settings.
  ;; https://www.sqlite.org/pragma.html
  ;; https://www.sqlite.org/inmemorydb.html

  (sqlite-execute db "CREATE INDEX refs_node_id  ON refs    (node_id);")
  (sqlite-execute db "CREATE INDEX tags_node_id  ON tags    (node_id);")
  (sqlite-execute db "CREATE INDEX alias_node_id ON aliases (node_id);")

  ;; Full disclosure, I have no idea what I'm doing
  (sqlite-execute db "PRAGMA cache_size = -40000;") ;; 40,960,000 bytes
  (sqlite-execute db "PRAGMA mmap_size = 81920000;")
  (unless org-mem-roamy-do-overwrite-real-db
    (sqlite-execute db "PRAGMA temp_store = memory;")
    (sqlite-execute db "PRAGMA synchronous = off;"))

  db)

(defun org-mem-roamy--populate-db-usably-for-emacsql (db row-sets)
  "Insert into DB the ROW-SETS."
  (seq-let (files nodes aliases citations refs tags links) row-sets
    (with-sqlite-transaction db
      (when files
        (sqlite-execute
         db (concat
             "INSERT INTO files VALUES "
             (org-mem-roamy--mk-literal-input-quoted-like-emacsql files))))
      (when nodes
        (sqlite-execute
         db (concat
             "INSERT INTO nodes VALUES "
             (org-mem-roamy--mk-literal-input-quoted-like-emacsql nodes))))
      (when aliases
        (sqlite-execute
         db (concat
             "INSERT INTO aliases VALUES "
             (org-mem-roamy--mk-literal-input-quoted-like-emacsql aliases))))
      (when citations
        (sqlite-execute
         db (concat
             "INSERT INTO citations VALUES "
             (org-mem-roamy--mk-literal-input-quoted-like-emacsql citations))))
      (when refs
        (sqlite-execute
         db (concat
             "INSERT INTO refs VALUES "
             (org-mem-roamy--mk-literal-input-quoted-like-emacsql refs))))
      (when tags
        (sqlite-execute
         db (concat
             "INSERT INTO tags VALUES "
             (org-mem-roamy--mk-literal-input-quoted-like-emacsql tags))))
      (when links
        (sqlite-execute
         db (concat
             "INSERT INTO links VALUES "
             (org-mem-roamy--mk-literal-input-quoted-like-emacsql links)))))))

(defun org-mem-roamy--mk-literal-input-quoted-like-emacsql (rows)
  "Turn ROWS into a literal \(not prepared) input for SQL INSERT.

In each row, print readably any atoms that are strings or lists,
i.e. use `prin1', to be usable to `emacsql'.  That means e.g. string
atoms have extraneous quote characters, with the consequence that the
database will be difficult to use with `sqlite-select'."
  (with-temp-buffer
    (let ((print-level nil)
          (print-length nil)
          (print-escape-newlines t)
          (print-escape-control-characters t)
          row beg)
      (while (setq row (pop rows))
        (insert "(")
        (cl-loop for value in row do
                 (cond ((null value)
                        (insert "NULL"))
                       ((numberp value)
                        (insert (number-to-string value)))
                       ((progn
                          (insert "'")
                          (setq beg (point))
                          (prin1 value (current-buffer))
                          (goto-char beg)
                          (while (search-forward "'" nil t)
                            (insert "'"))
                          (goto-char (point-max))
                          (insert "'"))))
                 (insert ", "))
        (unless (= 2 (point)) ;; In case above loop was a no-op
          (delete-char -2))
        (insert "), "))
      (unless (bobp) ; In case ROWS was empty
        (delete-char -2)))
    (buffer-string)))

(defvar org-mem-roamy--untitled-id-nodes nil)
(defun org-mem-roamy--mk-rows (&optional specific-files)
  "Return rows of data suitable for inserting into `org-mem-roamy-db'.

Specifically, return seven lists of rows, one for each SQL table
created by `org-mem-roamy--configure'.

With SPECIFIC-FILES, only return data that involves those files."
  (setq org-mem-roamy--untitled-id-nodes nil)
  (setq specific-files
        (nconc specific-files
               (seq-keep #'org-mem--truename-maybe specific-files)))
  (let (file-rows
        node-rows
        alias-rows
        citation-rows
        ref-rows
        tag-rows
        link-rows
        (print-length nil)
        (seen-files (make-hash-table :test 'equal))
        (roam-dir (when (and (boundp 'org-roam-directory)
                             (stringp org-roam-directory))
                    (cl-assert (file-name-absolute-p org-roam-directory))
                    (file-truename org-roam-directory))))
    (cl-loop
     for entry in (hash-table-values org-mem--id<>entry)
     as file = (org-mem-entry-file-truename entry)
     as id = (org-mem-entry-id entry)
     as title = (org-mem-entry-title-maybe entry)
     unless (and specific-files (not (member file specific-files)))
     unless (and roam-dir (not (string-prefix-p roam-dir file)))
     do
     (unless title
       (push entry org-mem-roamy--untitled-id-nodes))
     (unless (gethash file seen-files)
       (puthash file t seen-files)
       (push (org-mem-roamy--mk-file-row file) file-rows))

     ;; See `org-roam-db-insert-aliases'
     (cl-loop for alias in (org-mem-entry-roam-aliases entry) do
              (push (list id alias) alias-rows))
     ;; See `org-roam-db-insert-tags'
     (cl-loop for tag in (org-mem-entry-tags entry) do
              (push (list id tag) tag-rows))
     ;; See `org-roam-db-insert-file-node' and `org-roam-db-insert-node-data'
     (push (list id
                 (org-mem-entry-file-truename entry)
                 (org-mem-entry-level entry)
                 (org-mem-entry-pos entry)
                 (org-mem-entry-todo-state entry)
                 (org-mem-entry-priority entry)
                 (org-mem-entry-scheduled entry)
                 (org-mem-entry-deadline entry)
                 title
                 (org-mem-entry-properties entry)
                 (org-mem-entry-olpath entry))
           node-rows)
     ;; See `org-roam-db-insert-refs'
     (cl-loop for ref in (org-mem-entry-roam-refs entry) do
              (let ((type (gethash ref org-mem--roam-ref<>type)))
                (push (list id
                            (if (or (string-prefix-p "@" ref)
                                    (string-prefix-p "&" ref))
                                (substring ref 1)
                              ref)
                            (or type "cite"))
                      ref-rows))))

    (let ((dummy-props '(:outline nil)))
      (cl-loop
       for link in (org-mem-all-links)
       as file = (org-mem-link-file-truename link)
       when (org-mem-link-nearby-id link)
       unless (and specific-files (not (member file specific-files)))
       unless (and roam-dir (not (string-prefix-p roam-dir file)))
       do (if (org-mem-link-citation-p link)
              ;; See `org-roam-db-insert-citation'
              (push (list (org-mem-link-nearby-id link)
                          (substring (org-mem-link-target link) 1)
                          (org-mem-link-pos link)
                          dummy-props)
                    citation-rows)
            ;; See `org-roam-db-insert-link'
            (when (org-mem-link-type link)
              (push (list (org-mem-link-pos link)
                          (org-mem-link-nearby-id link)
                          (org-mem-link-target link)
                          (org-mem-link-type link)
                          dummy-props)
                    link-rows)))))

    (list file-rows
          node-rows
          alias-rows
          citation-rows
          ref-rows
          tag-rows
          link-rows)))

;; Numeric times can mix-and-match with Lisp times, i.e. these return the same:
;;    (format-time-string "%F %T" (time-add (time-to-seconds) 100))
;;    (format-time-string "%F %T" (time-add (current-time) 100))
;; So, we skip the overhead of `prin1-to-string' and just store numeric mtime,
;; unlike org-roam, which stores lists.
;; Overhead is hundreds of ms. https://github.com/org-roam/org-roam/pull/2509

(defun org-mem-roamy--mk-file-row (file)
  "Return a row for the files-table, with info about FILE."
  ;; See `org-roam-db-insert-file'
  (list file
        (org-mem-file-title-strict file)
        ""                            ; HACK: SHA1 hashing is slow, skip
        (org-mem-file-mtime-int file) ; HACK: org-roam doesn't use atime anyway
        (org-mem-file-mtime-int file)))


;;; Update-on-save

(defvar org-mem-roamy--async-new-rows nil)
(defun org-mem-roamy--update-db (parse-results)
  "Update currently connected DB, with data from PARSE-RESULTS.
Designed for `org-mem-post-targeted-scan-functions'."
  (seq-let (bad-paths file-data entries) parse-results
    (when (or bad-paths file-data)
      (let* ((T (current-time))
             (db (eieio-oref (org-mem-roamy-db) 'handle))
             (db-file (eieio-oref (org-mem-roamy-db) 'file))
             (sqlite3 (executable-find "sqlite3"))
             (newly-parsed-files (mapcar #'car file-data))
             (deletion-queries
              (append
               (cl-loop
                for file in (append bad-paths newly-parsed-files)
                collect (format "DELETE FROM files WHERE file LIKE '%s';"
                                (prin1-to-string file)))
               ;; Prevent "FOREIGN KEY constraint failed" or "UNIQUE
               ;; constraint failed" in case an entry got refiled to a
               ;; different file, and that file gets saved without saving the
               ;; previous file.  In that case, we're about to add entries
               ;; that appear to already exist in another location.
               (cl-loop
                for entry in entries
                as id = (and (org-mem-entry-id entry)
                             (not (member (org-mem-entry-file entry)
                                          newly-parsed-files))
                             (prin1-to-string (org-mem-entry-id entry)))
                when id
                collect (format "DELETE FROM nodes WHERE id LIKE '%s';"
                                id)
                and collect (format "DELETE FROM links WHERE source LIKE '%s';"
                                    id)))))
        ;; Maybe do the deletion async.  Otherwise Emacs blocks for seconds
        ;; waiting for sqlite to cascade-delete a big file.
        ;; Alas, an in-memory db can't be async...
        (if (and org-mem-roamy-do-try-async
                 db-file
                 sqlite3
                 (fboundp 'emacsql-close)
                 (fboundp 'org-roam-db))
            (progn
              (emacsql-close (org-roam-db))
              (make-process
               :name "sqlite3-org-mem-roamy"
               :command (list sqlite3
                              db-file
                              (concat "PRAGMA foreign_keys = on; "
                                      (apply #'concat deletion-queries)))
               :noquery t
               :sentinel (lambda (_process _event)
                           (when org-mem-roamy--async-new-rows
                             (org-mem-roamy--populate-db-usably-for-emacsql
                              (eieio-oref (org-roam-db) 'handle)
                              org-mem-roamy--async-new-rows))))
              (setq org-mem-roamy--async-new-rows
                    (and newly-parsed-files
                         (org-mem-roamy--mk-rows newly-parsed-files))))
          ;; Non-async method
          (dolist (query deletion-queries)
            (sqlite-execute db query))
          (let ((elapsed (float-time (time-since T))))
            (when (> elapsed 0.5)
              (message "org-mem-roamy--update-db: SQL DELETE took %.2fs"
                       elapsed)))
          (when newly-parsed-files
            (org-mem-roamy--populate-db-usably-for-emacsql
             db
             (org-mem-roamy--mk-rows newly-parsed-files))))))))


;;; Translators

(declare-function org-roam-node-create "ext:org-roam-node")
(declare-function org-roam-node-id "ext:org-roam-node")
(declare-function org-roam-reflink-create "ext:org-roam-mode")
(declare-function org-roam-backlink-create "ext:org-roam-mode")

(defun org-mem-roamy-mk-node (entry)
  "Make an org-roam-node object, from org-mem object ENTRY."
  (require 'org-roam-node)
  (unless (org-mem-entry-id entry)
    (error "org-mem-roamy-mk-node: An ID-less entry cannot make an org-roam-node: %s"
           entry))
  (org-roam-node-create
   :file       (org-mem-entry-file entry)
   :file-mtime (org-mem-file-mtime entry)
   :file-title (org-mem-file-title-strict entry)
   :id         (org-mem-entry-id entry)
   :scheduled  (org-mem-entry-scheduled entry)
   :deadline   (org-mem-entry-deadline entry)
   :level      (org-mem-entry-level entry)
   :title      (org-mem-entry-title entry)
   :tags       (org-mem-entry-tags entry)
   :aliases    (org-mem-entry-roam-aliases entry)
   :todo       (org-mem-entry-todo-state entry)
   :refs       (org-mem-entry-roam-refs entry)
   :point      (org-mem-entry-pos entry)
   :priority   (org-mem-entry-priority entry)
   :properties (org-mem-entry-properties entry)
   :olp        (org-mem-entry-olpath entry)))

(defun org-mem-roamy-mk-backlinks (target-roam-node &rest _)
  "Make `org-roam-backlink' objects pointing to TARGET-ROAM-NODE."
  (require 'org-roam-mode)
  (require 'org-roam-node)
  (let* ((target-id (org-roam-node-id target-roam-node))
         (links (gethash target-id org-mem--target<>links)))
    (cl-loop
     for link in links
     as src-id = (org-mem-link-nearby-id link)
     as src-entry = (gethash src-id org-mem--id<>entry)
     when src-entry
     collect (org-roam-backlink-create
              :target-node target-roam-node
              :source-node (org-mem-roamy-mk-node src-entry)
              :point (org-mem-link-pos link)))))

(defun org-mem-roamy-mk-reflinks (target-roam-node &rest _)
  "Make `org-roam-reflink' objects pointing to TARGET-ROAM-NODE."
  (require 'org-roam-mode)
  (require 'org-roam-node)
  (let* ((target-id (org-roam-node-id target-roam-node))
         (entry (gethash target-id org-mem--id<>entry)))
    (when entry
      (cl-loop
       for ref in (org-mem-entry-roam-refs entry)
       append (cl-loop
               for link in (gethash ref org-mem--target<>links)
               as src-id = (org-mem-link-nearby-id link)
               as src-entry = (gethash src-id org-mem--id<>entry)
               when src-entry
               collect (org-roam-reflink-create
                        :ref (org-mem-link-target link)
                        :source-node (org-mem-roamy-mk-node src-entry)
                        :point (org-mem-link-pos link)))))))

(provide 'org-mem-roamy)

;;; org-mem-roamy.el ends here
