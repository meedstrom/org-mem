;;; indexed-roam.el --- Make data like org-roam does -*- lexical-binding: t; -*-

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

;; A submodule with two purposes

;; 1. make Indexed aware of ROAM_ALIASES and ROAM_REFS
;; 2. make a SQL database

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'indexed)
(require 'sqlite)
(require 'emacsql)

;;;###autoload
(define-minor-mode indexed-roam-mode
  "Add awareness of ROAM_REFS and make the `indexed-roam' DB."
  :global t
  :group 'indexed
  (if indexed-roam-mode
      (progn
        (add-hook 'indexed-record-entry-functions 'indexed-roam--record-aliases-and-refs -5)
        (add-hook 'indexed-forget-entry-functions 'indexed-roam--forget-aliases-and-refs)
        (add-hook 'indexed-post-incremental-update-functions 'indexed-roam--update-db)
        (add-hook 'indexed-post-full-reset-functions 'indexed-roam--re-make-db)
        (indexed--scan-full))
    (remove-hook 'indexed-record-entry-functions 'indexed-roam--record-aliases-and-refs)
    (remove-hook 'indexed-forget-entry-functions 'indexed-roam--forget-aliases-and-refs)
    (remove-hook 'indexed-post-incremental-update-functions 'indexed-roam--update-db)
    (remove-hook 'indexed-post-full-reset-functions 'indexed-roam--re-make-db)))


;;; Aliases and refs support

(defvar indexed-roam--work-buf nil)
(defvar indexed-roam--ref<>id (make-hash-table :test 'equal))
(defvar indexed-roam--id<>refs (make-hash-table :test 'equal))
(defvar indexed-roam--ref<>type (make-hash-table :test 'equal)) ;; REVIEW: weird

;; This one works without the rest
;;;###autoload
(defun indexed-roam-aliases (entry)
  "Property ROAM_ALIASES in ENTRY, properly split."
  (when-let* ((aliases (indexed-property "ROAM_ALIASES" entry)))
    (split-string-and-unquote aliases)))

(defun indexed-roam-refs (entry)
  "Property ROAM_REFS in ENTRY, properly split."
  (if indexed-roam-mode
      (gethash (indexed-id entry) indexed-roam--id<>refs)
    (error "Function `indexed-roam-refs' depends on `indexed-roam-mode'")))

(defun indexed-roam-reflinks-to (entry)
  "All links that point to a member of ENTRY\\='s ROAM_REFS."
  (if indexed-roam-mode
      (cl-loop for ref in (indexed-roam-refs entry)
               append (gethash ref indexed--dest<>links))
    (error "Function `indexed-roam-reflinks-to' depends on `indexed-roam-mode'")))

(defun indexed-roam--wipe-ref-tables (_)
  (clrhash indexed-roam--ref<>id)
  (clrhash indexed-roam--id<>refs))

(defun indexed-roam--record-aliases-and-refs (entry)
  "Add any ENTRY aliases to `indexed--title<>id'."
  (when-let* ((id (indexed-id entry)))
    (dolist (alias (indexed-roam-aliases entry))
      ;; Include aliases in the collision-checks
      (when-let* ((other-id (gethash alias indexed--title<>id)))
        (unless (string= id other-id)
          (push (list (format-time-string "%H:%M") alias id other-id)
                indexed--title-collisions)))
      (puthash alias id indexed--title<>id))
    (when-let* ((refs (indexed-roam-split-refs-field
                       (indexed-property "ROAM_REFS" entry))))
      (puthash id refs indexed-roam--id<>refs)
      (dolist (ref refs)
        (puthash ref id indexed-roam--ref<>id)))))

(defun indexed-roam--forget-aliases-and-refs (entry)
  (dolist (ref (indexed-roam-refs entry))
    (dolist (id (gethash ref indexed-roam--ref<>id))
      (remhash id indexed-roam--id<>refs))
    (remhash ref indexed-roam--ref<>id))
  (dolist (alias (indexed-roam-aliases entry))
    (remhash alias indexed--title<>id)))

;;;###autoload
(defun indexed-roam-split-refs-field (roam-refs)
  "Split a ROAM-REFS field correctly.
What this means?  See indexed-test.el."
  (when roam-refs
    (with-current-buffer (get-buffer-create " *indexed-throwaway*" t)
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
                   path))))))


;;; Database

(defvar indexed-roam--connection nil
  "An EmacSQL connection.")

(defcustom indexed-roam-overwrite nil
  "Whether to overwrite the DB at `org-roam-db-location'.
If nil, write a diskless DB."
  :type 'boolean
  :group 'indexed
  :package-version '(indexed . "0.4.0"))

(defun indexed-roam--re-make-db (&rest _)
  "Close current `indexed-roam--connection' and populate a new one."
  (when (and indexed-roam-overwrite
             (boundp 'org-roam-db-version)
             (> org-roam-db-version 20))
    (display-warning 'indexed-roam "
Org-roam bumped the DB version, and indexed-roam has not caught up.
Setting `indexed-roam-overwrite' to nil for now.
Make up for it with: (setq org-roam-db-update-on-save t)")
    (setq indexed-roam-overwrite nil))
  (if indexed-roam-overwrite
      (and (fboundp 'org-roam-db--close)
           (org-roam-db--close))
    (and (fboundp 'emacsql-close)
         (fboundp 'emacsql-live-p)
         indexed-roam--connection
         (emacsql-live-p indexed-roam--connection)
         (emacsql-close indexed-roam--connection)))
  (indexed-roam))

(defun indexed-roam (&rest deprecated-args)
  "Return an EmacSQL connection.
If `indexed-roam-overwrite' is t, return that of `org-roam-db'.

If passed any DEPRECATED-ARGS, signal an error."
  (when deprecated-args
    (error "Function `indexed-roam' no longer takes arguments"))
  (unless indexed-roam-mode
    (error "Enable `indexed-roam-mode' to use `indexed-roam'"))
  (if (and (require 'emacsql nil t)
           (require 'emacsql-sqlite nil t)
           (fboundp 'emacsql-live-p)
           (fboundp 'emacsql-sqlite-open)
           (fboundp 'emacsql-sqlite-default-connection))
      (if (not (eq (emacsql-sqlite-default-connection)
                   'emacsql-sqlite-builtin-connection))
          (error "`indexed-roam-mode' requires built-in SQLite")
        (let ((T (current-time))
              conn name)

          (if indexed-roam-overwrite
              (if (and (require 'org-roam nil t)
                       (fboundp 'org-roam-db)
                       (fboundp 'org-roam-db--get-connection)
                       (boundp 'org-roam-db-location))
                  (progn
                    (when (and indexed-updater-mode
                               (bound-and-true-p org-roam-db-update-on-save))
                      (error "Both options should not be t: `indexed-roam-overwrite' and `org-roam-db-update-on-save'"))
                    (setq conn (org-roam-db--get-connection))
                    (unless (and conn (emacsql-live-p conn))
                      ;; No live connection, take the chance to repopulate.
                      ;; Note that live connections sometimes get closed by
                      ;; `indexed-roam--re-make-db', such as when you turn on
                      ;; `indexed-roam-mode'.
                      ;; Delete file instead of using `org-roam-db-clear-all',
                      ;; b/c that takes 10 seconds.
                      (cl-assert (file-name-absolute-p org-roam-db-location))
                      (ignore-errors (delete-file org-roam-db-location))
                      (setq conn (org-roam-db))
                      (setq name org-roam-db-location)
                      (indexed-roam--populate-usably-for-emacsql
                       (oref conn handle) (indexed-roam--mk-rows))))
                (error "Option `indexed-roam-overwrite' t, but org-roam unavailable"))
            ;; Option `indexed-roam-overwrite' nil; make own DB and connection.
            (setq conn indexed-roam--connection)
            (unless (and conn (emacsql-live-p conn))
              (setq conn (emacsql-sqlite-open nil))
              (setq indexed-roam--connection conn)
              (indexed-roam--configure (oref conn handle))
              (indexed-roam--populate-usably-for-emacsql
               (oref conn handle) (indexed-roam--mk-rows))))

          (when indexed--next-message
            ;; Print a benchmark if called by command `indexed-reset'.
            (setq indexed--next-message
                  (concat indexed--next-message
                          (format " + %.2fs writing %s"
                                  (float-time (time-since T))
                                  (or name "SQLite DB")))))
          conn))
    (error "`indexed-roam' requires \"emacsql\"")))

(defun indexed-roam--configure (db)
  "Set up tables, schemata and PRAGMA settings in DB."
  (sqlite-execute db "PRAGMA user_version = 20;")
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

  ;; That's it for compat with org-roam db.
  ;; Now play with perf settings.
  ;; https://www.sqlite.org/pragma.html
  ;; https://www.sqlite.org/inmemorydb.html

  (sqlite-execute db "CREATE INDEX refs_node_id  ON refs    (node_id);")
  (sqlite-execute db "CREATE INDEX tags_node_id  ON tags    (node_id);")
  (sqlite-execute db "CREATE INDEX alias_node_id ON aliases (node_id);")
  (sqlite-execute db "PRAGMA cache_size = -40000;") ;; 40,960,000 bytes

  ;; Full disclosure, I have no idea what I'm doing
  (sqlite-execute db "PRAGMA mmap_size = 81920000;")
  (sqlite-execute db "PRAGMA temp_store = memory;")
  (sqlite-execute db "PRAGMA synchronous = off;")
  db)

(defun indexed-roam--populate-usably-for-emacsql (db row-sets)
  (seq-let (files nodes aliases citations refs tags links) row-sets
    (with-sqlite-transaction db
      (when files
        (sqlite-execute
         db (concat
             "INSERT INTO files VALUES "
             (indexed-roam--mk-singular-value-quoted-like-emacsql files))))
      (when nodes
        (sqlite-execute
         db (concat
             "INSERT INTO nodes VALUES "
             (indexed-roam--mk-singular-value-quoted-like-emacsql nodes))))
      (when aliases
        (sqlite-execute
         db (concat
             "INSERT INTO aliases VALUES "
             (indexed-roam--mk-singular-value-quoted-like-emacsql aliases))))
      (when citations
        (sqlite-execute
         db (concat
             "INSERT INTO citations VALUES "
             (indexed-roam--mk-singular-value-quoted-like-emacsql citations))))
      (when refs
        (sqlite-execute
         db (concat
             "INSERT INTO refs VALUES "
             (indexed-roam--mk-singular-value-quoted-like-emacsql refs))))
      (when tags
        (sqlite-execute
         db (concat
             "INSERT INTO tags VALUES "
             (indexed-roam--mk-singular-value-quoted-like-emacsql tags))))
      (when links
        (sqlite-execute
         db (concat
             "INSERT INTO links VALUES "
             (indexed-roam--mk-singular-value-quoted-like-emacsql links)))))))

(defun indexed-roam--mk-singular-value-quoted-like-emacsql (rows)
  "Turn ROWS into literal \(not prepared) value for a SQL INSERT.
In each row, print atoms that are strings or lists, readably."
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

(defun indexed-roam--mk-rows (&optional specific-files)
  "Return rows of data suitable for inserting into `indexed-roam' DB.

Specifically, return lists of rows, one for each SQL table
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
        (roam-dir (when (boundp 'org-roam-directory)
                    (abbreviate-file-name (file-truename org-roam-directory)))))
    (cl-loop
     with seen-files = (make-hash-table :test 'equal)
     for entry in (indexed-org-id-nodes)
     as file = (indexed-file-name entry)
     as id = (indexed-id entry)
     unless (and specific-files (not (member file specific-files)))
     unless (and roam-dir (not (string-prefix-p roam-dir file)))
     do
     (unless (gethash file seen-files)
       (puthash file t seen-files)
       (push (indexed-roam--mk-file-row file) file-rows))

     ;; See `org-roam-db-insert-aliases'
     (cl-loop for alias in (indexed-roam-aliases entry) do
              (push (list id alias) alias-rows))
     ;; See `org-roam-db-insert-tags'
     (cl-loop for tag in (indexed-tags entry) do
              (push (list id tag) tag-rows))
     ;; See `org-roam-db-insert-file-node' and `org-roam-db-insert-node-data'
     (push (list id
                 (indexed-file-name entry)
                 (indexed-heading-lvl entry)
                 (indexed-pos entry)
                 (indexed-todo-state entry)
                 (indexed-priority entry)
                 (indexed-scheduled entry)
                 (indexed-deadline entry)
                 (indexed-title entry)
                 (indexed-properties entry)
                 (indexed-olpath entry))
           node-rows)
     ;; See `org-roam-db-insert-refs'
     (cl-loop for ref in (indexed-roam-refs entry) do
              (let ((type (gethash ref indexed-roam--ref<>type)))
                (push (list id
                            ref
                            (or type "cite"))
                      ref-rows))))

    (let ((dummy-props '(:outline nil)))
      (cl-loop
       for link in (indexed-org-links)
       as file = (indexed-org-link-file-name link)
       when (indexed-nearby-id link)
       unless (and specific-files (not (member file specific-files)))
       unless (and roam-dir (not (string-prefix-p roam-dir file)))
       do (if (indexed-type link)
              ;; See `org-roam-db-insert-link'
              (push (list (indexed-pos link)
                          (indexed-nearby-id link)
                          (indexed-dest link)
                          (indexed-type link)
                          dummy-props)
                    link-rows)
            ;; See `org-roam-db-insert-citation'
            (push (list (indexed-origin link)
                        (substring (indexed-dest link) 1)
                        (indexed-pos link)
                        dummy-props)
                  citation-rows))))

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
;; So, we skip the overhead of `prin1-to-string' and just store integer mtime,
;; unlike org-roam, which stores lists.

(defun indexed-roam--mk-file-row (file)
  "Return a row representing FILE for the files-table."
  (let ((data (indexed-file-data file)))
    ;; See `org-roam-db-insert-file'
    (list file
          (indexed-file-title data)
          ""                        ; HACK: Hashing is slow, skip
          (indexed-file-mtime data) ; HACK: org-roam doesn't use atime anyway
          (indexed-file-mtime data))))


;;; Update-on-save

(defun indexed-roam--update-db (parse-results)
  "Update current DB about nodes and links from PARSE-RESULTS.
Suitable on `indexed-post-incremental-update-functions'."
  ;; NOTE: There's a likely performance bug in Emacs sqlite.c.
  ;;       I have a yuge file, which takes 0.01 seconds to delete on the
  ;;       sqlite3 command line... but 0.53 seconds with `sqlite-execute'.
  ;;
  ;;       Aside from tracking down the bug, could we workaround by getting rid
  ;;       of all the CASCADE rules and pre-determine what needs to be deleted?
  ;;       It's not The Way to use a RDBMS, but it's a simple enough puzzle.
  (let* ((db (oref (indexed-roam) handle))
         (files (mapcar #'indexed-file-name (nth 1 parse-results)))
         (rows (indexed-roam--mk-rows files)))
    (dolist (file files)
      (sqlite-execute db "DELETE FROM files WHERE file LIKE ?;"
                      (list (prin1-to-string file))))
    (indexed-roam--populate-usably-for-emacsql db rows)))


;;; Dev tools

(defvar emacsql-type-map)
(defun indexed-roam--insert-schemata-atpt ()
  "Print `org-roam-db--table-schemata' as raw SQL at point."
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


;;; Bonus utilities

;; If saving buffers is slow with org-roam.  Stop updating org-roam.db on save,
;; and use this shim to let your *org-roam* buffer be up to date anyway.
;; Setup:

;; (setq org-roam-db-update-on-save nil) ;; if saving is slow
;; (indexed-updater-mode)
;; (indexed-roam-mode)
;; (advice-add 'org-roam-backlinks-get :override #'indexed-roam-mk-backlinks)
;; (advice-add 'org-roam-reflinks-get  :override #'indexed-roam-mk-reflinks)

(declare-function org-roam-node-create "org-roam-node")
(declare-function org-roam-node-id "org-roam-node")
(declare-function org-roam-reflink-create "org-roam-mode")
(declare-function org-roam-backlink-create "org-roam-mode")

(defun indexed-roam-mk-node (entry)
  "Make an org-roam-node object, from indexed object ENTRY."
  (require 'org-roam-node)
  (unless (indexed-id entry)
    (error "indexed-roam-mk-node: An ID-less entry cannot make an org-roam-node: %s"
           entry))
  (org-roam-node-create
   :file (indexed-file-name entry)
   :file-mtime (indexed-file-mtime entry)
   :file-title (indexed-file-title entry)
   :id (indexed-id entry)
   :scheduled (indexed-scheduled entry)
   :deadline (indexed-deadline entry)
   :level (indexed-heading-lvl entry)
   :title (indexed-title entry)
   :tags (indexed-tags entry)
   :aliases (indexed-roam-aliases entry)
   :todo (indexed-todo-state entry)
   :refs (indexed-roam-refs entry)
   :point (indexed-pos entry)
   :priority (indexed-priority entry)
   :properties (indexed-properties entry)
   :olp (indexed-olpath entry)))

(defun indexed-roam-mk-backlinks (target-roam-node &rest _)
  "Make `org-roam-backlink' objects pointing to TARGET-ROAM-NODE.

Can be used in two ways:
- As override-advice for `org-roam-backlinks-get'.
- Directly, if TARGET-ROAM-NODE is an output of `indexed-roam-mk-node'."
  (require 'org-roam-mode)
  (require 'org-roam-node)
  (let* ((target-id (org-roam-node-id target-roam-node))
         (links (gethash target-id indexed--dest<>links)))
    (cl-loop
     for link in links
     as src-id = (indexed-origin link)
     as src-entry = (gethash src-id indexed--id<>entry)
     when src-entry
     collect (org-roam-backlink-create
              :target-node target-roam-node
              :source-node (indexed-roam-mk-node src-entry)
              :point (indexed-pos link)))))

;; REVIEW:  Are our refs exactly the same as org-roam's refs?
(defun indexed-roam-mk-reflinks (target-roam-node &rest _)
  "Make `org-roam-reflink' objects pointing to TARGET-ROAM-NODE.

Can be used in two ways:
- As override-advice for `org-roam-reflinks-get'.
- Directly, if TARGET-ROAM-NODE is an output of `indexed-roam-mk-node'."
  (require 'org-roam-mode)
  (require 'org-roam-node)
  (let* ((target-id (org-roam-node-id target-roam-node))
         (entry (gethash target-id indexed--id<>entry)))
    (when entry
      (cl-loop
       for ref in (indexed-roam-refs entry)
       append (cl-loop
               for link in (gethash ref indexed--dest<>links)
               as src-id = (indexed-origin link)
               as src-entry = (gethash src-id indexed--id<>entry)
               when src-entry
               collect (org-roam-reflink-create
                        :ref (indexed-dest link)
                        :source-node (indexed-roam-mk-node src-entry)
                        :point (indexed-pos link)))))))

(provide 'indexed-roam)

;;; indexed-roam.el ends here
