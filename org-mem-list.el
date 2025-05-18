;;; org-mem-list.el --- Bonus commands -*- lexical-binding: t; -*-

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

;; Bonus commands for exploring the org-mem data.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-mem)
(require 'sqlite)
(require 'llama)
(require 'sqlite-mode)
(declare-function eieio-oref "eieio-core")
(declare-function org-mem-roamy-db "org-mem-roamy")
(defvar org-mem-db1--connection)
(defvar org-mem-roamy-db-mode)

(defun org-mem-list--goto-file-pos (file.pos)
  "Given cons cell FILE.POS, visit FILE \(car\) and go to POS \(cdr\)."
  (find-file (car file.pos))
  (goto-char (cdr file.pos)))

(defun org-mem-list--goto-id (id)
  "Go to ID."
  (let ((entry (org-mem-entry-by-id id)))
    (find-file (org-mem-entry-file entry))
    (goto-char (org-mem-entry-pos entry))))

;;;###autoload
(defun org-mem-list-problems ()
  "List problems encountered while parsing."
  (interactive)
  (if org-mem--problems
      (org-mem-list--pop-to-tabulated-buffer
       :buffer "*org-mem scan problems*"
       :format [("Time" 6 t) ("Scan choked near position" 27 t) ("Issue" 0 t)]
       :reverter #'org-mem-list-problems
       :entries
       (cl-loop
        for ( time file pos signal ) in org-mem--problems collect
        (list (sxhash (cons file pos))
              (vector time
                      (buttonize (format "%s:%d" (file-name-nondirectory file)
                                         pos)
                                 #'org-mem-list--goto-file-pos
                                 (cons file pos))
                      (format "%s" signal)))))
    (message "Congratulations, no problems scanning %d entries in %d files!"
             (length (org-mem-all-entries))
             (hash-table-count org-mem--file<>metadata))))

;;;###autoload
(defun org-mem-list-title-collisions ()
  "Pop up a buffer displaying title collisions between ID-nodes.
To automatically warn, set `org-mem-do-warn-title-collisions'."
  (interactive)
  (if org-mem--title-collisions
      (org-mem-list--pop-to-tabulated-buffer
       :buffer "*org-mem title collisions*"
       :format [("Time" 6 t) ("Shared name" 30 t) ("ID" 37 t) ("Other ID" 0 t)]
       :reverter #'org-mem-list-title-collisions
       :entries
       (cl-loop
        for row in org-mem--title-collisions
        collect (seq-let ( time name id1 id2 ) row
                  (list
                   (sxhash row)
                   (vector time
                           name
                           (buttonize id1 #'org-mem-list--goto-id id1)
                           (buttonize id2 #'org-mem-list--goto-id id2))))))
    (message "Congratulations, no title collisions! (among %d ID-nodes)"
             (hash-table-count org-mem--title<>id))))

;;;###autoload
(defun org-mem-list-dead-id-links ()
  "List links that lead to no known ID."
  (interactive)
  (let* ((dead-links
          (cl-loop for target being each hash-key of org-mem--target<>links
                   using (hash-values links)
                   unless (gethash target org-mem--id<>entry)
                   append (cl-loop for link in links
                                   when (equal "id" (org-mem-link-type link))
                                   collect (cons target link))))
         (longest
          (cl-loop for (_ . link) in dead-links
                   maximize (length (org-mem-link-target link)))))
    (message "%d dead links found" (length dead-links))
    (when dead-links
      (org-mem-list--pop-to-tabulated-buffer
       :buffer "*org-mem dead links*"
       :format `[("Unknown ID reference" ,(1+ longest) t) ("Location" 0 t)]
       :reverter #'org-mem-list-dead-id-links
       :entries
       (cl-loop
        for (target . link) in dead-links
        as origin-id = (org-mem-link-nearby-id link)
        as entry = (org-mem-entry-by-id origin-id)
        collect
        (list (sxhash link)
              (vector target
                      (buttonize (org-mem-entry-title entry)
                                 #'org-mem-list--goto-file-pos
                                 (cons (org-mem-entry-file entry)
                                       (org-mem-link-pos link))))))))))

;; TODO: Maybe generalize: track a list of open DBs and explore any of them
(defun org-mem-list-db-contents (&optional db)
  "Explore contents of currently used SQLite DB.

With optional argument DB, explore that database connection
instead of default `org-mem-roamy--connection'."
  (interactive)
  (require 'org-mem-roamy)
  (require 'eieio)
  (cl-assert (sqlite-available-p))
  ;; this is way too roundabout
  (let (dbs)
    (and (bound-and-true-p org-mem-db1--connection)
         (ignore-errors
           (sqlite-pragma org-mem-db1--connection "hi"))
         (push org-mem-db1--connection dbs))
    (and org-mem-roamy-db-mode
         (ignore-errors
           (sqlite-pragma (eieio-oref (org-mem-roamy-db) 'handle) "hi"))
         (push (eieio-oref (org-mem-roamy-db) 'handle) dbs))
    (let ((db (cond (db)
                    ((length> dbs 1)
                     (if (equal "org" (read-answer
                                       "List [r]oam or [o]rgdb contents?"
                                       '(("roam" ?r "org-mem-roamy")
                                         ("org" ?o "org-mem-db1"))))
                         (nth 1 dbs)
                       (nth 0 dbs)))
                    ((car dbs)))))
      (if db
          (progn
            (pop-to-buffer
             (get-buffer-create (format "*SQLite %.50s*" (prin1-to-string db))))
            (sqlite-mode)
            ;; (when (stringp db) (setq-local default-directory (file-name-directory db)))
            (setq-local sqlite--db db)
            (sqlite-mode-list-tables))
        (message "No DB yet")))))

;; TODO: Could be way more detailed.
;; Like (inspector-inspect (org-mem-all-entries))
;;;###autoload
(defun org-mem-list-entries ()
  "List all Org entries."
  (interactive)
  (org-mem-list--pop-to-tabulated-buffer
   :buffer "*org-mem entries*"
   :format [("Entry" 30 t) ("File" 30 t) ("Outline path" 0 t)]
   :reverter #'org-mem-list-entries
   :entries
   (cl-loop
    for entry in (org-mem-all-entries)
    collect
    (list (sxhash entry)
          (vector (buttonize (org-mem-entry-title entry)
                             #'org-mem-list--goto-file-pos
                             (cons (org-mem-entry-file entry)
                                   (org-mem-entry-pos entry)))
                  (file-name-nondirectory (org-mem-entry-file entry))
                  (string-join (org-mem-entry-olpath entry) " > "))))))

(cl-defun org-mem-list--pop-to-tabulated-buffer (&key buffer format entries reverter)
  "Create, populate and display a `tabulated-list-mode' buffer.

BUFFER is a buffer or buffer name where the list should be created.
FORMAT is the value to which `tabulated-list-format' should be set.
ENTRIES is the value to which `tabulated-list-entries' should be set.

Optional argument REVERTER is a function to add buffer-locally to
`tabulated-list-revert-hook'."
  (unless (and buffer format)
    (user-error
     "org-mem-list--pop-to-tabulated-buffer: Mandatory arguments are buffer, format, entries"))
  (when (null entries)
    (message "No entries to tabulate"))
  (pop-to-buffer (get-buffer-create buffer))
  (tabulated-list-mode)
  (setq tabulated-list-format format)
  (tabulated-list-init-header)
  (setq tabulated-list-entries entries)
  (when reverter (add-hook 'tabulated-list-revert-hook reverter nil t))
  (tabulated-list-print t))

(provide 'org-mem-list)

;;; org-mem-list.el ends here
