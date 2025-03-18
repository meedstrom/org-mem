;;; indexed-list.el --- Bonus commands -*- lexical-binding: t; -*-
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

;; Bonus commands for exploring the indexed data.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'indexed)
(require 'sqlite)
(require 'sqlite-mode)
(declare-function indexed-roam "indexed-roam")

(defun indexed-list--goto-file-pos (file.pos)
  "Go to FILE at POS."
  (find-file (car file.pos))
  (goto-char (cdr file.pos)))

(defun indexed-list--goto-id (id)
  "Go to ID."
  (let ((entry (indexed-entry-by-id id)))
    (find-file (indexed-file-name entry))
    (goto-char (indexed-pos entry))))

;;;###autoload
(defun indexed-list-problems ()
  "List problems encountered while parsing."
  (interactive)
  (if indexed--problems
      (indexed-list--pop-to-tabulated-buffer
       :buffer "*indexing problems*"
       :format [("Time" 6 t) ("Scan choked near position" 27 t) ("Issue" 0 t)]
       :reverter #'indexed-list-problems
       :entries
       (cl-loop
        for ( time file pos signal ) in indexed--problems collect
        (list (sxhash (cons file pos))
              (vector time
                      (buttonize (format "%s:%d" (file-name-nondirectory file)
                                         pos)
                                 #'indexed-list--goto-file-pos
                                 (cons file pos))
                      (format "%s" signal)))))
    (message "Congratulations, no problems scanning %d entries in %d files!"
             (length (indexed-org-entries))
             (hash-table-count indexed--file<>data))))

;;;###autoload
(defun indexed-list-title-collisions ()
  "Pop up a buffer listing title collisions between org-ID nodes."
  (interactive)
  (if indexed--title-collisions
      (indexed-list--pop-to-tabulated-buffer
       :buffer "*title collisions*"
       :format [("Time" 6 t) ("Shared name" 30 t) ("ID" 37 t) ("Other ID" 0 t)]
       :reverter #'indexed-list-title-collisions
       :entries
       (cl-loop
        for row in indexed--title-collisions
        collect (seq-let ( time name id1 id2 ) row
                  (list
                   (sxhash row)
                   (vector time
                           name
                           (buttonize id1 #'indexed-list--goto-id id1)
                           (buttonize id2 #'indexed-list--goto-id id2))))))
    (message "Congratulations, no title collisions! (among %d ID-nodes)"
             (hash-table-count indexed--title<>id))))

;;;###autoload
(defun indexed-list-id-collisions ()
  "Pop up a buffer listing title collisions between org-ID nodes."
  (interactive)
  (if indexed--id-collisions
      (indexed-list--pop-to-tabulated-buffer
       :buffer "*ID collisions*"
       :format [("Time" 6 t) ("ID" 37 t) ("Title 1" 25 t) ("Title 2" 0 t)]
       :reverter #'indexed-list-title-collisions
       :entries
       (cl-loop
        for row in indexed--id-collisions
        collect (seq-let ( time id name1 name2 ) row
                  (list
                   (sxhash row)
                   (vector time
                           (buttonize id #'indexed-list--goto-id id)
                           name1
                           name2)))))
    (message "Congratulations, no ID collisions! (among %d titles)"
             (hash-table-count indexed--title<>id))))

;;;###autoload
(defun indexed-list-dead-id-links ()
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
      (indexed-list--pop-to-tabulated-buffer
       :buffer "*dead links*"
       :format [("Location" 40 t) ("Unknown ID reference" 40 t)]
       :reverter #'indexed-list-dead-id-links
       :entries
       (cl-loop
        for (dest . link) in dead-links
        as origin-id = (indexed-origin link)
        as entry = (gethash origin-id indexed--id<>entry)
        collect
        (list (sxhash link)
              (vector (buttonize (indexed-title entry)
                                 #'indexed-list--goto-file-pos
                                 (cons (indexed-file-name entry)
                                       (indexed-pos link))
                                 dest))))))))

;; TODO: Maybe generalize: track a list of open DBs and explore any of them
(defun indexed-list-db-contents (&optional db)
  "Explore contents of currently used SQLite DB.

With optional argument DB, explore that database connection
instead of default `indexed-roam--connection'."
  (interactive)
  (require 'indexed-roam)
  (cl-assert (sqlite-available-p))
  (let ((db (or db (indexed-roam))))
    (unless (ignore-errors (sqlite-pragma db "im_still_here"))
      (error "indexed-roam-show-contents: Not a live DB connection: %s" db))
    (pop-to-buffer
     (get-buffer-create (format "*SQLite %.50s*" (prin1-to-string db))))
    (sqlite-mode)
    (setq-local sqlite--db db)
    (sqlite-mode-list-tables)))

;; TODO: Could be way more detailed.
;;;###autoload
(defun indexed-list-entries ()
  "List all Org entries."
  (interactive)
  (indexed-list--pop-to-tabulated-buffer
   :buffer "*all Org entries*"
   :format [("Entry" 30 t) ("File" 30 t) ("Outline path" 0 t)]
   :reverter #'indexed-list-entries
   :entries
   (cl-loop
    for entry in (indexed-org-entries)
    collect
    (list (sxhash entry)
          (vector (buttonize (indexed-title entry)
                             #'indexed-list--goto-file-pos
                             (cons (indexed-file-name entry)
                                   (indexed-pos entry)))
                  (file-name-nondirectory (indexed-file-name entry))
                  (string-join (indexed-olpath entry) " > "))))))

;; so i dont have to remember the boilerplate nor update in many places
(cl-defun indexed-list--pop-to-tabulated-buffer (&key buffer format entries reverter)
  "Create, populate and display a `tabulated-list-mode' buffer.

BUFFER is a buffer or buffer name where the list should be created.
FORMAT is the value to which `tabulated-list-format' should be set.
ENTRIES is the value to which `tabulated-list-entries' should be set.

Optional argument REVERTER is a function to add buffer-locally to
`tabulated-list-revert-hook'."
  (unless (and buffer format)
    (user-error
     "indexed-list--pop-to-tabulated-buffer: Mandatory arguments are buffer, format, entries"))
  (when (null entries)
    (message "No entries to tabulate"))
  (pop-to-buffer (get-buffer-create buffer))
  (tabulated-list-mode)
  (setq tabulated-list-format format)
  (tabulated-list-init-header)
  (setq tabulated-list-entries entries)
  (when reverter (add-hook 'tabulated-list-revert-hook reverter nil t))
  (tabulated-list-print t))

(provide 'indexed-list)

;;; indexed-list.el ends here
