;;; org-mem-list.el --- Bonus commands -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Bonus commands for exploring the org-mem data.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-mem)
(require 'sqlite)
(require 'llama)
(require 'sqlite-mode)
(require 'eieio)
(eieio-declare-slots handle)
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
(cl-defun org-mem-list--pop-to-tabulated-buffer (&key buffer format entries reverter)
  "Create, populate and display a `tabulated-list-mode' buffer.

BUFFER is a buffer or buffer name to use.
FORMAT is the value to which `tabulated-list-format' should be set.
ENTRIES is the value to which `tabulated-list-entries' should be set.

Optional argument REVERTER is a function to add buffer-locally to
`tabulated-list-revert-hook'."
  (unless (and buffer format)
    (error "org-mem-list--pop-to-tabulated-buffer: Mandatory arguments are BUFFER, FORMAT, ENTRIES"))
  (when (null entries)
    (message "No entries to tabulate"))
  (pop-to-buffer (get-buffer-create buffer))
  (tabulated-list-mode)
  (setq tabulated-list-format format)
  (tabulated-list-init-header)
  (setq tabulated-list-entries entries)
  (when reverter (add-hook 'tabulated-list-revert-hook reverter nil t))
  (tabulated-list-print t))


;;;###autoload
(defun org-mem-list-entries ()
  (interactive)
  (if (fboundp 'inspector-inspect)
      (inspector-inspect (org-mem-all-entries))
    (message "Command org-mem-list-entries depends on package \"inspector\"")))

;;;###autoload
(defun org-mem-list-entries-in-file ()
  (interactive)
  (if (fboundp 'inspector-inspect)
      (inspector-inspect (org-mem-entries-in-file (buffer-file-name (buffer-base-buffer))))
    (message "Command org-mem-list-entries-in-file depends on package \"inspector\"")))

;;;###autoload
(defun org-mem-list-links ()
  (interactive)
  (if (fboundp 'inspector-inspect)
      (inspector-inspect (org-mem-all-links))
    (message "Command org-mem-list-entries depends on package \"inspector\"")))

;;;###autoload
(defun org-mem-list-links-in-file ()
  (interactive)
  (if (fboundp 'inspector-inspect)
      (inspector-inspect (org-mem-links-in-file (buffer-file-name (buffer-base-buffer))))
    (message "Command org-mem-list-entries depends on package \"inspector\"")))

;;;###autoload
(defun org-mem-list-problems ()
  "List problems encountered while parsing."
  (interactive)
  (if org-mem--problems
      (org-mem-list--pop-to-tabulated-buffer
       :buffer "*org-mem scan problems*"
       :format [("Time" 6 t) ("Scan choked near position" 27 t) ("Pos" 6 t) ("Issue" 0 t)]
       :reverter #'org-mem-list-problems
       :entries
       (cl-loop
        for (time file pos signal lnum) in org-mem--problems collect
        (list (sxhash (cons file pos))
              (vector time
                      (buttonize (format "%s:%d:%d"
                                         (file-name-nondirectory file)
                                         lnum
                                         pos)
                                 #'org-mem-list--goto-file-pos
                                 (cons file pos))
                      ;; In case link got truncated so pos isn't visible.
                      ;; The tabulated-list library definitely has limitations.
                      (number-to-string pos)
                      (format "%S" signal)))))
    (message "Congratulations, no problems scanning %d entries in %d files!"
             (length (org-mem-all-entries))
             (hash-table-count org-mem--truename<>metadata))))

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
    (message "Congratulations, no title collisions! (among %d titles and aliases)"
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
        as entry = (org-mem-link-entry link)
        collect
        (list (sxhash link)
              (vector target
                      (buttonize (org-mem-entry-title entry)
                                 #'org-mem-list--goto-file-pos
                                 (cons (org-mem-entry-file entry)
                                       (org-mem-link-pos link))))))))))

;; TODO: Maybe generalize: track a list of open DBs and explore any of them
;;;###autoload
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


(defvar org-mem--elisp-scratch :never-nil)
(defun org-mem--prin1-to-fontified (value &optional is-raw-string)
  (cl-assert (eq (current-buffer) org-mem--elisp-scratch))
  (erase-buffer)
  (if is-raw-string (insert value)
    (prin1 value (current-buffer) '((length . 50) (level . 2))))
  (font-lock-fontify-region (point-min) (point-max))
  (buffer-string))

;;;###autoload
(defun org-mem-list-example (&optional entry)
  "Display data from ENTRY, or a random member of `org-mem-all-entries'.
See also command `org-mem-entry-at-point'."
  (interactive)
  (let ((entry (or entry (seq-random-elt (org-mem-all-entries))))
        (entry-funs '(org-mem-entry-active-timestamps
                      org-mem-entry-active-timestamps-int
                      org-mem-entry-children
                      org-mem-entry-clocks
                      org-mem-entry-clocks-int
                      org-mem-entry-closed
                      org-mem-entry-closed-int
                      org-mem-entry-dangling-clocks
                      org-mem-entry-deadline
                      org-mem-entry-deadline-int
                      org-mem-entry-file
                      org-mem-entry-file-truename
                      org-mem-entry-id
                      org-mem-entry-keywords
                      org-mem-entry-level
                      org-mem-entry-lnum
                      org-mem-entry-olpath
                      org-mem-entry-olpath-with-file-title
                      org-mem-entry-olpath-with-file-title-or-basename
                      org-mem-entry-olpath-with-self
                      org-mem-entry-olpath-with-self-with-file-title
                      org-mem-entry-olpath-with-self-with-file-title-or-basename
                      org-mem-entry-pos
                      org-mem-entry-priority
                      org-mem-entry-properties-inherited
                      org-mem-entry-properties-local
                      org-mem-entry-pseudo-id
                      org-mem-entry-roam-aliases
                      org-mem-entry-roam-refs
                      org-mem-entry-scheduled
                      org-mem-entry-scheduled-int
                      org-mem-entry-stats-cookies
                      org-mem-entry-subtree-p
                      org-mem-entry-tags
                      org-mem-entry-tags-inherited
                      org-mem-entry-tags-local
                      org-mem-entry-text
                      org-mem-entry-title
                      org-mem-entry-title-maybe
                      org-mem-entry-todo-state
                      org-mem-id-links-to-entry
                      org-mem-links-in-entry
                      org-mem-next-entry
                      org-mem-previous-entry
                      org-mem-roam-reflinks-to-entry))
        (file-funs '(org-mem-entries-in-file
                     org-mem-file-attributes
                     org-mem-file-char-count
                     org-mem-file-coding-system
                     org-mem-file-id-strict
                     org-mem-file-id-topmost
                     org-mem-file-keywords
                     org-mem-file-known-p
                     org-mem-file-line-count
                     org-mem-file-mtime
                     org-mem-file-mtime-floor
                     org-mem-file-ptmax
                     org-mem-file-size
                     org-mem-file-title-or-basename
                     org-mem-file-title-strict
                     org-mem-file-title-topmost)))

    (pop-to-buffer (get-buffer-create "*org-mem example*" t))
    ;; (window-tool-bar-mode)
    ;; (keymap-set tool-bar-map )
    (setq-local buffer-read-only t)
    (setq-local revert-buffer-function (lambda (&rest _) (org-mem-list-example)))
    (keymap-local-set "g" #'revert-buffer-quick)
    (keymap-local-set "q" #'quit-window)
    (keymap-local-set "n" (lambda ()
                            (interactive)
                            (when (org-mem-next-entry entry)
                              (org-mem-list-example (org-mem-next-entry entry)))))
    (keymap-local-set "p" (lambda ()
                            (interactive)
                            (when (org-mem-previous-entry entry)
                              (org-mem-list-example (org-mem-previous-entry entry)))))
    (let ((buffer-read-only nil)
          (win-start-line (line-number-at-pos (window-start)))
          (win-line (line-number-at-pos)))
      (erase-buffer)
      (insert "Example data taken from entry titled \""
              (org-mem-entry-title entry) "\"\n"
              "Type g for a random entry.\n"
              "Type n or p for next/previous entry in same file.\n\n")
      ;; Use a persistent "temp buffer" so we can really spam this command.
      (unless (buffer-live-p org-mem--elisp-scratch)
        (setq org-mem--elisp-scratch (get-buffer-create " *org-mem-elisp-scratch*" t))
        (with-current-buffer org-mem--elisp-scratch
          (emacs-lisp-mode))
        (run-with-idle-timer 5 nil #'kill-buffer org-mem--elisp-scratch))
      (mapc #'insert
            (with-current-buffer org-mem--elisp-scratch
              (cl-loop
               for func in entry-funs
               nconc (list (string-pad (concat "(" (propertize (symbol-name func)
                                                               'face 'font-lock-function-call-face)
                                               " ENTRY)")
                                       67)
                           " => "
                           (org-mem--prin1-to-fontified (funcall func entry))
                           "\n"))))
      (newline)
      (mapc #'insert
            (with-current-buffer org-mem--elisp-scratch
              (cl-loop
               with file = (org-mem-entry-file entry)
               for func in file-funs
               nconc (list (string-pad (concat
                                        "(" (propertize (symbol-name func) 'face 'font-lock-function-call-face)
                                        " (" (propertize "org-mem-entry-file" 'face 'font-lock-function-call-face)
                                        " ENTRY))")
                                       60)
                           " => "
                           (org-mem--prin1-to-fontified (funcall func file))
                           "\n"))))
      ;; Restore scroll position
      (goto-char (point-min))
      (forward-line (- win-start-line 1))
      (set-window-start (selected-window) (point))
      (forward-line (- win-line win-start-line)))))

(provide 'org-mem-list)

;;; org-mem-list.el ends here
