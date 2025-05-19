;;; org-mem-updater.el --- Incremental caching -*- lexical-binding: t; -*-

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

;; Optional mechanisms to update the tables in just-in-time fashion,
;; reducing our need to do `org-mem--full-scan' so often.

;; Technically, repeating a full scan is never needed *IF* we use these hooks
;; correctly.  However, that is hard and humans are fallible.

;; If a full scan is sufficiently performant, you can just do it more often,
;; instead of using these hooks at all.  It is also a simple way to detect
;; filesystem changes made by other Emacsen or the command line.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'llama)
(require 'org-mem)
(require 'org-mem-parser)
(declare-function org-element-property "org-element-ast")
(declare-function org-entry-get-with-inheritance "org")
(declare-function org-get-tags "org")


;;; Targeted-scan

;; NOTE: When setting `delete-by-moving-to-trash' is t, `delete-file' calls
;;       `move-file-to-trash' which calls `rename-file'.  And it appears that
;;       `rename-file' can also call `delete-file'.  Happy coding!

(defun org-mem-updater--handle-rename (file newname &rest _)
  "Arrange to scan NEWNAME for entries and links, and forget FILE."
  (org-mem-updater--handle-delete file)
  (cl-assert newname) ;; b/c below func would accept nil
  (org-mem-updater--handle-save newname))

(defun org-mem-updater--handle-delete (file &optional _trash)
  "Forget entries and links in FILE.

If FILE differs from the name by which the actual file is listed in our
tables, because a parent directory is a symlink or the abbreviation
differs, try to discover the known name variant and operate on that.

However, do not do so when FILE itself satisfies `file-symlink-p'.
In that case, there may be nothing wrong with the known name."
  (when (and (string-suffix-p ".org" file)
             ;; Don't accidentally scrub Tramp files from org-id-locations
             ;; just because we chose to know nothing about them.
             (not (org-mem--tramp-file-p file)))
    (let ((bad (list file))
          (cached-true (gethash file org-mem--wild-filename<>abbr-truename)))
      (when (and cached-true (not (file-symlink-p file)))
        (push cached-true bad))
      (org-mem-updater--forget-file-contents bad)
      (org-mem--invalidate-file-names bad)
      (mapc #'clrhash (hash-table-values org-mem--key<>subtable)))))

(defun org-mem-updater--handle-save (&optional file)
  "Arrange to scan entries and links in FILE, or current buffer file."
  (unless file (setq file buffer-file-truename))
  (when (and (string-suffix-p ".org" file)
             (not (backup-file-name-p file))
             (not (org-mem--tramp-file-p file)))
    (org-mem-updater--scan-targeted file)))

(defun org-mem-updater--scan-targeted (files)
  "Arrange to scan FILES."
  (when files
    (el-job-launch :id 'org-mem-updater
                   :inject-vars (org-mem--mk-work-vars)
                   :load-features '(org-mem-parser)
                   :inputs (ensure-list files)
                   :funcall-per-input #'org-mem-parser--parse-file
                   :callback #'org-mem-updater--finalize-targeted-scan)))

(defun org-mem-updater--finalize-targeted-scan (parse-results _job)
  "Handle PARSE-RESULTS from `org-mem-updater--scan-targeted'."
  (run-hook-with-args 'org-mem-pre-targeted-scan-functions parse-results)
  (seq-let (bad-paths file-data entries links problems) parse-results
    (when bad-paths
      (org-mem-updater--forget-links-from-files bad-paths)
      (org-mem-updater--forget-file-contents bad-paths)
      (org-mem--invalidate-file-names bad-paths))
    (org-mem-updater--forget-links-from-files (mapcar #'car file-data))
    (mapc #'clrhash (hash-table-values org-mem--key<>subtable))
    (with-current-buffer
        (setq org-mem-scratch (get-buffer-create " *org-mem-scratch*" t))
      (dolist (datum file-data)
        (puthash (car datum) datum org-mem--file<>metadata)
        (run-hook-with-args 'org-mem-record-file-functions datum))
      (dolist (entry entries)
        (org-mem--record-entry entry)
        (run-hook-with-args 'org-mem-record-entry-functions entry))
      (dolist (link links)
        (org-mem--record-link link)
        (run-hook-with-args 'org-mem-record-link-functions link)))
    (dolist (prob problems)
      (push prob org-mem--problems))
    (run-hook-with-args 'org-mem-post-targeted-scan-functions parse-results)
    (when bad-paths
      (let ((good-paths (seq-keep #'org-mem--abbr-truename bad-paths)))
        (org-mem-updater--scan-targeted (seq-difference good-paths bad-paths))))
    (when problems
      (message "Scan had problems, see M-x org-mem-list-problems"))))

(defun org-mem-updater--forget-file-contents (files)
  "Delete from tables, most info relating to FILES and their contents.
You should also run `org-mem--invalidate-file-names',
and potentially `org-mem-updater--forget-links-from-files'."
  (setq files (ensure-list files))
  (when files
    (with-current-buffer
        (setq org-mem-scratch (get-buffer-create " *org-mem-scratch*" t))
      (dolist (file files)
        (dolist (entry (gethash file org-mem--file<>entries))
          (remhash (org-mem-entry-id entry) org-mem--id<>entry)
          (remhash (org-mem-entry-title-maybe entry) org-mem--title<>id)
          (run-hook-with-args 'org-mem-forget-entry-functions entry))
        (remhash file org-mem--file<>entries)
        (remhash file org-mem--file<>metadata)
        (run-hook-with-args 'org-mem-forget-file-functions file)))))

;; For downstream use
(defvar org-mem-updater--target<>old-links (make-hash-table :test 'equal)
  "Previous state of `org-mem--target<>links' for some targets.")

(defun org-mem-updater--forget-links-from-files (stale-files)
  "Remove from tables, all links in STALE-FILES.
Also clear `org-mem-updater--target<>old-links' and stash in it the
relevant rows from `org-mem--target<>links' prior to updating those rows
in the latter table."
  (clrhash org-mem-updater--target<>old-links)
  (let ((stale-eids (mapcar #'org-mem-entry--internal-id
                            (org-mem-entries-in-files stale-files)))
        stale-links
        targets-to-update)
    (dolist (eid stale-eids)
      (dolist (link (gethash eid org-mem--internal-entry-id<>links))
        (unless (member (org-mem-link-target link) targets-to-update)
          (push (org-mem-link-target link) targets-to-update)))
      ;; Be hygienic, prolly not important.
      (remhash eid org-mem--internal-entry-id<>links))
    (dolist (target targets-to-update)
      (let ((links (gethash target org-mem--target<>links)))
        (puthash target links org-mem-updater--target<>old-links)
        (cl-loop
         for link in links
         if (memq (org-mem-link--internal-entry-id link) stale-eids)
         do (push link stale-links)
         else collect link into reduced-link-set
         finally do
         (puthash target reduced-link-set org-mem--target<>links))))
    (dolist (link stale-links)
      (run-hook-with-args 'org-mem-forget-link-functions link))))


;;; Instant placeholders

(defun org-mem-updater-ensure-buffer-file-known ()
  "Record basic file metadata if not already known.
Use this if you cannot wait for `org-mem-updater-mode' to pick it up."
  (require 'org)
  (when (and buffer-file-truename
             (derived-mode-p 'org-mode)
             (not (gethash buffer-file-truename org-mem--file<>metadata))
             (file-exists-p buffer-file-truename))
    (puthash buffer-file-truename
             (list buffer-file-truename
                   (file-attributes buffer-file-truename)
                   (line-number-at-pos (point-max))
                   (point-max))
             org-mem--file<>metadata)))

(declare-function org-element-context "org-element")
(defun org-mem-updater-ensure-link-at-point-known (&rest _)
  "Record the link at point.
Use this if you cannot wait for `org-mem-updater-mode' to pick it up.
No support for citations."
  (require 'org)
  (require 'org-element-ast)
  (when (and buffer-file-truename
             (derived-mode-p 'org-mode))
    (when-let* ((el (org-element-context))
                (target (org-element-property :path el))
                (type (org-element-property :type el)))
      (let ((desc-beg (org-element-property :contents-begin el))
            (desc-end (org-element-property :contents-end el)))
        (org-mem-updater-ensure-buffer-file-known)
        (org-mem--record-link
         (record 'org-mem-link
                 buffer-file-truename
                 (point)
                 type
                 target
                 (and desc-beg
                      (buffer-substring-no-properties desc-beg desc-end))
                 nil
                 (org-entry-get-with-inheritance "ID")
                 ;; HACK
                 nil))))))

(declare-function org-current-level "org")
(declare-function org-entry-beginning-position "org")
(declare-function org-entry-properties "org")
(declare-function org-get-heading "org")
(declare-function org-get-outline-path "org")
(declare-function org-get-title "org")
(declare-function org-get-todo-state "org")
(declare-function org-link-display-format "ol")
(defvar org-outline-path-cache)
(defun org-mem-updater-ensure-entry-at-point-known ()
  "Record the entry at point.
Use this if you cannot wait for `org-mem-updater-mode' to pick it up."
  (require 'org)
  (require 'ol)
  (when (and buffer-file-truename
             (derived-mode-p 'org-mode))
    (org-mem-updater-ensure-buffer-file-known)
    (let ((id (org-entry-get-with-inheritance "ID"))
          (case-fold-search t))
      (save-excursion
        (without-restriction
          (when id
            (goto-char (point-min))
            (re-search-forward (concat "^[ \t]*:ID: +" (regexp-quote id))))
          (let* ((heading (org-get-heading t t t t))
                 (pos (and heading (org-entry-beginning-position)))
                 (olp-w-self (and heading (org-get-outline-path t t)))
                 (properties (org-entry-properties))
                 (ftitle (org-get-title)))
            ;; Polish the strings
            (when heading (setq heading (org-link-display-format
                                         (substring-no-properties heading))))
            (when ftitle (setq ftitle (org-link-display-format
                                       (substring-no-properties ftitle))))
            (org-mem--record-entry
             (record 'org-mem-entry
                     buffer-file-truename
                     (if heading (line-number-at-pos pos t) 1)
                     (if heading pos 1)
                     (or heading ftitle)
                     ;; NOTE: Don't use `org-reduced-level' since
                     ;;       org-mem-parser.el also does not.
                     (or (org-current-level) 0)
                     id
                     nil
                     ;; HACK: partial data
                     (append (cl-loop
                              for heading in olp-w-self
                              as pos = (car (cl-rassoc heading org-outline-path-cache
                                                       :key #'car :test #'string=))
                              collect (list -1 -1 pos heading nil nil))
                             (list (list 0 1 1 ftitle nil nil)))
                     nil ;; HACK
                     properties
                     (org-mem-updater--tags-at-point-inherited-only)
                     (org-get-tags nil t)
                     (when heading (org-get-todo-state))
                     (cdr (assoc "DEADLINE" properties))
                     (cdr (assoc "SCHEDULED" properties))
                     nil
                     nil))))))))

(defvar org-use-tag-inheritance)
(defvar org-trust-scanner-tags)
(defun org-mem-updater--tags-at-point-inherited-only ()
  "Like `org-get-tags', but get only the inherited tags."
  (require 'org)
  (let ((all-tags (if org-use-tag-inheritance
                      (org-get-tags)
                    (let ((org-use-tag-inheritance t)
                          (org-trust-scanner-tags nil))
                      (org-get-tags)))))
    (cl-loop for tag in all-tags
             when (get-text-property 0 'inherited tag)
             collect (substring-no-properties tag))))


;;; Mode

(defvar org-mem-updater--timer (timer-create)
  "Timer for intermittently running `org-mem--scan-full'.")

(defun org-mem-updater--activate-timer (&rest _)
  "Adjust `org-mem-updater--timer' based on duration of last full scan.
If timer not running, start it."
  (let ((new-delay (* 15 (1+ org-mem--time-elapsed))))
    (when (or (not (member org-mem-updater--timer timer-idle-list))
              ;; Don't enter an infinite loop -- idle timers can be a footgun.
              (not (> (float-time (or (current-idle-time) 0))
                      new-delay)))
      (cancel-timer org-mem-updater--timer)
      (setq org-mem-updater--timer
            (run-with-idle-timer new-delay t #'org-mem--scan-full)))))

;;;###autoload
(define-minor-mode org-mem-updater-mode
  "Keep Org-mem cache up to date."
  :global t
  :group 'org-mem
  (require 'org-mem-updater)
  (if org-mem-updater-mode
      (progn
        (add-hook 'org-mem-post-full-scan-functions #'org-mem-updater--activate-timer 90)
        (add-hook 'after-save-hook #'org-mem-updater--handle-save)
        (advice-add 'rename-file :after #'org-mem-updater--handle-rename)
        (advice-add 'delete-file :after #'org-mem-updater--handle-delete)
        (advice-add 'org-insert-link :after #'org-mem-updater-ensure-link-at-point-known)
        (org-mem-updater--activate-timer)
        (org-mem--scan-full))
    (remove-hook 'org-mem-post-full-scan-functions #'org-mem-updater--activate-timer)
    (remove-hook 'after-save-hook #'org-mem-updater--handle-save)
    (advice-remove 'rename-file #'org-mem-updater--handle-rename)
    (advice-remove 'delete-file #'org-mem-updater--handle-delete)
    (advice-remove 'org-insert-link #'org-mem-updater-ensure-link-at-point-known)
    (cancel-timer org-mem-updater--timer)))

(provide 'org-mem-updater)

;;; org-mem-updater.el ends here
