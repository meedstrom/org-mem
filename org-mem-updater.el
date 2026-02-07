;;; org-mem-updater.el --- Incremental caching -*- lexical-binding: t; -*-

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
(require 'el-job)
(require 'org-mem)
(require 'org-mem-parser)
(define-obsolete-variable-alias 'org-mem-updater--timer 'org-mem-updater--reset-timer "0.28.0 (2026-02-07)")


;;; Targeted-scan

(defun org-mem-updater-update (&optional synchronous)
  "Update cache for each file that has changed, appeared or disappeared.

If SYNCHRONOUS, block Emacs until done.
If SYNCHRONOUS and interrupted by a quit, cancel the update."
  (let* ((db-files (copy-hash-table org-mem--truename<>metadata))
         (modified-files
          (cl-loop for truename in (org-mem--list-files-from-fs)
                   as attr = (file-attributes truename)
                   as real-mtime = (and attr (file-attribute-modification-time attr))
                   as db-mtime = (prog1 (org-mem-file-mtime truename)
                                   (remhash truename db-files))
                   when (or (not db-mtime)
                            (not (time-equal-p db-mtime real-mtime)))
                   collect truename))
         (removed-files
          (hash-table-keys db-files))
         (all (append removed-files modified-files)))
    ;; If async job already ongoing, this will cancel that and run a new one
    (el-job-ng-run
     :id 'org-mem-updater
     :inject-vars (append (org-mem--mk-work-vars)
                          (el-job-ng-vars org-mem-inject-vars))
     :require (cons 'org-mem-parser org-mem-load-features)
     :inputs all
     :funcall-per-input #'org-mem-parser--parse-file
     :callback #'org-mem-updater--finalize-targeted-scan)
    (when synchronous
      (el-job-ng-await-or-die
       'org-mem-updater 3600
       (format "Running org-mem-updater-update... (files: %S)" all)))))

(defun org-mem-updater--finalize-targeted-scan (parse-results)
  "Handle PARSE-RESULTS from `org-mem-updater-update'."
  (run-hook-with-args 'org-mem-pre-targeted-scan-functions parse-results)
  (mapc #'clrhash (hash-table-values org-mem--key<>subtable))
  (let (bad-paths problems)
    (with-current-buffer (setq org-mem-scratch (get-buffer-create " *org-mem-scratch*" t))
      (cl-loop for (bad-path problem file-datum entries links) in parse-results do
               (when bad-path (push bad-path bad-paths))
               (when problem (push problem problems))
               (org-mem-updater--forget-file-contents (car file-datum))
               (puthash (car file-datum) file-datum org-mem--truename<>metadata)
               (run-hook-with-args 'org-mem-record-file-functions file-datum)
               (dolist (entry entries)
                 (org-mem--record-entry entry)
                 (run-hook-with-args 'org-mem-record-entry-functions entry))
               (dolist (link links)
                 (push link (gethash (org-mem-link--internal-entry-id link)
                                     org-mem--internal-entry-id<>links))
                 (run-hook-with-args 'org-mem-record-link-functions link))))
    ;; REVIEW: Assuming it is fine to invalidate now and no need to
    ;;         move this line to before the above loop
    (org-mem--invalidate-file-names bad-paths)
    (org-mem--rebuild-specially-indexed-tables)

    (run-hook-with-args 'org-mem-post-targeted-scan-functions parse-results)
    (when problems
      (setq org-mem--problems (append problems org-mem--problems))
      (message "Scan had problems, see M-x org-mem-list-problems"))))

(defun org-mem-updater--forget-file-contents (files)
  "Delete from tables, most info relating to FILES and their contents.
You should also run `org-mem--invalidate-file-names'
and `org-mem--rebuild-specially-indexed-tables'."
  (setq files (ensure-list files))
  (when files
    (with-current-buffer (setq org-mem-scratch (get-buffer-create " *org-mem-scratch*" t))
      (dolist (file files)
        (dolist (entry (gethash file org-mem--truename<>entries))
          (remhash (org-mem-entry-id entry) org-mem--id<>entry)
          (remhash (org-mem-entry-title-maybe entry) org-mem--title<>id)
          (remhash (org-mem-entry--internal-id entry) org-mem--internal-entry-id<>links)
          (run-hook-with-args 'org-mem-forget-entry-functions entry))
        (remhash file org-mem--truename<>entries)
        (remhash file org-mem--truename<>metadata)
        (run-hook-with-args 'org-mem-forget-file-functions file)))))


;;; Instant placeholders

(declare-function org-current-level "org")
(declare-function org-element-context "org-element")
(declare-function org-entry-beginning-position "org")
(declare-function org-entry-get "org")
(declare-function org-entry-properties "org")
(declare-function org-find-property "org")
(declare-function org-get-heading "org")
(declare-function org-get-outline-path "org")
(declare-function org-get-title "org")
(declare-function org-get-todo-state "org")
(declare-function org-link-display-format "ol")
(declare-function org-element-property "org-element-ast")
(declare-function org-entry-get-with-inheritance "org")
(declare-function org-get-tags "org")
(declare-function org-before-first-heading-p "org")
(declare-function org-parse-time-string "org-macs")
(defvar org-entry-property-inherited-from)
(defvar org-outline-path-cache)
(defvar org-trust-scanner-tags)
(defvar org-use-tag-inheritance)

(defun org-mem-updater-ensure-buffer-file-known ()
  "Record basic file metadata if not already known.
Use this if you cannot wait for `org-mem-updater-mode' to pick it up."
  (require 'org)
  (when (and buffer-file-name
             (derived-mode-p 'org-mode)
             (file-exists-p buffer-file-name))
    (let ((file (file-truename buffer-file-name)))
      (unless (gethash file org-mem--truename<>metadata)
        (puthash file
                 (list file
                       (file-attributes file)
                       (line-number-at-pos (point-max) t)
                       (point-max))
                 org-mem--truename<>metadata)))))

(defun org-mem-updater-ensure-link-at-point-known (&rest _)
  "Record the link at point.
Use this if you cannot wait for `org-mem-updater-mode' to pick it up.
No support for citations."
  (require 'org)
  (require 'org-element-ast)
  (when (and buffer-file-name
             (derived-mode-p 'org-mode))
    (let ((el (org-element-context))
          (truename (file-truename buffer-file-name)))
      (when (and (org-element-property :path el)
                 (cl-notany (##string-search % buffer-file-name)
                            org-mem-exclude)
                 (cl-notany (##string-search % truename)
                            org-mem-exclude))
        (org-mem-updater-ensure-buffer-file-known)
        (let ((link (org-mem-updater-mk-link-atpt)))
          (push link (gethash (org-mem-link--internal-entry-id link)
                              org-mem--internal-entry-id<>links))
          (run-hook-with-args 'org-mem-record-link-functions link))))))

(defun org-mem-updater-ensure-id-node-at-point-known ()
  "Record ID-node at point.
Use this if you cannot wait for `org-mem-updater-mode' to pick it up."
  (require 'org)
  (require 'ol)
  (when (and buffer-file-name
             (derived-mode-p 'org-mode)
             (org-entry-get-with-inheritance "ID"))
    (let ((truename (file-truename buffer-file-name)))
      (when (and (cl-notany (##string-search % buffer-file-name)
                            org-mem-exclude)
                 (cl-notany (##string-search % truename)
                            org-mem-exclude))
        (save-excursion
          (without-restriction
            (goto-char org-entry-property-inherited-from)
            (org-mem-updater-ensure-buffer-file-known)
            (org-mem--record-entry (org-mem-updater-mk-entry-atpt))))))))

(defun org-mem-updater-mk-link-atpt ()
  "Return an `org-mem-link' object appropriate for link at point.
It is not associated with any entries or files, however.
Return nil if no link at point.
No support for citations."
  (require 'org)
  (require 'org-element-ast)
  (if-let* ((el (org-element-context))
            (target (org-element-property :path el)))
      (let ((type (org-element-property :type el))
            (desc-beg (org-element-property :contents-begin el))
            (desc-end (org-element-property :contents-end el))
            (truename (file-truename buffer-file-name)))
        (record 'org-mem-link
                truename
                (point)
                type
                target ;; HACK: includes supplement
                (and desc-beg (buffer-substring-no-properties desc-beg desc-end))
                nil
                (org-entry-get-with-inheritance "ID")
                nil ;; HACK: supplement field is nil
                (+ (org-mem-parser--hash truename)
                   (if (org-before-first-heading-p)
                       0
                     (org-entry-beginning-position)))))
    (error "No link at point %d in %s" (point) (current-buffer))))

(defun org-mem-updater-mk-entry-atpt ()
  "Return an `org-mem-entry' object appropriate for entry at point.
It is not associated with any links or files, however.
Some fields are incomplete or left at nil."
  (require 'org)
  (require 'ol)
  (let* ((heading (org-get-heading t t t t))
         (pos (and heading (org-entry-beginning-position)))
         (olp-w-self (and heading (org-get-outline-path t t)))
         (properties (org-entry-properties nil 'standard))
         (closed (cdr (assoc "CLOSED" properties)))
         (deadline (cdr (assoc "DEADLINE" properties)))
         (scheduled (cdr (assoc "SCHEDULED" properties)))
         (ftitle (org-get-title))
         (title (or heading ftitle))
         (truename (file-truename buffer-file-name)))
    (when title
      (setq title (org-link-display-format (substring-no-properties title))))
    (record 'org-mem-entry
            truename
            (if heading (line-number-at-pos pos t) 1)
            (if heading pos 1)
            title
            ;; NOTE: Don't use `org-reduced-level' since org-mem-parser.el does not
            (or (org-current-level) 0)
            (org-entry-get nil "ID")
            nil
            nil
            (and closed
                 (time-convert (encode-time (org-parse-time-string closed))
                               'integer))
            ;; HACK: Partial data, enough for `org-mem-entry-olpath' to work with
            (append (cl-loop
                     for heading in olp-w-self
                     as pos = (car (cl-rassoc heading org-outline-path-cache
                                              :key #'car :test #'string=))
                     collect (list -1 -1 pos heading nil nil))
                    (list (list 0 1 1 ftitle nil nil)))
            (and deadline
                 (time-convert (encode-time (org-parse-time-string deadline))
                               'integer))
            nil
            nil
            properties
            (and scheduled
                 (time-convert (encode-time (org-parse-time-string scheduled))
                               'integer))
            nil
            (org-mem-updater--tags-at-point-inherited-only)
            (org-get-tags nil t)
            (when heading (org-get-todo-state))
            (+ (org-mem-parser--hash truename) (if heading pos 0)))))

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

(defvar org-mem-updater--reset-timer (timer-create)
  "Timer for intermittently running `org-mem--scan-full'.")

(defun org-mem-updater-adjust-reset-timer (&rest _)
  "Adjust `org-mem-updater--reset-timer' based on duration of last full scan.
If timer not running, start it.
Override this if you prefer different timer delays, or no timer."
  (let ((new-delay (* 10 (1+ org-mem--time-elapsed))))
    (when (or (not (member org-mem-updater--reset-timer timer-idle-list))
              ;; Don't enter an infinite loop -- idle timers can be a footgun.
              (not (> (float-time (or (current-idle-time) 0))
                      new-delay)))
      (cancel-timer org-mem-updater--reset-timer)
      (setq org-mem-updater--reset-timer
            (run-with-idle-timer new-delay t #'org-mem--scan-full)))))

(defun org-mem-updater--update-file-soon (&optional file &rest _)
  "Schedule to run `org-mem-updater-update' very soon.

Designed for `after-save-hook' and as advice for `delete-file' and
`rename-file'.  Such functions might be called many times in a loop,
and this design is meant to avoid invoking `org-mem-updater-update'
for every FILE, but wait and do a massed invocation afterwards."
  (setq file (or file buffer-file-name))
  (when (and file (cl-some (##string-suffix-p % file) org-mem-suffixes))
    (org-mem-updater--update-soon)))

(defvar org-mem-updater--massing-timer nil)
(defun org-mem-updater--update-soon ()
  "Schedule to run `org-mem-updater-update' very soon.
If already scheduled, postpone to very soon."
  (if (memq org-mem-updater--massing-timer timer-list)
      (timer-set-time org-mem-updater--massing-timer
                      (time-add (current-time) 0.5))
    (setq org-mem-updater--massing-timer
          (run-with-timer 0.5 nil #'org-mem-updater-update))))

;;;###autoload
(define-minor-mode org-mem-updater-mode
  "Keep Org-mem cache up to date."
  :global t
  :group 'org-mem
  (require 'org-mem-updater)
  (if org-mem-updater-mode
      (progn
        (add-hook 'org-mem-post-full-scan-functions #'org-mem-updater-adjust-reset-timer 90)
        (add-hook 'after-save-hook                  #'org-mem-updater--update-file-soon)
        (advice-add 'rename-file :after             #'org-mem-updater--update-file-soon)
        (advice-add 'delete-file :after             #'org-mem-updater--update-file-soon)
        (org-mem-updater-adjust-reset-timer)
        (org-mem--scan-full))
    (remove-hook 'org-mem-post-full-scan-functions #'org-mem-updater-adjust-reset-timer)
    (remove-hook 'after-save-hook                  #'org-mem-updater--update-file-soon)
    (advice-remove 'rename-file                    #'org-mem-updater--update-file-soon)
    (advice-remove 'delete-file                    #'org-mem-updater--update-file-soon)
    (cancel-timer org-mem-updater--reset-timer)))

(defvar org-mem-updater--id-or-ref-target<>old-links :obsolete)
(defvar org-mem-updater--new-id-or-ref-targets :obsolete)
(define-obsolete-function-alias 'org-mem-updater--adjust-timer #'org-mem-updater-adjust-reset-timer "0.28.0 (2026-02-07)")

(provide 'org-mem-updater)

;;; org-mem-updater.el ends here
