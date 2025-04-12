;;; indexed-x.el --- Incremental indexing -*- lexical-binding: t; -*-

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
;; reducing our need to do `indexed--full-scan' so often.

;; Technically, repeating a full scan is never needed *IF* we use these hooks
;; correctly.  However, that is hard and humans are fallible.

;; If a full scan is sufficiently performant, you can just do it more often,
;; instead of using these hooks at all.  It is also a simple way to detect
;; filesystem changes made by other Emacsen or the command line.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'llama)
(require 'indexed)
(require 'indexed-org-parser)
(defvar org-use-tag-inheritance)
(defvar org-trust-scanner-tags)
(defvar org-id-track-globally)
(defvar org-id-locations)
(declare-function org-current-level "org")
(declare-function org-element-context "org-element")
(declare-function org-element-property "org-element")
(declare-function org-entry-beginning-position "org")
(declare-function org-entry-get-with-inheritance "org")
(declare-function org-entry-properties "org")
(declare-function org-get-heading "org")
(declare-function org-get-outline-path "org")
(declare-function org-get-tags "org")
(declare-function org-get-title "org")
(declare-function org-get-todo-state "org")
(declare-function org-link-display-format "ol")

(defvar indexed-x--timer (timer-create))
(defun indexed-x--activate-timer (&rest _)
  "Adjust `indexed-x--timer' based on duration of last indexing.
If not running, start it."
  (let ((new-delay (* 20 (1+ indexed--time-elapsed))))
    (when (or (not (member indexed-x--timer timer-idle-list))
              ;; Don't enter an infinite loop -- idle timers can be a footgun.
              (not (> (float-time (or (current-idle-time) 0))
                      new-delay)))
      (cancel-timer indexed-x--timer)
      (setq indexed-x--timer
            (run-with-idle-timer new-delay t #'indexed--scan-full)))))

(defun indexed-x--handle-save ()
  "Arrange to re-scan nodes and links in current buffer."
  (when indexed-updater-mode
    (let ((% buffer-file-truename))
      (when (and (string-suffix-p ".org" %)
                 (not (backup-file-name-p %))
                 (not (indexed--tramp-file-p %)))
        (indexed-x--scan-targeted %)))))

;; NOTE: When setting `delete-by-moving-to-trash' is t, `delete-file' calls
;;       `move-file-to-trash' which calls `rename-file'.  Good to know.
;;       ... And it appears that `rename-file' can also call `delete-file'.
;;       Happy coding!

;; (defun indexed-x--handle-rename (file newname &rest _)
;;   "Arrange to scan NEWNAME for nodes and links, and forget FILE."
;;   (indexed-x--scan-targeted
;;    (thread-last (list file newname)
;;                 (cl-remove-if-not (##string-suffix-p ".org" %))
;;                 (cl-remove-if #'backup-file-name-p)
;;                 (cl-remove-if #'indexed--tramp-file-p)
;;                 ;; REVIEW: May not apply right to oldname?
;;                 (mapcar #'file-truename)
;;                 (indexed--fast-abbrev-file-names))))

(defun indexed-x--handle-delete (file &optional _trash)
  "Arrange to forget nodes and links in FILE."
  (when indexed-updater-mode
    (when (string-suffix-p ".org" file)
      (unless (indexed--tramp-file-p file)
        (setq file (indexed--fast-abbrev-file-names file))
        ;; Used to just hand the file to `indexed-x--scan-targeted' which will
        ;; have the same effect if the file is gone, but sometimes it is not
        ;; gone, thanks to `delete-by-moving-to-trash'.
        (indexed-x--forget-files (list file))
        (indexed-x--forget-links-from
         (mapcar #'indexed-id (indexed-entries-in file)))))))

(defun indexed-x--scan-targeted (files)
  "Arrange to scan FILES."
  (when files
    (el-job-launch :id 'indexed-x
                   :inject-vars (indexed--mk-work-vars)
                   :load-features '(indexed-org-parser)
                   :inputs (ensure-list files)
                   :funcall-per-input #'indexed-org-parser--parse-file
                   :callback #'indexed-x--finalize-targeted)))

(defun indexed-x--finalize-targeted (results _job)
  "Use RESULTS to update tables.
Argument JOB is the el-job object."
  (run-hook-with-args 'indexed-pre-incremental-update-functions results)
  (seq-let (missing-files file-data entries links problems) results
    (indexed-x--forget-files missing-files)
    (indexed-x--forget-links-from (mapcar #'indexed-id entries))
    (dolist (fdata file-data)
      (puthash (indexed-file-name fdata) fdata indexed--file<>data)
      (run-hook-with-args 'indexed-record-file-functions fdata))
    (dolist (entry entries)
      (indexed--record-entry entry)
      (run-hook-with-args 'indexed-record-entry-functions entry))
    (dolist (link links)
      (indexed--record-link link)
      (run-hook-with-args 'indexed-record-link-functions link))
    (dolist (prob problems)
      (push prob indexed--problems))
    (run-hook-with-args 'indexed-post-incremental-update-functions results)
    (when problems
      (message "Scan had problems, see M-x indexed-list-problems"))))

(defun indexed-x--forget-files (goners)
  "Remove from cache, most info about entries in file list GONERS.

For a thorough cleanup, you should also run
`indexed-x--forget-links-from'."
  (when (setq goners (ensure-list goners))
    (let (files)
      ;; Low priority: clean `indexed--abbr-truenames' just in case.
      (maphash (lambda (file truename)
                 (when (member truename goners)
                   (remhash file indexed--abbr-truenames)
                   (push file files)))
               indexed--abbr-truenames)
      (when (and indexed-sync-with-org-id
                 (indexed--try-ensure-org-id-table-p))
        ;; REVIEW: This can possibly be simplified to the same thing we do with
        ;;         table `indexed--id<>entry' below, but need to think
        ;;         carefully about that.
        (maphash (lambda (id file)
                   (when (member file files)
                     (remhash id org-id-locations)))
                 org-id-locations)))
    (dolist (entry (indexed-org-entries))
      (when (member (indexed-file-name entry) goners)
        (remhash (indexed-id entry) indexed--id<>entry)
        (remhash (indexed-title entry) indexed--title<>id)
        (run-hook-with-args 'indexed-forget-entry-functions entry)))
    (dolist (goner goners)
      (remhash goner indexed--file<>data)
      (remhash goner indexed--file<>entries)
      (run-hook-with-args 'indexed-forget-file-functions goner))))

;; TODO: Explain why this is separate from above.
;;       Tried to merge once, realized there was a reason to separate, now
;;       forgot the reason.
(defvar indexed-x-last-removed-links nil)
(defun indexed-x--forget-links-from (dead-ids)
  "Forget links with :nearby-id matching any of DEAD-IDS.
Put the forgotten links into `indexed-x-last-removed-links'."
  (let ((dests-to-update
         (cl-loop
          for origin being each hash-key of indexed--origin<>links
          using (hash-values link-set)
          when (member origin dead-ids)
          append (mapcar #'indexed-dest link-set))))
    (dolist (id dead-ids)
      (remhash id indexed--origin<>links))
    (setq indexed-x-last-removed-links nil)
    (cl-loop
     for dest being each hash-key of indexed--dest<>links
     using (hash-values link-set)
     when (member dest dests-to-update)
     do (puthash dest
                 (cl-loop for link in link-set
                          if (member (indexed-origin link) dead-ids)
                          do (push link indexed-x-last-removed-links)
                          else collect link)
                 indexed--dest<>links))
    (dolist (link indexed-x-last-removed-links)
      (run-hook-with-args 'indexed-forget-link-functions link))))


;;; Helper API for weird situations

(defun indexed-x-ensure-buffer-file-known ()
  "Ensure file data is in cache right now.
Use this if you cannot wait for `indexed-updater-mode'
to pick it up."
  (require 'org)
  (when (and buffer-file-truename
             (derived-mode-p 'org-mode)
             (not (gethash buffer-file-truename indexed--file<>data))
             (file-exists-p buffer-file-truename))
    ;; Satisfice w/ incomplete data for now
    (puthash buffer-file-truename
             (indexed-file-data--make-obj
              :file-name buffer-file-truename
              :file-title (org-get-title)
              ;; :max-lines (line-number-at-pos (point-max)) ;; Slow
              :max-lines 0
              :ptmax (point-max)
              :toplvl-id nil)
             indexed--file<>data)))

(defun indexed-x-ensure-link-at-point-known (&rest _)
  (require 'org)
  (require 'org-element)
  (when (and buffer-file-truename
             (derived-mode-p 'org-mode))
    (when-let* ((el (org-element-context))
                (dest (org-element-property :path el))
                (type (org-element-property :type el)))
      (indexed-x-ensure-buffer-file-known)
      (indexed--record-link
       (indexed-org-link--make-obj
        :nearby-id (org-entry-get-with-inheritance "ID")
        :pos (point)
        :type type
        :dest dest
        :file-name buffer-file-truename)))))

(defun indexed-x-ensure-entry-at-point-known ()
  "Record the entry at point.
Use this if you cannot wait for `indexed-updater-mode'
to pick it up.

Unlike `indexed-x-ensure-buffer-file-known', this will re-record no
matter what, which is useful in the context that a heading title has
changed."
  (require 'org)
  (require 'ol)
  (when (and buffer-file-truename
             (derived-mode-p 'org-mode))
    (indexed-x-ensure-buffer-file-known)
    (let ((id (org-entry-get-with-inheritance "ID"))
          (case-fold-search t))
      (save-excursion
        (without-restriction
          (when id
            (goto-char (point-min))
            (re-search-forward (concat "^[\t\s]*:id: +" (regexp-quote id))))
          (let ((props (org-entry-properties))
                (heading (org-get-heading t t t t))
                (ftitle (org-get-title)))
            (when heading
              (setq heading (org-link-display-format
                             (substring-no-properties heading))))
            (when ftitle
              (setq ftitle (org-link-display-format
                            (substring-no-properties ftitle))))
            (indexed--record-entry
             (indexed-org-entry--make-obj
              :id id
              :title (or heading ftitle)
              :file-name buffer-file-truename
              :pos (if heading (org-entry-beginning-position) 1)
              ;; NOTE: Don't use `org-reduced-level' since
              ;;       indexed-org-parser.el also does not.
              :heading-lvl (or (org-current-level) 0)
              :olpath (org-get-outline-path)
              :lnum (if heading (line-number-at-pos
                                 (org-entry-beginning-position) t)
                      1)
              :priority nil ;; HACK
              :properties props
              :tags-local (org-get-tags nil t)
              :tags-inherited (indexed-x--tags-at-point-inherited-only)
              :todo-state (when heading (org-get-todo-state))
              :deadline (cdr (assoc "DEADLINE" props))
              :scheduled (cdr (assoc "SCHEDULED" props))))))))))

(defun indexed-x--tags-at-point-inherited-only ()
  "Like `org-get-tags', but get only the inherited tags."
  (require 'org)
  (let ((all-tags (if org-use-tag-inheritance
                      ;; NOTE: Above option can have complex rules.
                      ;; This handles them correctly, but it's moot as
                      ;; `indexed-org-parser--parse-file' does not.
                      (org-get-tags)
                    (let ((org-use-tag-inheritance t)
                          (org-trust-scanner-tags nil))
                      (org-get-tags)))))
    (cl-loop for tag in all-tags
             when (get-text-property 0 'inherited tag)
             collect (substring-no-properties tag))))

(defun indexed-x-snitch-to-org-id ()
  (declare (obsolete "baked into `indexed-sync-with-org-id'" "2025-03-26")))

(provide 'indexed-x)

;;; indexed-x.el ends here
