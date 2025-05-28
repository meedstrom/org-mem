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


;;; Targeted-scan

(defun org-mem-updater--handle-rename (file newname &rest _)
  "Arrange to scan NEWNAME for entries and links, and forget FILE."
  (org-mem-updater--handle-delete file)
  (unless (memq 'move-file-to-trash
                (cl-loop for i from 1 to 15 collect (cadr (backtrace-frame i))))
    (cl-assert newname) ;; b/c below func would accept nil
    (org-mem-updater--handle-save newname)))

(defun org-mem-updater--handle-delete (file &optional _trash)
  "Forget entries and links in FILE.

If FILE differs from the name by which the actual file is listed in our
tables, because a parent directory is a symlink or the abbreviation
differs, try to discover the known name variant, then forget the data
from that file.

However, do not do so when FILE itself satisfies `file-symlink-p'.
In that case, there may be nothing wrong with the known name."
  (when (and (or (string-suffix-p ".org" file)
                 (string-suffix-p ".org_archive" file))
             ;; Don't accidentally scrub Tramp files from org-id-locations
             ;; just because we chose to know nothing about them.
             (not (org-mem--tramp-file-p file)))
    (let ((bad (list file))
          (cached-true (gethash file org-mem--wild-filename<>truename)))
      (when (and cached-true (not (file-symlink-p file)))
        (push cached-true bad))
      (org-mem-updater--forget-links-from-files bad) ;; REVIEW
      (org-mem-updater--forget-file-contents bad)
      (org-mem--invalidate-file-names bad)
      (mapc #'clrhash (hash-table-values org-mem--key<>subtable)))))

(defun org-mem-updater--handle-save (&optional file)
  "Arrange to scan entries and links in FILE, or current buffer file."
  (unless file (setq file buffer-file-name))
  (when (and (or (string-suffix-p ".org" file)
                 (string-suffix-p ".org_archive" file))
             (not (backup-file-name-p file))
             (not (org-mem--tramp-file-p file)) ;; superfluous
             (cl-notany (##string-search % file) org-mem-exclude))
    (org-mem-updater--scan-targeted file)))

(defun org-mem-updater--scan-targeted (file)
  "Arrange to scan FILE or FILEs."
  (let ((truenames (thread-last
                     (ensure-list file)
                     (seq-keep #'org-mem--truename-maybe)
                     (seq-uniq)
                     (seq-filter (##cl-loop for xclude in org-mem-exclude
                                            never (string-search xclude %))))))
    (el-job-launch :id 'org-mem-updater
                   :inject-vars (org-mem--mk-work-vars)
                   :load-features '(org-mem-parser)
                   :inputs truenames
                   :funcall-per-input #'org-mem-parser--parse-file
                   :callback #'org-mem-updater--finalize-targeted-scan)))

(defun org-mem-updater--finalize-targeted-scan (parse-results _job)
  "Handle PARSE-RESULTS from `org-mem-updater--scan-targeted'."
  (run-hook-with-args 'org-mem-pre-targeted-scan-functions parse-results)
  (seq-let (bad-paths file-data entries links problems) parse-results
    (let ((stale-files (append bad-paths (mapcar #'car file-data))))
      (org-mem-updater--forget-links-from-files stale-files)
      (org-mem-updater--forget-file-contents stale-files))
    (when bad-paths
      (org-mem--invalidate-file-names bad-paths))
    (mapc #'clrhash (hash-table-values org-mem--key<>subtable))
    (with-current-buffer
        (setq org-mem-scratch (get-buffer-create " *org-mem-scratch*" t))
      (dolist (datum file-data)
        (puthash (car datum) datum org-mem--truename<>metadata)
        (run-hook-with-args 'org-mem-record-file-functions datum))
      (dolist (entry entries)
        (org-mem--record-entry entry))
      (dolist (link links)
        (org-mem--record-link link)
        (unless (gethash (org-mem-link-target link)
                         org-mem-updater--id-or-ref-target<>old-links)
          (when (or (org-mem-roam-reflink-p link)
                    (org-mem-id-link-p link))
            (push (org-mem-link-target link)
                  org-mem-updater--new-id-or-ref-targets)))))
    (dolist (prob problems)
      (push prob org-mem--problems))
    (run-hook-with-args 'org-mem-post-targeted-scan-functions parse-results)
    (when bad-paths
      (let ((good-paths (seq-keep #'org-mem--truename-maybe bad-paths)))
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
        (dolist (entry (gethash file org-mem--truename<>entries))
          (remhash (org-mem-entry-id entry) org-mem--id<>entry)
          (remhash (org-mem-entry-title-maybe entry) org-mem--title<>id)
          (run-hook-with-args 'org-mem-forget-entry-functions entry))
        (remhash file org-mem--truename<>entries)
        (remhash file org-mem--truename<>metadata)
        (run-hook-with-args 'org-mem-forget-file-functions file)))))

;; For downstream use
(defvar org-mem-updater--id-or-ref-target<>old-links (make-hash-table :test 'equal)
  "Previous state of `org-mem--target<>links' for some targets.
Only includes targets that are or were IDs or roam-refs, so that the
values can be considered \"forward links\" qualified for backlinks under
some paradigms.
Values may be in different order.")

(defvar org-mem-updater--new-id-or-ref-targets nil
  "IDs and ROAM_REFS that had no links targeting them but now do.
Even if the ID or ref is old, it would previously have had no presence
in table `org-mem--target<>links'.")

;; FIXME: It seems to remove too many, or they don't all get re-added or something
(defun org-mem-updater--forget-links-from-files (stale-files)
  "Remove from tables, all links in STALE-FILES.
Also clear `org-mem-updater--id-or-ref-target<>old-links' and stash into it the
relevant rows from `org-mem--target<>links' prior to updating those rows
in the latter table."
  (clrhash org-mem-updater--id-or-ref-target<>old-links)
  (let ((stale-eids (mapcar #'org-mem-entry--internal-id
                            (org-mem-entries-in-files stale-files)))
        targets-to-update)
    (dolist (eid stale-eids)
      (dolist (link (gethash eid org-mem--internal-entry-id<>links))
        (cl-pushnew (org-mem-link-target link) targets-to-update
                    :test #'equal))
      (remhash eid org-mem--internal-entry-id<>links))
    (dolist (tgt targets-to-update)
      (cl-loop
       for link in (gethash tgt org-mem--target<>links)
       if (memq (org-mem-link--internal-entry-id link) stale-eids)
       collect link into stale-links
       else collect link into reduced-link-set
       finally do
       (puthash tgt reduced-link-set org-mem--target<>links)
       (dolist (link stale-links)
         (run-hook-with-args 'org-mem-forget-link-functions link))
       (dolist (link (nconc stale-links reduced-link-set))
         ;; TODO: Don't waste perf checking every link, check tgt once
         (when (or (org-mem-roam-reflink-p link)
                   (org-mem-id-link-p link))
           (push link (gethash tgt org-mem-updater--id-or-ref-target<>old-links))))))))


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
    (let ((el (org-element-context)))
      (when (and (org-element-property :path el)
                 (cl-notany (##string-search % buffer-file-name)
                            org-mem-exclude)
                 (cl-notany (##string-search % (file-truename buffer-file-name))
                          org-mem-exclude))
        (org-mem-updater-ensure-buffer-file-known)
        (org-mem--record-link (org-mem-updater-mk-link-atpt))))))

(defun org-mem-updater-ensure-id-node-at-point-known ()
  "Record ID-node at point.
Use this if you cannot wait for `org-mem-updater-mode' to pick it up."
  (require 'org)
  (require 'ol)
  (when (and buffer-file-name
             (derived-mode-p 'org-mode))
    (when (and (org-entry-get-with-inheritance "ID")
               (cl-notany (##string-search % buffer-file-name)
                          org-mem-exclude)
               (cl-notany (##string-search % (file-truename buffer-file-name))
                          org-mem-exclude))
      (save-excursion
        (without-restriction
          (goto-char org-entry-property-inherited-from)
          (org-mem-updater-ensure-buffer-file-known)
          (org-mem--record-entry (org-mem-updater-mk-entry-atpt)))))))

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
                target
                (and desc-beg (buffer-substring-no-properties desc-beg desc-end))
                nil
                (org-entry-get-with-inheritance "ID")
                (org-mem-parser--mk-id
                 truename (if (org-before-first-heading-p) 0
                            (org-entry-beginning-position)))))
    (error "No link at point %d in %s" (point) (current-buffer))))

(defun org-mem-updater-mk-entry-atpt ()
  "Return an `org-mem-entry' object appropriate for entry at point.
It is not associated with any links or files, however."
  (require 'org)
  (require 'ol)
  (let* ((heading (org-get-heading t t t t))
         (pos (and heading (org-entry-beginning-position)))
         (olp-w-self (and heading (org-get-outline-path t t)))
         (properties (org-entry-properties))
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
            ;; HACK: Partial data, enough for `org-mem-entry-olpath' to work with
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
            (org-mem-parser--mk-id truename (if heading pos 0))
            ;; HACK: nils are fine
            nil
            nil)))

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

(defun org-mem-updater--adjust-timer (&rest _)
  "Adjust `org-mem-updater--timer' based on duration of last full scan.
If timer not running, start it.
Override this if you prefer different timer delays, or no timer."
  (let ((new-delay (* 10 (1+ org-mem--time-elapsed))))
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
        (add-hook 'org-mem-post-full-scan-functions #'org-mem-updater--adjust-timer 90)
        (add-hook 'after-save-hook                  #'org-mem-updater--handle-save)
        (advice-add 'rename-file :after             #'org-mem-updater--handle-rename)
        (advice-add 'delete-file :after             #'org-mem-updater--handle-delete)
        (org-mem-updater--adjust-timer)
        (org-mem--scan-full))
    (remove-hook 'org-mem-post-full-scan-functions #'org-mem-updater--adjust-timer)
    (remove-hook 'after-save-hook                  #'org-mem-updater--handle-save)
    (advice-remove 'rename-file                    #'org-mem-updater--handle-rename)
    (advice-remove 'delete-file                    #'org-mem-updater--handle-delete)
    (cancel-timer org-mem-updater--timer)))

(define-obsolete-function-alias 'org-mem-updater-ensure-entry-at-point-known
  #'org-mem-updater-ensure-id-node-at-point-known "2025-05-21")
(define-obsolete-function-alias 'org-mem-updater--activate-timer
  #'org-mem-updater--adjust-timer "2025-05-24")

(provide 'org-mem-updater)

;;; org-mem-updater.el ends here
