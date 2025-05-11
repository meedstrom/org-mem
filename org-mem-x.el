;;; org-mem-x.el --- Incremental indexing -*- lexical-binding: t; -*-

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
(require 'seq)
(require 'llama)
(require 'org-mem)
(require 'org-mem-parser)
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
(define-obsolete-variable-alias 'indexed-x-last-removed-links 'org-mem-x-forgotten-links "2025-05-11")

(defvar org-mem-x--timer (timer-create)
  "Timer for intermittently running `org-mem--scan-full'.")

(defun org-mem-x--activate-timer (&rest _)
  "Adjust `org-mem-x--timer' based on duration of last full scan.
If timer not running, start it."
  (let ((new-delay (* 20 (1+ org-mem--time-elapsed))))
    (when (or (not (member org-mem-x--timer timer-idle-list))
              ;; Don't enter an infinite loop -- idle timers can be a footgun.
              (not (> (float-time (or (current-idle-time) 0))
                      new-delay)))
      (cancel-timer org-mem-x--timer)
      (setq org-mem-x--timer
            (run-with-idle-timer new-delay t #'org-mem--scan-full)))))

;; NOTE: When setting `delete-by-moving-to-trash' is t, `delete-file' calls
;;       `move-file-to-trash' which calls `rename-file'.  And it appears that
;;       `rename-file' can also call `delete-file'.  Happy coding!

(defun org-mem-x--handle-rename (file newname &rest _)
  "Arrange to scan NEWNAME for entries and links, and forget FILE."
  (org-mem-x--handle-delete file)
  (cl-assert newname)
  (org-mem-x--handle-save newname))

(defun org-mem-x--handle-save (&optional file)
  "Arrange to scan entries and links in FILE or current buffer file."
  (unless file (setq file buffer-file-truename))
  (when (and (string-suffix-p ".org" file)
             (not (backup-file-name-p file))
             (not (org-mem--tramp-file-p file)))
    (org-mem-x--scan-targeted file)))

(defun org-mem-x--handle-delete (file &optional _trash)
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
      (org-mem-x--forget-file-contents bad)
      (org-mem--invalidate-file-names bad)
      (mapc #'clrhash (hash-table-values org-mem--key<>subtable)))))

(defun org-mem-x--scan-targeted (files)
  "Arrange to scan FILES."
  (when files
    (el-job-launch :id 'org-mem-x
                   :inject-vars (org-mem--mk-work-vars)
                   :load-features '(org-mem-parser)
                   :inputs (ensure-list files)
                   :funcall-per-input #'org-mem-parser--parse-file
                   :callback #'org-mem-x--finalize-targeted)))

(defun org-mem-x--finalize-targeted (parse-results _job)
  "Handle PARSE-RESULTS from `org-mem-x--scan-targeted'."
  (run-hook-with-args 'org-mem-pre-targeted-scan-functions parse-results)
  (mapc #'clrhash (hash-table-values org-mem--key<>subtable))
  (seq-let (bad-paths file-data entries links problems) parse-results
    (when bad-paths
      (org-mem-x--forget-file-contents bad-paths)
      (org-mem--invalidate-file-names bad-paths))
    (dolist (datum file-data)
      (puthash (car datum) datum org-mem--file<>metadata)
      (run-hook-with-args 'org-mem-record-file-functions datum))
    (dolist (entry entries)
      (org-mem--record-entry entry)
      (run-hook-with-args 'org-mem-record-entry-functions entry))
    (dolist (link links)
      (org-mem--record-link link)
      (run-hook-with-args 'org-mem-record-link-functions link))
    (dolist (prob problems)
      (push prob org-mem--problems))
    (run-hook-with-args 'org-mem-post-targeted-scan-functions parse-results)
    (when bad-paths
      (let ((good-paths (seq-keep #'org-mem--abbr-truename bad-paths)))
        (org-mem-x--scan-targeted (seq-difference good-paths bad-paths))))
    (when problems
      (message "Scan had problems, see M-x org-mem-list-problems"))))

;; XXX Test. Verify links still get forgotten: check for backlink duplicates
(defvar org-mem-x-forgotten-links nil)
(defun org-mem-x--forget-file-contents (files)
  "Delete from tables, most info relating to FILES and their contents.
You should also run `org-mem--invalidate-file-names'.

For downstream use, a list of deleted `org-mem-link' records is put in
variable `org-mem-x-forgotten-links' until next time.  These are the
same links that were passed to `org-mem-forget-link-functions'."
  (setq files (ensure-list files))
  (when files
    (dolist (file files)
      (dolist (entry (org-mem-entries-in-file file))
        (remhash (org-mem-entry-id entry) org-mem--id<>entry)
        (remhash (org-mem-entry-title entry) org-mem--title<>id)
        (remhash (org-mem-entry-internal-id entry)
                 org-mem--internal-entry-id<>links)
        (run-hook-with-args 'org-mem-forget-entry-functions entry))
      (remhash file org-mem--file<>entries)
      (remhash file org-mem--file<>metadata)
      (run-hook-with-args 'org-mem-forget-file-functions file))
    (setq org-mem-x-forgotten-links nil)
    (maphash (lambda (dest links)
               (let (reduced-link-set)
                 (dolist (link links)
                   (if (member (org-mem-link-file link) files)
                       (push link org-mem-x-forgotten-links)
                     (push link reduced-link-set)))
                 (puthash dest reduced-link-set org-mem--dest<>links)))
             org-mem--dest<>links)
    (dolist (link org-mem-x-forgotten-links)
      (run-hook-with-args 'org-mem-forget-link-functions link))))


;;; Instant placeholders

(defun org-mem-x-ensure-buffer-file-known ()
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

(defun org-mem-x-ensure-link-at-point-known (&rest _)
  "Record the link at point.
Use this if you cannot wait for `org-mem-updater-mode' to pick it up."
  (require 'org)
  (require 'org-element)
  (when (and buffer-file-truename
             (derived-mode-p 'org-mode))
    (when-let* ((el (org-element-context))
                (dest (org-element-property :path el))
                (type (org-element-property :type el)))
      (org-mem-x-ensure-buffer-file-known)
      (org-mem--record-link
       (org-mem-link--make-obj
        :file buffer-file-truename
        :pos (point)
        :citation-p nil ;; HACK
        :type type
        :dest dest
        :nearby-id (org-entry-get-with-inheritance "ID"))))))

(defun org-mem-x-ensure-entry-at-point-known ()
  "Record the entry at point.
Use this if you cannot wait for `org-mem-updater-mode' to pick it up."
  (require 'org)
  (require 'ol)
  (when (and buffer-file-truename
             (derived-mode-p 'org-mode))
    (org-mem-x-ensure-buffer-file-known)
    (let ((id (org-entry-get-with-inheritance "ID"))
          (case-fold-search t))
      (save-excursion
        (without-restriction
          (when id
            (goto-char (point-min))
            (re-search-forward (concat "^[ \t]*:id: +" (regexp-quote id))))
          (let ((props (org-entry-properties))
                (heading (org-get-heading t t t t))
                (ftitle (org-get-title)))
            (when heading
              (setq heading (org-link-display-format
                             (substring-no-properties heading))))
            (when ftitle
              (setq ftitle (org-link-display-format
                            (substring-no-properties ftitle))))
            (org-mem--record-entry
             (org-mem-entry--make-obj
              :id id
              :title (or heading ftitle)
              :file buffer-file-truename
              :pos (if heading (org-entry-beginning-position) 1)
              ;; NOTE: Don't use `org-reduced-level' since
              ;;       org-mem-parser.el also does not.
              :level (or (org-current-level) 0)
              :lnum (if heading (line-number-at-pos
                                 (org-entry-beginning-position) t)
                      1)
              ;; HACK
              :crumbs
              (cons (list 0 0 0 ftitle nil nil)
                    (nreverse
                     (cl-loop for heading in (org-get-outline-path t t)
                              collect (list 0 0 0 heading nil nil))))
              :priority nil ;; HACK
              :properties props
              :tags-local (org-get-tags nil t)
              :todo-state (when heading (org-get-todo-state))
              :deadline (cdr (assoc "DEADLINE" props))
              :scheduled (cdr (assoc "SCHEDULED" props))))))))))


(define-obsolete-function-alias 'indexed-x--handle-save                #'org-mem-x--handle-save "2025-05-11")
(define-obsolete-function-alias 'indexed-x--handle-delete              #'org-mem-x--handle-delete "2025-05-11")
(define-obsolete-function-alias 'indexed-x--scan-targeted              #'org-mem-x--scan-targeted "2025-05-11")
(define-obsolete-function-alias 'indexed-x--finalize-targeted          #'org-mem-x--finalize-targeted "2025-05-11")
(define-obsolete-function-alias 'indexed-x--forget-files               #'org-mem-x--forget-file-contents "2025-05-11")
(define-obsolete-function-alias 'indexed-x-ensure-buffer-file-known    #'org-mem-x-ensure-buffer-file-known "2025-05-11")
(define-obsolete-function-alias 'indexed-x-ensure-link-at-point-known  #'org-mem-x-ensure-link-at-point-known "2025-05-11")
(define-obsolete-function-alias 'indexed-x-ensure-entry-at-point-known #'org-mem-x-ensure-entry-at-point-known "2025-05-11")
(define-obsolete-function-alias 'indexed--activate-timer               #'org-mem-x--activate-timer "2025-05-11")

(provide 'org-mem-x)

;;; org-mem-x.el ends here
