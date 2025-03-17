;;; indexed-x.el --- Incremental indexing -*- lexical-binding: t; -*-

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

;; Optional mechanisms to update the tables in just-in-time fashion,
;; reducing our need to do `indexed--full-scan' so often.

;; Technically, a full reset is never needed *IF* we use these hooks
;; correctly.  However, that is hard and humans are fallible.

;; If a full reset is sufficiently performant, you can just do it more often,
;; instead of using these hooks at all.  It is also a simple way to detect
;; filesystem changes made by other Emacsen or the command line.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'llama)
(require 'indexed)
(declare-function tramp-tramp-file-p "tramp")

(defun indexed-x--tramp-file-p (file)
  "Pass FILE to `tramp-tramp-file-p' if Tramp is loaded."
  (when (featurep 'tramp)
    (tramp-tramp-file-p file)))

(defun indexed-x--handle-save ()
  "Arrange to re-scan nodes and links in current buffer."
  (when indexed-mode
    (let ((% buffer-file-truename))
      (when (and (string-suffix-p ".org" %)
                 (not (backup-file-name-p %))
                 (not (indexed-x--tramp-file-p %)))
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
;;                 (cl-remove-if #'indexed-x--tramp-file-p)
;;                 ;; REVIEW: May not apply right to oldname?
;;                 (mapcar #'file-truename)
;;                 (indexed--abbrev-file-names))))

;; FIXME: Hook `indexed-x-pre-update-functions' should run,
;;        either go back to just running `indexed-x--scan-targeted' or
;;        make a "forget-file-functions" hook?
(defun indexed-x--handle-delete (file &optional _trash)
  "Arrange to forget nodes and links in FILE."
  (when indexed-mode
    (when (string-suffix-p ".org" file)
      (unless (indexed-x--tramp-file-p file)
        (setq file (indexed--abbrev-file-names file))
        ;; Used to just hand the file to `indexed-x--scan-targeted' which will
        ;; have the same effect if the file is gone, but sometimes it is not
        ;; gone, thanks to `delete-by-moving-to-trash'.
        (indexed-x--forget-files (list file))
        (indexed-x--forget-links-from
         (mapcar #'indexed-id (indexed-entries-in file)))))))

(defun indexed-x-record-entry (&rest args)
  "See `indexed-org-entry--make-obj' for ARGS."
  (let ((link (apply #'indexed-org-link--make-obj args)))
    (cl-destructuring-bind (&key dest origin) args
      
      (push link (gethash dest indexed--dest<>links))  
      (push link (gethash origin indexed--origin<>links)))))

(defun indexed-x-record-link (&rest args)
  "See `indexed-org-link--make-obj' for ARGS."
  (let ((link (apply #'indexed-org-link--make-obj args)))
    (cl-destructuring-bind (&key dest origin) args
      
      (push link (gethash dest indexed--dest<>links))  
      (push link (gethash origin indexed--origin<>links)))))

(defun indexed-x--scan-targeted (files)
  "Arrange to scan FILES."
  (when files
    (el-job-launch :id 'indexed-x
                   :inject-vars (indexed--mk-work-vars)
                   :load-features '(indexed-org-parser)
                   :funcall-per-input #'indexed-org-parser--parse-file
                   :inputs (ensure-list files)
                   :callback #'indexed-x--finalize-modified)))

(defvar indexed-x-pre-update-functions nil)
(defvar indexed-x-post-update-functions nil)
(defun indexed-x--finalize-modified (results _job)
  "Use RESULTS to update tables.
Argument JOB is the el-job object."
  (run-hook-with-args 'indexed-x-pre-update-functions results)
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
      (push link (gethash (indexed-origin link) indexed--origin<>links))
      (push link (gethash (indexed-dest link)   indexed--dest<>links))
      (run-hook-with-args 'indexed-record-link-functions link))
    (dolist (prob problems)
      (push prob indexed--problems))
    (run-hook-with-args 'indexed-x-post-update-functions results)
    (when problems
      (message "Scan had problems, see M-x org-node-list-scan-problems"))
    ;; (run-hook-with-args 'org-node-rescan-functions
    ;; (append missing-files found-files))
    ))

(defvar indexed-x-forget-file-functions nil)
(defvar indexed-x-forget-entry-functions nil)
(defun indexed-x--forget-files (files)
  "Remove from cache, info about entries in FILES.

For a thorough cleanup, you should also run
`indexed-x--forget-links-from'."
  (when (setq files (ensure-list files))
    (cl-loop
     for entry in (indexed-org-entries)
     when (member (indexed-file-name entry) files)
     do
     (remhash (indexed-id entry) indexed--id<>entry)
     (remhash (indexed-title entry) indexed--title<>id)
     (run-hook-with-args 'indexed-x-forget-entry-functions entry))
    (dolist (file files)
      (remhash file indexed--file<>data)
      (run-hook-with-args 'indexed-x-forget-file-functions file))))

(defvar indexed-x-last-removed-links nil)
;; XXX maybe rename origin to nearby-id
(defun indexed-x--forget-links-from (dead-ids)
  "Forget links with :origin matching any of DEAD-IDS.
Put the forgotten links into `indexed-x-last-removed-links'."
  (let ((dests-to-update
         (cl-loop
          for origin being each hash-key of indexed--origin<>links
          using (hash-values link-set)
          when (member origin dead-ids)
          append (mapcar (##plist-get % :dest) link-set))))
    (mapc (##remhash % indexed--origin<>links) dead-ids)
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
                 indexed--dest<>links))))

(provide 'indexed-x)

;;; indexed-x.el ends here
