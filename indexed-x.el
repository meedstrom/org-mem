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

;; Will add code here soon.

;; Support keeping the cache up to date, without doing a reset so often.

;;; Code:


;; TODO: The idle timer will detect new files appearing, created by other
;;       emacsen, but won't run the hook `org-node-rescan-functions' on them,
;;       which would be good to do.  So check for new files and then try to
;;       use `org-node--scan-targeted', since that runs the hook, but it is
;;       easy to imagine a pitfall where the list of new files is just all
;;       files, and then we do NOT want to run the hook.  So use a heuristic
;;       cutoff like 10 files.
;; (defun org-node--catch-unknown-modifications ()
;;   (let ((new (-difference (org-node-list-files) indexed-org-files))))
;;   (if (> 10 )
;;       (org-node--scan-all)
;;     (org-node--scan-targeted))
;;   )


(provide 'indexed-x)

;;; indexed-x.el ends here
