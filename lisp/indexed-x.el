;;; indexed-x.el --- RENAMED -*- lexical-binding: t; -*-

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

;;; Code:

(require 'org-mem-updater)
(define-obsolete-function-alias 'indexed-x--handle-save                #'org-mem-updater--handle-save                 "0.7.0 (2025-05-11)")
(define-obsolete-function-alias 'indexed-x--handle-rename              #'org-mem-updater--handle-rename               "0.7.0 (2025-05-11)")
(define-obsolete-function-alias 'indexed-x--handle-delete              #'org-mem-updater--handle-delete               "0.7.0 (2025-05-11)")
(define-obsolete-function-alias 'indexed-x-ensure-buffer-file-known    #'org-mem-updater-ensure-buffer-file-known     "0.7.0 (2025-05-11)")
(define-obsolete-function-alias 'indexed-x-ensure-link-at-point-known  #'org-mem-updater-ensure-link-at-point-known   "0.7.0 (2025-05-11)")
(define-obsolete-function-alias 'indexed-x-ensure-entry-at-point-known #'org-mem-updater-ensure-entry-at-point-known  "0.7.0 (2025-05-11)")
(define-obsolete-function-alias 'indexed--activate-timer               #'org-mem-updater--activate-timer              "0.7.0 (2025-05-11)")
(define-obsolete-function-alias 'indexed-updater-mode                  #'org-mem-updater-mode                         "0.7.0 (2025-05-11)")
(provide 'indexed-x)

;;; indexed-x.el ends here
