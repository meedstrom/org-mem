;;; indexed-roam.el --- RENAMED -*- lexical-binding: t; -*-

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

(require 'org-mem-roamy)
(define-obsolete-function-alias 'indexed-roam-mode          #'org-mem-roamy-db-mode       "0.7.0 (2025-05-11)")
(define-obsolete-function-alias 'indexed-roam               #'org-mem-roamy-db            "0.7.0 (2025-05-11)")
(define-obsolete-function-alias 'indexed-roam--update-db    #'org-mem-roamy--update-db    "0.7.0 (2025-05-11)")
(define-obsolete-function-alias 'indexed-roam-mk-node       #'org-mem-roamy-mk-node       "0.7.0 (2025-05-11)")
(define-obsolete-function-alias 'indexed-roam-mk-backlinks  #'org-mem-roamy-mk-backlinks  "0.7.0 (2025-05-11)")
(define-obsolete-function-alias 'indexed-roam-mk-reflinks   #'org-mem-roamy-mk-reflinks   "0.7.0 (2025-05-11)")
(provide 'indexed-roam)

;;; indexed-roam.el ends here
