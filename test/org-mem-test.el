;;; org-mem-test.el ---  -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'find-func)
(require 'llama)
(require 'org-mem)
(require 'org-mem-roamy)
(require 'org-mem-updater)

(ert-deftest test-split-refs-field ()
  (let ((result
         (org-mem--split-roam-refs-field
            (concat " \"[cite:@citekey abcd ; @citekey2 cdefgh;@citekey3]\""
                    " \"[[citep:&citekey4 abcd ; &citekey5 cdefgh;&citekey6]]\""
                    " \"[[https://gnu.org/A Link With Spaces/index2.htm]]\""
                    " [[https://gnu.org/A Link With Spaces/index.htm][baz]]"
                    " https://gnu.org [cite:&citekey7]  @foo &bar "
                    " info:with%20escaped%20spaces"))))
    (should (cl-every (##member % result)
                      '("@citekey"
                        "@citekey4"
                        "@citekey7"
                        "@foo"
                        "@bar"
                        "with escaped spaces"
                        "//gnu.org/A Link With Spaces/index.htm"
                        "//gnu.org/A Link With Spaces/index2.htm"
                        "//gnu.org")))
    (should (equal "https" (cdr (assoc "//gnu.org/A Link With Spaces/index.htm"
                                       org-mem--roam-ref<>type))))
    (should (equal "https" (cdr (assoc "//gnu.org"
                                       org-mem--roam-ref<>type))))
    (should (equal nil (cdr (assoc "@citekey"
                                   org-mem--roam-ref<>type))))
    (should (equal nil (cdr (assoc "citekey"
                                   org-mem--roam-ref<>type))))))

(defun org-mem-test-clock-p (clock)
  (if (length= clock 1)
      (iso8601-parse (nth 0 clock))
    (and (iso8601-parse (nth 0 clock))
         (iso8601-parse (nth 1 clock))
         (natnump (nth 2 clock)))))

(defun org-mem-test-clock-int-p (clock)
  (if (length= clock 1)
      (natnump (nth 0 clock))
    (and (length= clock 3)
         (natnump (nth 0 clock))
         (natnump (nth 1 clock))
         (natnump (nth 2 clock)))))

(ert-deftest various ()
  ;; TODO: Supply some sample files
  ;; (let ((org-mem-watch-dirs (list (expand-file-name ".")))
  ;;       (org-mem-do-sync-with-org-id)
  ;;       (org-id-locations (make-hash-table :test 'equal))
  ;;       (org-id-extra-files nil))
  ;;   (org-mem-reset)
  ;;   )

  (should (seq-every-p #'stringp (org-mem-all-ids)))
  (should (seq-every-p #'org-mem-entry-p (org-mem-all-entries)))
  (should (seq-every-p #'org-mem-entry-p (org-mem-all-id-nodes)))
  (should (seq-every-p #'org-mem-entry-p (org-mem-all-entries-with-active-timestamps)))
  (should (seq-every-p #'org-mem-entry-p (org-mem-all-entries-with-dangling-clock)))
  (should (seq-every-p #'org-mem-entry-p (mapcar #'org-mem-entry-by-id (org-mem-all-ids))))
  (should (seq-every-p #'org-mem-link-p (org-mem-all-links)))
  (should (seq-every-p #'org-mem-link-p (org-mem-all-id-links)))
  (should (seq-every-p #'file-exists-p (org-mem-all-files)))
  (should-not (seq-find #'file-symlink-p (org-mem-all-files)))

  (dolist (e (org-mem-all-entries))
    (should (file-equal-p (org-mem-file-truename e) (org-mem-file e)))
    (should (stringp (org-mem-title e)))
    (should (natnump (org-mem-level e)))
    (should (natnump (org-mem-lnum e)))
    (should (natnump (org-mem-pos e)))
    (should (booleanp (org-mem-subtree-p e)))
    (should (seq-every-p #'stringp (org-mem-tags e)))
    (should (seq-set-equal-p (org-mem-tags e)
                             (append (org-mem-tags-local e)
                                     (org-mem-tags-inherited e))))
    (should (seq-every-p #'org-mem-test-clock-int-p (org-mem-clocks-int e)))
    (should (seq-every-p #'org-mem-test-clock-p (org-mem-clocks e)))
    (should (seq-every-p #'natnump (org-mem-active-timestamps-int e)))
    (should (seq-every-p #'iso8601-parse (org-mem-active-timestamps e)))
    (should (seq-every-p #'stringp (org-mem-olpath e)))
    (should (seq-every-p #'stringp (org-mem-olpath-with-self e)))
    (should (seq-every-p #'stringp (org-mem-olpath-with-self-with-file-title e)))
    (should (seq-every-p #'stringp (org-mem-olpath-with-self-with-file-title e t)))
    (should (seq-every-p #'stringp (org-mem-olpath-with-file-title e)))
    (should (seq-every-p #'stringp (org-mem-olpath-with-file-title e t)))
    (should (seq-every-p #'stringp (org-mem-roam-aliases e)))
    (should (seq-every-p #'stringp (org-mem-roam-refs e)))
    (when (org-mem-title-maybe e) (should (stringp (org-mem-title-maybe e))))
    (when (org-mem-priority e)    (should (stringp (org-mem-priority e))))
    (when (org-mem-text e)        (should (stringp (org-mem-text e))))
    (when (org-mem-todo-state e)  (should (stringp (org-mem-todo-state e))))

    (when (or (org-mem-deadline-int e)
              (org-mem-deadline e))
      (should (natnump (org-mem-deadline-int e)))
      (should (iso8601-parse (org-mem-deadline e))))
    (when (or (org-mem-scheduled-int e)
              (org-mem-scheduled e))
      (should (natnump (org-mem-scheduled-int e)))
      (should (iso8601-parse (org-mem-scheduled e))))
    (when (or (org-mem-closed-int e)
              (org-mem-closed e))
      (should (natnump (org-mem-closed-int e)))
      (should (iso8601-parse (org-mem-closed e))))

    (cl-loop for (key . value) in (append (org-mem-properties e)
                                          (org-mem-properties-inherited e))
             do (should (and (stringp key)
                             (or (stringp value) (null value))))))

  (dolist (l (org-mem-all-links))
    (should (file-equal-p (org-mem-file-truename l) (org-mem-file l)))
    (should (stringp (org-mem-link-target l)))
    (should (natnump (org-mem-link-pos l)))
    (should (natnump (org-mem-link--internal-entry-id l)))
    (should (booleanp (org-mem-link-citation-p l)))
    (when (org-mem-link-type l)        (should (stringp (org-mem-link-type l))))
    (when (org-mem-link-description l) (should (stringp (org-mem-link-description l))))
    (when (org-mem-link-nearby-id l)   (should (stringp (org-mem-link-nearby-id l))))))

;;; org-mem-test.el ends here
