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

(require 'ert)
(require 'find-func)
(require 'llama)
(require 'org-mem)
(require 'org-mem-roamy)
(require 'org-mem-x)

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

;;; org-mem-test.el ends here
