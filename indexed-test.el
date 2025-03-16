;;; indexed-test.el ---  -*- lexical-binding: t; -*-
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

;;; Code:

(require 'ert)
(require 'seq)
(require 'find-func)
(require 'llama)
(require 'indexed)
(require 'indexed-roam)
(require 'indexed-x)

;; FIXME
(ert-deftest test-split-refs-field ()
  (let ((result
         (with-current-buffer (setq indexed-roam--work-buf
                                    (get-buffer-create " *indexed-roam*" t))
           (indexed-roam--split-refs-field
            (concat " \"[cite:@citekey abcd ; @citekey2 cdefgh;@citekey3]\""
                    " \"[[citep:&citekey4 abcd ; &citekey5 cdefgh;&citekey6]]\""
                    " \"[[https://gnu.org/A Link With Spaces/index2.htm]]\""
                    " [[https://gnu.org/A Link With Spaces/index.htm][baz]]"
                    " https://gnu.org [cite:&citekey7]  @foo &bar "
                    " info:with%20escaped%20spaces")))))
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
                                       org-node-parser--paths-types))))
    (should (equal "https" (cdr (assoc "//gnu.org"
                                       org-node-parser--paths-types))))
    (should (equal nil (cdr (assoc "@citekey"
                                   org-node-parser--paths-types))))
    (should (equal nil (cdr (assoc "citekey"
                                   org-node-parser--paths-types))))))

;;; indexed-test.el ends here
