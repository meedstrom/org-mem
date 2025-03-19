;;; indexed-org-parser.el --- Gotta go fast -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Martin Edström
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is worker code meant for child processes.  It should load no
;; libraries at runtime.

;;; Code:

;; TODO: Drop the @ from @citations (needs change in several places)

;; REVIEW: Should all regexp char classes including \n (newline) also include
;; \r (carriage return)?  Looking at quite a bit of Org code, they don't seem
;; to bother.  The regexp engine translates anyway in DOS-coded buffers?

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; Tell compiler these aren't free variables
(defvar $plain-re)
(defvar $bracket-re)
(defvar $merged-re)
(defvar $global-todo-re)
(defvar $nonheritable-tags)
(defvar $inlinetask-min-level)
(defvar $structures-to-ignore) ; TODO: implement
(defvar $drawers-to-ignore) ; TODO: implement

(defvar indexed-org-parser--found-links nil
  "Link objects found so far.")

(defun indexed-org-parser--make-todo-regexp (keywords-string)
  "Build a regexp from KEYWORDS-STRING.
KEYWORDS-STRING is expected to be the sort of thing you see after
a #+todo: or #+seq_todo: or #+typ_todo: setting in an Org file.

The resulting regexp should be able to match any of
the custom TODO words thus defined."
  (thread-last keywords-string
               (replace-regexp-in-string "(.*?)" "")
               (string-replace "|" "")
               (string-trim)
               (split-string)
               (regexp-opt)))

;; Should we also use equiv of `org-link-escape'?
(defun indexed-org-parser--org-link-display-format (s)
  "Copy of `org-link-display-format'.
Format string S for display - this means replace every link inside S
with only their description if they have one, and in any case strip the
brackets."
  (replace-regexp-in-string
   $bracket-re
   (lambda (m) (or (match-string 2 m) (match-string 1 m)))
   s nil t))

(defvar indexed-org-parser--heading-re (rx bol (repeat 1 14 "*") " "))
(defun indexed-org-parser--next-heading ()
  "Similar to `outline-next-heading'."
  (if (and (bolp) (not (eobp)))
      ;; Prevent matching the same line forever
      (forward-char))
  (if (re-search-forward indexed-org-parser--heading-re nil 'move)
      (goto-char (pos-bol))))

(defun indexed-org-parser--collect-links-until (end id-here file)
  "From here to buffer position END, look for forward-links.

Argument ID-HERE is the ID of the subtree where this function is being
executed (or that of an ancestor heading, if the current subtree has
none), to be included in each link's metadata.  FILE likewise.

It is important that END does not extend past any sub-heading, as
the subheading potentially has an ID of its own."
  (let ((beg (point))
        link-type path link-pos)
    ;; Here it may help to know that:
    ;; - `$plain-re' will be morally the same as `org-link-plain-re'
    ;; - `$merged-re' merges the above with `org-link-bracket-re'
    (while (re-search-forward $merged-re end t)
      ;; Record same position that `org-roam-db-map-links' would
      (setq link-pos (- (match-end 0) 1))
      (if (setq path (match-string 1))
          ;; Link is the [[bracketed]] kind.  Is there an URI: style link
          ;; inside?  Here is the magic that allows links to have spaces, it is
          ;; not possible with $plain-re alone.
          (if (string-match $plain-re path)
              (setq link-type (match-string 1 path)
                    path (string-trim-left path ".*?:"))
            ;; Nothing of interest between the brackets
            (setq link-type nil))
        ;; Link is the unbracketed kind
        (setq link-type (match-string 3)
              path (match-string 4)))
      (when link-type
        (unless (save-excursion
                  ;; If point is on a # comment line, skip
                  (goto-char (pos-bol))
                  (looking-at-p "[\t\s]*# "))
          ;; Special case: Org 9.7 `org-id-link-use-context'...
          (when (and (equal link-type "id"))
            (let ((chop (string-search "::" path)))
              (when chop (setq path (substring path 0 chop)))))
          (push (record 'indexed-org-link
                        (string-replace "%20" " " path)
                        file
                        id-here
                        link-pos
                        link-type)
                indexed-org-parser--found-links))))

    ;; Start over and look for @citekeys
    (goto-char beg)
    (while (search-forward "[cite" end t)
      (let ((closing-bracket (save-excursion (search-forward "]" end t))))
        (if closing-bracket
            ;; Use a modified `org-element-citation-key-re'
            (while (re-search-forward "[&@][!#-+./:<>-@^-`{-~[:word:]-]+"
                                      closing-bracket
                                      t)
              ;; Record same position that `org-roam-db-map-citations' would
              (setq link-pos (1+ (match-beginning 0)))
              (if (save-excursion
                    (goto-char (pos-bol))
                    (looking-at-p "[\t\s]*# "))
                  ;; On a # comment, skip citation
                  (goto-char closing-bracket)
                (push (record 'indexed-org-link
                              ;; Replace & with @
                              (concat "@" (substring (match-string 0) 1))
                              file
                              id-here
                              link-pos
                              nil)
                      indexed-org-parser--found-links)))
          (error "No closing bracket to [cite:")))))
  (goto-char (or end (point-max))))

(defun indexed-org-parser--collect-properties (beg end)
  "Collect Org properties between BEG and END into a plist.
Assumes BEG and END are buffer positions delimiting a region in
between buffer substrings \":PROPERTIES:\" and \":END:\"."
  (let (result pos-start pos-eol)
    (goto-char beg)
    (while (< (point) end)
      (skip-chars-forward "\t\s")
      (unless (looking-at-p ":")
        (error "Possibly malformed property drawer"))
      (forward-char)
      (setq pos-start (point))
      (setq pos-eol (pos-eol))
      (or (search-forward ":" pos-eol t)
          (error "Possibly malformed property drawer"))
      (unless (= pos-eol (point))
        ;; Let's just not collect property lines like
        ;; :header-args:emacs-lisp+: :results silent :noweb yes :var end=9
        (when (looking-at-p " ")
          (push (cons (upcase (buffer-substring pos-start (1- (point))))
                      (string-trim (buffer-substring (point) pos-eol)))
                result)))
      (forward-line 1))
    result))


;;; Main

(defvar indexed-org-parser--buf nil)
(defun indexed-org-parser--init-buf-and-switch ()
  "Setup a throwaway buffer in which to work and make it current.
Also set some variables, including global variables."
  (switch-to-buffer (get-buffer-create " *indexed-org-parser*" t))
  (setq indexed-org-parser--buf (current-buffer))
  (setq buffer-read-only t)
  (setq case-fold-search t)
  (setq file-name-handler-alist nil)
  (when $inlinetask-min-level
    (setq indexed-org-parser--heading-re
          (rx-to-string
           `(seq bol (repeat 1 ,(1- $inlinetask-min-level) "*") " ")))))

(defun indexed-org-parser--parse-file (FILE)
  "Gather entries, links and other data in FILE."
  (unless (equal indexed-org-parser--buf (current-buffer))
    (indexed-org-parser--init-buf-and-switch))
  (setq indexed-org-parser--found-links nil)
  (let ((file-todo-option-re
         (rx bol (* space) (or "#+todo: " "#+seq_todo: " "#+typ_todo: ")))
        missing-file
        found-entries
        file-data
        problem
        HEADING-POS HERE FAR END ID-HERE ID FILE-ID CRUMBS
        DRAWER-BEG DRAWER-END
        TITLE FILE-TITLE LNUM
        TODO-STATE TODO-RE FILE-TODO-SETTINGS
        TAGS FILE-TAGS HERITABLE-TAGS
        SCHED DEADLINE PRIORITY LEVEL PROPS)
    (condition-case err
        (catch 'file-done
          (when (not (file-readable-p FILE))
            ;; FILE does not exist, user probably deleted or renamed a file.
            (setq missing-file FILE)
            (throw 'file-done t))
          ;; Skip symlinks, they cause duplicates if the true file is also in
          ;; the file list.  Note that symlinks should not be treated how we
          ;; treat missing files.
          (when (file-symlink-p FILE)
            (throw 'file-done t))
          ;; NOTE: Don't use `insert-file-contents-literally'!  It sets
          ;; `coding-system-for-read' to `no-conversion', which results in
          ;; wrong values for HEADING-POS when the file contains any Unicode.
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert-file-contents FILE))
          (goto-char 1)

          ;; If the very first line of file is a heading, don't try to scan any
          ;; file-level front matter.  Our usage of
          ;; `indexed-org-parser--next-heading' cannot handle that edge-case.
          (if (looking-at-p "\\*")
              (progn
                (setq FILE-ID nil)
                (setq FILE-TITLE nil)
                (setq TODO-RE $global-todo-re))
            ;; Narrow until first heading
            (when (indexed-org-parser--next-heading)
              (narrow-to-region 1 (point))
              (goto-char 1))
            ;; Rough equivalent of `org-end-of-meta-data' for the file
            ;; level front matter, can jump somewhat too far but that's ok
            (setq FAR (if (re-search-forward "^ *?[^#:\n]" nil t)
                          (1- (point))
                        ;; There's no content other than front matter
                        (point-max)))
            (goto-char 1)
            (setq PROPS
                  (if (re-search-forward "^[\t\s]*:properties:" FAR t)
                      (progn
                        (forward-line 1)
                        (indexed-org-parser--collect-properties
                         (point)
                         (if (re-search-forward "^[\t\s]*:end:" FAR t)
                             (pos-bol)
                           (error "Couldn't find :END: of drawer"))))
                    nil))
            (setq HERE (point))
            (setq FILE-TAGS
                  (if (re-search-forward "^#\\+filetags: " FAR t)
                      (split-string
                       (buffer-substring (point) (pos-eol))
                       ":" t)
                    nil))
            (goto-char HERE)
            (setq TODO-RE
                  (if (re-search-forward file-todo-option-re FAR t)
                      (progn
                        (setq FILE-TODO-SETTINGS nil)
                        ;; Because you can have multiple #+todo: lines...
                        (while (progn
                                 (push (buffer-substring (point) (pos-eol))
                                       FILE-TODO-SETTINGS)
                                 (re-search-forward
                                  file-todo-option-re FAR t)))
                        (indexed-org-parser--make-todo-regexp
                         (string-join FILE-TODO-SETTINGS " ")))
                    $global-todo-re))
            (goto-char HERE)
            (setq FILE-TITLE (when (re-search-forward "^#\\+title: +" FAR t)
                               (string-trim-right
                                (indexed-org-parser--org-link-display-format
                                 (buffer-substring (point) (pos-eol))))))
            (setq FILE-ID (cdr (assoc "ID" PROPS)))
            (goto-char HERE)
            ;; Don't count org-super-links backlinks as forward links
            ;; TODO: Rewrite more readably
            (if (re-search-forward "^[	 ]*:BACKLINKS:" nil t)
                (progn
                  (setq END (point))
                  (unless (search-forward ":end:" nil t)
                    (error "Couldn't find :END: of drawer"))
                  ;; Collect from end of backlinks drawer to first heading
                  (indexed-org-parser--collect-links-until nil FILE-ID FILE))
              (setq END (point-max)))
            (goto-char HERE)
            (indexed-org-parser--collect-links-until END FILE-ID FILE)
            (push (record 'indexed-org-entry
                          nil
                          FILE
                          0
                          FILE-ID
                          1
                          nil
                          1
                          nil
                          PROPS
                          nil
                          nil
                          FILE-TAGS
                          (or FILE-TITLE (file-name-nondirectory FILE))
                          nil)
                  found-entries)
            (goto-char (point-max))
            ;; We should now be at the first heading
            (widen))

          ;; Prep
          (setq LNUM (line-number-at-pos))
          (setq CRUMBS nil)
          (setq FILE-TAGS (cl-loop for tag in FILE-TAGS
                                   unless (member tag $nonheritable-tags)
                                   collect tag))

          ;; Loop over the file's headings
          (while (not (eobp))
            (catch 'entry-done
              ;; Narrow til next heading
              (narrow-to-region (point)
                                (save-excursion
                                  (or (indexed-org-parser--next-heading)
                                      (point-max))))
              (setq HEADING-POS (point))
              (setq LEVEL (skip-chars-forward "*"))
              (skip-chars-forward " ")
              (let ((case-fold-search nil))
                (setq TODO-STATE
                      (if (looking-at TODO-RE)
                          (prog1 (buffer-substring (point) (match-end 0))
                            (goto-char (match-end 0))
                            (skip-chars-forward " "))
                        nil))
                ;; [#A] [#B] [#C]
                (setq PRIORITY
                      (if (looking-at "\\[#[A-Z0-9]+\\]")
                          (prog1 (match-string 0)
                            (goto-char (match-end 0))
                            (skip-chars-forward " "))
                        nil)))
              ;; Skip statistics-cookie such as "[2/10]"
              (when (looking-at "\\[[0-9]*/[0-9]*\\]")
                (goto-char (match-end 0))
                (skip-chars-forward " "))
              (setq HERE (point))
              ;; Any tags in heading?
              (if (re-search-forward " +:.+: *$" (pos-eol) t)
                  (progn
                    (goto-char (match-beginning 0))
                    (setq TAGS (split-string (match-string 0) ":" t " *"))
                    (setq TITLE (string-trim-right
                                 (indexed-org-parser--org-link-display-format
                                  (buffer-substring HERE (point))))))
                (setq TAGS nil)
                (setq TITLE (string-trim-right
                             (indexed-org-parser--org-link-display-format
                              (buffer-substring HERE (pos-eol))))))
              ;; Gotta go forward 1 line, see if it is a planning-line, and
              ;; if it is, then go forward 1 more line, and if that is a
              ;; :PROPERTIES: line, then we're safe to collect properties
              (forward-line 1)
              (setq HERE (point))
              (setq FAR (pos-eol))
              (setq SCHED
                    (if (re-search-forward "[\t\s]*SCHEDULED: +" FAR t)
                        (prog1 (buffer-substring
                                (point)
                                (+ 1 (point) (skip-chars-forward "^]>\n")))
                          (goto-char HERE))
                      nil))
              (and SCHED (length< SCHED 11) (error "Malformed SCHEDULED"))
              (setq DEADLINE
                    (if (re-search-forward "[\t\s]*DEADLINE: +" FAR t)
                        (prog1 (buffer-substring
                                (point)
                                (+ 1 (point) (skip-chars-forward "^]>\n")))
                          (goto-char HERE))
                      nil))
              (and DEADLINE (length< DEADLINE 11) (error "Malformed DEADLINE"))
              (when (or SCHED
                        DEADLINE
                        (re-search-forward "[\t\s]*CLOSED: +" FAR t))
                ;; Alright, so there was a planning-line, meaning any
                ;; :PROPERTIES: are not on this line but the next.
                (forward-line 1)
                (setq FAR (pos-eol)))
              (skip-chars-forward "\t\s")
              (setq PROPS
                    (if (looking-at-p ":properties:")
                        (progn
                          (forward-line 1)
                          (indexed-org-parser--collect-properties
                           (point)
                           (if (re-search-forward "^[\t\s]*:end:" nil t)
                               (pos-bol)
                             (error "Couldn't find :END: of drawer"))))
                      nil))
              (setq ID (cdr (assoc "ID" PROPS)))
              (setq HERITABLE-TAGS
                    (cl-loop for tag in TAGS
                             unless (member tag $nonheritable-tags)
                             collect tag))
              ;; CRUMBS is a list that can look like
              ;;    ((3 "Heading" "id1234" ("noexport" "work" "urgent"))
              ;;     (2 "Another heading" "id6532" ("work"))
              ;;     (... ... ... ...))
              ;; if the previous heading looked like
              ;;    *** Heading  :noexport:work:urgent:
              ;;       :PROPERTIES:
              ;;       :ID: id1234
              ;;       :END:
              ;; It lets us track context so we know the outline path to the
              ;; current entry and what tags it should be able to inherit.
              ;; Update the list.
              (cl-loop until (> LEVEL (or (caar CRUMBS) 0))
                       do (pop CRUMBS)
                       finally do
                       (push (list LEVEL TITLE ID HERITABLE-TAGS)
                             CRUMBS))
              (push (record 'indexed-org-entry
                            nil
                            FILE
                            LEVEL
                            ID
                            LNUM
                            (nreverse (mapcar #'cadr (cdr CRUMBS)))
                            HEADING-POS
                            PRIORITY
                            PROPS
                            SCHED
                            (delete-dups
                             (apply #'append
                                    FILE-TAGS
                                    (mapcar #'cadddr (cdr CRUMBS))))
                            TAGS
                            TITLE
                            TODO-STATE)
                    found-entries)

              ;; Heading analyzed, now collect links in entry body!

              (setq ID-HERE
                    (or ID
                        (cl-loop for crumb in CRUMBS thereis (caddr crumb))
                        FILE-ID
                        (throw 'entry-done t)))
              (setq HERE (point))
              ;; Don't count org-super-links backlinks.
              ;; TODO: Generalize this mechanism, use configurable lists
              ;; `$structures-to-ignore' and `$drawers-to-ignore'
              (setq DRAWER-BEG (re-search-forward "^[	 ]*:BACKLINKS:" nil t))
              (setq DRAWER-END
                    (and DRAWER-BEG
                         (or (search-forward ":end:" nil t)
                             (error "Couldn't find :END: of drawer"))))

              ;; Collect links inside the heading
              (goto-char HEADING-POS)
              (indexed-org-parser--collect-links-until (pos-eol) ID-HERE FILE)
              ;; Collect links between property drawer and backlinks drawer
              (goto-char HERE)
              (when DRAWER-BEG
                (indexed-org-parser--collect-links-until DRAWER-BEG ID-HERE FILE))
              ;; Collect links until next heading
              (goto-char (or DRAWER-END HERE))
              (indexed-org-parser--collect-links-until (point-max) ID-HERE FILE))
            (goto-char (point-max))
            (setq LNUM (+ (- LNUM 1) (line-number-at-pos)))
            (widen))

          (setq file-data
                (record 'indexed-file-data
                        FILE
                        FILE-TITLE
                        LNUM
                        ;; Use integer mtime for `eq' operations
                        (ceiling (float-time
                                  (file-attribute-modification-time
                                   (file-attributes FILE))))
                        (point)
                        FILE-ID)))
      
      ;; Don't crash when there is an error signal, just report it.
      ;; Could allow for plural problems here, but one per file is plenty
      (( t error )
       (setq problem (list (format-time-string "%H:%M") FILE (point) err))))

    (list (if missing-file (list missing-file))
          (if file-data (list file-data))
          found-entries
          indexed-org-parser--found-links
          (if problem (list problem)))))

(provide 'indexed-org-parser)

;;; indexed-org-parser.el ends here
