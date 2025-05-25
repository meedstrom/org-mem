;;; org-mem-parser.el --- Gotta go fast -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.
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

;; This file is worker code meant for child processes.
;; It should load no libraries at runtime nor enable any major mode.

;;; Code:

;; TODO: See org manual on property syntax.  It implies you can set file-level
;; id without a prop drawer, just a line #+PROPERTY: id asdfghjkl1234.
;; Let's support that, that's neat.

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; Tell compiler these aren't free variables
(defvar $plain-re)
(defvar $bracket-re)
(defvar $merged-re)
(defvar $do-cache-text)
(defvar $default-todo-re)
(defvar $nonheritable-tags)
(defvar $inlinetask-min-level)
(defvar $use-tag-inheritance)
(defvar $structures-to-ignore) ; TODO: implement
(defvar $drawers-to-ignore) ; TODO: implement

(defvar org-mem-parser--found-links nil
  "Link objects found so far.")

(defvar org-mem-parser--found-timestamps nil
  "Active timestamps found so far.")

(defvar org-mem-parser--all-dir-locals nil
  "Dir-local variables found so far.")

(defun org-mem-parser--make-todo-regexp (keywords-string)
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

(defun org-mem-parser--mk-id (file-name pos)
  "Reduce FILE-NAME and POS into an `eq'-safe probably-unique fixnum."
  (+ (string-to-number (substring (secure-hash 'md5 file-name)
                                  -7)
                       16)
     pos))

;; REVIEW: Should we also use an equivalent of `org-link-escape'?
(defun org-mem-parser--org-link-display-format (s)
  "Copy of `org-link-display-format'.
Format string S for display - this means replace every link inside S
with only their description if they have one, and in any case strip the
brackets."
  (replace-regexp-in-string
   $bracket-re
   (lambda (m) (or (match-string 2 m) (match-string 1 m)))
   s nil t))

;; REVIEW: I wonder what's the most handy way to search on dates/times with
;;         SQL?  Do people separate date and time into different columns?  Or
;;         store as numbers and use some SQL functions to compare them?
;;         Just mirroring the data format of org-roam now.

(defconst org-mem-parser--org-ts-regexp0
  "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\( +[^]+0-9>\r\n -]+\\)?\\( +\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Copy of `org-ts-regexp0'.")

(defun org-mem-parser--stamp-to-iso8601 (s)
  "Parse the first Org timestamp in string S and return as ISO8601."
  (let ((time
         ;; Code from `org-parse-time-string', which claims to be fast.
         (if (not (string-match org-mem-parser--org-ts-regexp0 s))
             (error "Not an Org time string: %s" s)
           (list 0
	         (cond ((match-beginning 8)
                        (string-to-number (match-string 8 s)))
	               (t 0))
	         (cond ((match-beginning 7)
                        (string-to-number (match-string 7 s)))
	               (t 0))
	         (string-to-number (match-string 4 s))
	         (string-to-number (match-string 3 s))
	         (string-to-number (match-string 2 s))
	         nil -1 nil))))
    (when time
      (format-time-string "%FT%H:%M" (encode-time time)))))

(defvar org-mem-parser--heading-re nil)
(defun org-mem-parser--next-heading ()
  "Similar to `outline-next-heading'."
  (if (and (bolp) (not (eobp)))
      ;; Prevent matching the same line forever
      (forward-char))
  (if (re-search-forward org-mem-parser--heading-re nil 'move)
      (goto-char (pos-bol))))

(defconst org-mem--org-ts-regexp
  "<\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?\\)>")

(defun org-mem-parser--collect-links-until (end id-here file internal-entry-id)
  "From here to buffer position END, look for forward-links.

Argument ID-HERE is the ID of the subtree where this function is being
executed (or that of an ancestor heading, if the current subtree has
none), to be included in each link's metadata.  FILE and
INTERNAL-ENTRY-ID likewise.

It is important that END does not extend past any sub-heading, as
the subheading potentially has an ID of its own."
  (let ((beg (point))
        LINK-TYPE LINK-PATH LINK-POS LINK-DESC)
    ;; Here it may help to know that:
    ;; - `$plain-re' will be morally the same as `org-link-plain-re'
    ;; - `$merged-re' merges the above with `org-link-bracket-re'
    (while (re-search-forward $merged-re end t)
      ;; Record same position that `org-roam-db-map-links' does.
      (setq LINK-POS (- (match-end 0) 1))
      (setq LINK-PATH (match-string 1))
      (setq LINK-DESC (match-string 2))
      (if LINK-PATH
          ;; Link is the [[bracketed]] kind.
          (let ((colon-pos (string-search ":" LINK-PATH)))
            (if (and colon-pos
                     (length> LINK-PATH colon-pos)
                     (not (eq ?: (aref LINK-PATH (1+ colon-pos))))
                     (not (eq ?\s (aref LINK-PATH (1+ colon-pos)))))
                ;; Guess that this is a valid URI: type link
                (setq LINK-TYPE (substring LINK-PATH 0 colon-pos)
                      LINK-PATH (substring LINK-PATH (1+ colon-pos)))
              (setq LINK-TYPE nil)))
        ;; Link is the unbracketed kind and matched `org-mem-seek-link-types'.
        (setq LINK-TYPE (match-string 3)
              LINK-PATH (match-string 4)))

      (unless (save-excursion
                ;; If point is in a # comment line, skip
                (goto-char (pos-bol))
                (looking-at-p "[\s\t]*# "))
        ;; Handle a special case opened by Org 9.7 `org-id-link-use-context'
        (when (and (equal LINK-TYPE "id"))
          (let ((chop (string-search "::" LINK-PATH)))
            (when chop (setq LINK-PATH (substring LINK-PATH 0 chop)))))
        (push (record 'org-mem-link
                      file
                      LINK-POS
                      LINK-TYPE
                      (string-replace "%20" " " LINK-PATH) ; nicety, but will regret
                      LINK-DESC
                      nil
                      id-here
                      internal-entry-id)
              org-mem-parser--found-links)
        ;; TODO: Fish any org-ref v3 &citekeys out of LINK-PATH and make a new link
        ;;       object for each.  Then stop including &citekeys in below step.
        ))

    ;; Start over and look for @citekeys
    ;; TODO: Make less permissive after we only look for org 9.5 citations here
    (goto-char beg)
    (while (search-forward "[cite" end t)
      (when-let* ((closing-bracket (save-excursion
                                     (or (search-forward "]" end t)
                                         (error "No closing bracket to [cite:"))))
                  (colon (search-forward ":" closing-bracket t)))
        ;; Use a modified `org-element-citation-key-re'
        (while (re-search-forward "[&@][!#-+./:<>-@^-`{-~[:word:]-]+"
                                  closing-bracket
                                  t)
          ;; Record same position that `org-roam-db-map-citations' does
          (setq LINK-POS (1+ (match-beginning 0)))
          (setq LINK-DESC (buffer-substring colon (1- closing-bracket)))
          (if (save-excursion
                (goto-char (pos-bol))
                (looking-at-p "[\s\t]*# "))
              ;; On a # comment, skip citation
              (goto-char closing-bracket)
            (push (record 'org-mem-link
                          file
                          LINK-POS
                          LINK-DESC
                          "cite"
                          ;; Replace & with @ like `org-mem--split-roam-refs-field'
                          (concat "@" (substring (match-string 0) 1))
                          t
                          id-here
                          internal-entry-id)
                  org-mem-parser--found-links)))))

    ;; New 2025-05-23: Start over and look for active timestamps
    (goto-char beg)
    (while (re-search-forward org-mem--org-ts-regexp end t)
      (push (org-mem-parser--stamp-to-iso8601 (match-string 0))
            org-mem-parser--found-timestamps)))
  (goto-char (or end (point-max))))

(defun org-mem-parser--collect-properties (beg end)
  "Collect Org properties between BEG and END into an alist.
Assumes BEG and END are buffer positions delimiting a region in
between buffer substrings \":PROPERTIES:\" and \":END:\"."
  (let (result START EOL)
    (goto-char beg)
    (while (< (point) end)
      (skip-chars-forward "\s\t")
      (unless (looking-at-p ":")
        (error "Possibly malformed property drawer"))
      (forward-char)
      (when (eolp)
        (error "Possibly malformed property drawer"))
      (setq START (point))
      (setq EOL (pos-eol))
      (or (search-forward ":" EOL t)
          (error "Possibly malformed property drawer"))
      ;; In the wild you see some :MULTIPLE:COLONS:IN:NAME: properties,
      ;; but they are illegal according to `org-property-drawer-re'.
      ;; So no need to handle that case, skip.
      (when (looking-at-p " ")
        (push (cons (upcase (buffer-substring START (1- (point))))
                    (string-trim (buffer-substring (point) EOL)))
              result))
      (forward-line 1))
    result))


;;; Main

(defvar org-mem-parser--buf nil)
(defun org-mem-parser--parse-file (file)
  "Gather entries, links and other data in FILE."
  ;; This condition should fail in the normal case (running in a subprocess),
  ;; but it lets us debug this function in the main process.
  (when (fboundp 'org-mem--mk-work-vars)
    (dolist (var (org-mem--mk-work-vars))
      (set (car var) (cdr var))))
  (unless (eq org-mem-parser--buf (current-buffer))
    (switch-to-buffer
     (setq org-mem-parser--buf (get-buffer-create " *org-mem-parser*" t))))
  (setq org-mem-parser--found-links nil)
  (setq org-mem-parser--found-timestamps nil)
  (unless org-mem-parser--heading-re
    (setq org-mem-parser--heading-re
          (if $inlinetask-min-level
              (rx-to-string
               `(seq bol (repeat 1 ,(1- $inlinetask-min-level) "*") " "))
            (rx bol (repeat 1 14 "*") " "))))
  (let ((file-name-handler-alist nil)
        (case-fold-search t)
        (buffer-read-only t)
        (file-todo-option-re
         (rx bol (* space) (or "#+todo: " "#+seq_todo: " "#+typ_todo: ")))
        bad-path
        found-entries
        file-data
        problem
        attrs
        coding
        ;; Upcased names change value a lot, take care to keep correct.
        ID ID-HERE INTERNAL-ENTRY-ID
        TAGS USE-TAG-INHERITANCE NONHERITABLE-TAGS
        TITLE HEADING-POS LNUM CRUMBS
        TODO-STATE TODO-RE
        SCHED DEADLINE CLOSED PRIORITY LEVEL PROPS
        ;; Arbitrarily-named buffer positions
        HERE FAR END DRAWER-BEG DRAWER-END)
    (condition-case err
        (progn
          (when (not (file-exists-p file))
            (setq bad-path file)
            (signal 'skip-file t))
          (when (file-symlink-p file)
            (setq bad-path file)
            (signal 'skip-file t))
          (when (not (file-readable-p file))
            ;; NOTE: Don't declare it bad, that'd delist it from
            ;;       org-id-locations, which the user may not want.
            (error "File not readable"))
          ;; NOTE: Don't use `insert-file-contents-literally'!  It sets
          ;; `coding-system-for-read' to `no-conversion', which results in
          ;; wrong values for HEADING-POS when the file contains any multibyte.
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert-file-contents file)
            (setq coding last-coding-system-used))
          (setq INTERNAL-ENTRY-ID (org-mem-parser--mk-id file 0))
          (setq TODO-RE $default-todo-re)
          (setq attrs (file-attributes file))
          ;; To amend later if we make it to the end.
          (setq file-data (list file attrs -1 -1 coding))

          ;; Apply relevant dir-locals and file-locals.
          (let* ((dir-or-cache (dir-locals-find-file file))
                 (dir-class-vars (dir-locals-get-class-variables
                                  (if (listp dir-or-cache)
                                      (nth 1 dir-or-cache)
                                    (dir-locals-read-from-dir
                                     (file-name-directory file)))))
                 (locals (append (hack-local-variables--find-variables)
                                 (hack-local-variables-prop-line)
                                 (cdr (assq 'org-mode dir-class-vars))
                                 (cdr (assq 'text-mode dir-class-vars))
                                 (cdr (assq nil dir-class-vars)))))
            (let ((x (assq 'org-use-tag-inheritance locals)))
              (setq USE-TAG-INHERITANCE (if x (cdr x)
                                          $use-tag-inheritance)))
            (let ((x (assq 'org-tags-exclude-from-inheritance locals)))
              (setq NONHERITABLE-TAGS (if x (cdr x)
                                        $nonheritable-tags))))

          ;; If the very first line of file is a heading, don't try to scan
          ;; content before it.  Our usage of `org-mem-parser--next-heading'
          ;; cannot handle that edge-case.
          (unless (looking-at-p "\\*")
            ;; Narrow until first heading
            (when (org-mem-parser--next-heading)
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
                  (if (re-search-forward "^[\s\t]*:PROPERTIES:" FAR t)
                      (progn
                        (forward-line 1)
                        (org-mem-parser--collect-properties
                         (point)
                         (if (re-search-forward "^[\s\t]*:END:" FAR t)
                             (pos-bol)
                           (error "Couldn't find :END: of drawer"))))
                    nil))
            (setq HERE (point))
            (setq TAGS
                  (if (re-search-forward "^#\\+FILETAGS: " FAR t)
                      (split-string
                       (buffer-substring (point) (pos-eol))
                       ":" t)
                    nil))
            (goto-char HERE)
            (let (collected-todo-lines)
              (while (re-search-forward file-todo-option-re FAR t)
                (push (buffer-substring (point) (pos-eol)) collected-todo-lines))
              (setq TODO-RE (if collected-todo-lines
                                (org-mem-parser--make-todo-regexp
                                 (string-join collected-todo-lines " "))
                              $default-todo-re)))
            (goto-char HERE)
            (setq TITLE (when (re-search-forward "^#\\+TITLE: +" FAR t)
                          (string-trim-right
                           (org-mem-parser--org-link-display-format
                            (buffer-substring (point) (pos-eol))))))
            (setq ID (cdr (assoc "ID" PROPS)))
            (goto-char HERE)
            ;; Don't count org-super-links backlinks as forward links
            ;; TODO: Rewrite more readably
            (if (re-search-forward "^[\s\t]*:BACKLINKS:" nil t)
                (progn
                  (setq END (point))
                  (unless (search-forward ":END:" nil t)
                    (error "Couldn't find :END: of drawer"))
                  ;; Collect from end of backlinks drawer to first heading
                  (org-mem-parser--collect-links-until nil ID file INTERNAL-ENTRY-ID))
              (setq END (point-max)))
            (goto-char HERE)
            (org-mem-parser--collect-links-until END ID file INTERNAL-ENTRY-ID)
            (goto-char (point-max))
            ;; We should now be at the first heading
            (widen))
          (push (record 'org-mem-entry
                        file
                        1
                        1
                        TITLE
                        0
                        ID
                        nil
                        nil
                        nil
                        nil
                        PROPS
                        nil
                        nil
                        TAGS
                        nil
                        INTERNAL-ENTRY-ID
                        (and $do-cache-text (buffer-string))
                        org-mem-parser--found-timestamps)
                found-entries)
          (setq org-mem-parser--found-timestamps nil)

          (let ((heritable-tags
                 (and USE-TAG-INHERITANCE
                      (seq-difference TAGS NONHERITABLE-TAGS))))
            (push (list 0 1 1 TITLE ID heritable-tags) CRUMBS))
          (setq LNUM (line-number-at-pos))
          ;; Loop over the file's headings
          (while (not (eobp))
            ;; Narrow til next heading
            (narrow-to-region (point)
                              (save-excursion
                                (or (org-mem-parser--next-heading)
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
                               (org-mem-parser--org-link-display-format
                                (buffer-substring HERE (point))))))
              (setq TAGS nil)
              (setq TITLE (string-trim-right
                           (org-mem-parser--org-link-display-format
                            (buffer-substring HERE (pos-eol))))))
            ;; REVIEW: This is possibly overkill, and could be
            ;;         written in a way easier to follow.
            ;; Gotta go forward 1 line, see if it is a planning-line, and
            ;; if it is, then go forward 1 more line, and if that is a
            ;; :PROPERTIES: line, then we're safe to collect properties
            (forward-line 1)
            (setq HERE (point))
            (setq FAR (pos-eol))
            (setq SCHED
                  (if (re-search-forward "[\s\t]*SCHEDULED: +" FAR t)
                      (prog1 (org-mem-parser--stamp-to-iso8601
                              (buffer-substring
                               (point)
                               (+ (point) (skip-chars-forward "^]>\n"))))
                        (goto-char HERE))
                    nil))
            (setq DEADLINE
                  (if (re-search-forward "[\s\t]*DEADLINE: +" FAR t)
                      (prog1 (org-mem-parser--stamp-to-iso8601
                              (buffer-substring
                               (point)
                               (+ (point) (skip-chars-forward "^]>\n"))))
                        (goto-char HERE))
                    nil))
            (setq CLOSED
                  (if (re-search-forward "[\s\t]*CLOSED: +" FAR t)
                      (prog1 (org-mem-parser--stamp-to-iso8601
                              (buffer-substring
                               (point)
                               (+ (point) (skip-chars-forward "^]>\n"))))
                        (goto-char HERE))
                    nil))
            (when (or SCHED DEADLINE CLOSED)
              ;; Alright, so there was a planning-line, meaning any
              ;; :PROPERTIES: are not on this line but the next.
              (forward-line 1)
              (setq FAR (pos-eol)))
            (skip-chars-forward "\s\t")
            (setq PROPS
                  (if (looking-at-p ":PROPERTIES:")
                      (progn
                        (forward-line 1)
                        (org-mem-parser--collect-properties
                         (point)
                         (if (re-search-forward "^[\s\t]*:END:" nil t)
                             (pos-bol)
                           (error "Couldn't find :END: of drawer"))))
                    nil))
            (setq ID (cdr (assoc "ID" PROPS)))
            (setq INTERNAL-ENTRY-ID (org-mem-parser--mk-id file HEADING-POS))
            ;; TODO: Document this elsewhere
            ;; CRUMBS is a kind of state machine; a list that can look like
            ;;    ((3 23 500 "Heading" "id1234" ("noexport" "work" "urgent"))
            ;;     (2 10 122 "Another heading" "id6532" ("work"))
            ;;     (... ... ... ...))
            ;; if the previous heading (on line 23, char 500) looked like
            ;;    *** Heading  :noexport:work:urgent:
            ;;       :PROPERTIES:
            ;;       :ID: id1234
            ;;       :END:
            ;; It lets us track context so we know the outline path to the
            ;; current entry and what tags it should be able to inherit.

            ;; There are two ways we could store inherited tags for end use.
            ;; Either put them in a "tags-inherited" field, or put the
            ;; heritable local tags inside CRUMBS that a function
            ;; "tags-inherited" can later use to figure it out.

            ;; Going with the former to simplify implementation of
            ;; `org-mem-updater-mk-entry-atpt'.

            ;; That constraint is also why we cannot just let CRUMBS be a flat
            ;; list of positions and figure out everything else in real time,
            ;; because positions change.

            ;; Suppose someone filters entries by an inherited tag ":notes:" for
            ;; display as completion candidates, but we can't find the ancestors
            ;; because the positions are wrong in an unsaved buffer, and then a
            ;; newly inserted heading does not show up among candidates --
            ;; you get the idea.  Better to rely as little as possible on
            ;; cross-referencing with other entries' positions.

            (let ((heritable-tags
                   (and USE-TAG-INHERITANCE
                        (cl-loop for tag in TAGS
                                 unless (member tag NONHERITABLE-TAGS)
                                 collect tag))))
              (cl-loop until (> LEVEL (caar CRUMBS)) do (pop CRUMBS))
              (push (list LEVEL LNUM HEADING-POS TITLE ID heritable-tags)
                    CRUMBS))

            ;; Heading and properties analyzed, now seek links in entry text.

            (setq ID-HERE
                  (cl-loop for crumb in CRUMBS thereis (cl-fifth crumb)))
            (setq HERE (point))
            ;; Ignore backlinks drawer, it would lead to double-counting.
            ;; TODO: Generalize this mechanism to use configurable lists
            ;;       `$structures-to-ignore' and `$drawers-to-ignore'.
            (setq DRAWER-BEG (re-search-forward "^[\s\t]*:BACKLINKS:" nil t))
            (setq DRAWER-END
                  (and DRAWER-BEG
                       (or (search-forward ":END:" nil t)
                           (error "Couldn't find :END: of drawer"))))

            ;; Collect links inside the heading
            (goto-char HEADING-POS)
            (org-mem-parser--collect-links-until (pos-eol) ID-HERE file INTERNAL-ENTRY-ID)
            ;; Collect links between property drawer and backlinks drawer
            (goto-char HERE)
            (when DRAWER-BEG
              (org-mem-parser--collect-links-until DRAWER-BEG ID-HERE file INTERNAL-ENTRY-ID))
            ;; Collect links until next heading
            (goto-char (or DRAWER-END HERE))
            (org-mem-parser--collect-links-until (point-max) ID-HERE file INTERNAL-ENTRY-ID)

            (push (record 'org-mem-entry
                          file
                          LNUM
                          HEADING-POS
                          TITLE
                          LEVEL
                          ID
                          CLOSED
                          (mapcar #'butlast CRUMBS)
                          DEADLINE
                          PRIORITY
                          PROPS
                          SCHED
                          ;; Inherited tags
                          (nreverse
                           (delete-dups
                            (flatten-tree
                             (mapcar #'last (cdr CRUMBS)))))
                          TAGS
                          TODO-STATE
                          INTERNAL-ENTRY-ID
                          (and $do-cache-text (buffer-string))
                          org-mem-parser--found-timestamps)
                  found-entries)
            (setq org-mem-parser--found-timestamps nil)
            (goto-char (point-max))
            ;; NOTE: Famously slow `line-number-at-pos' is fast in narrow.
            (setq LNUM (+ LNUM -1 (line-number-at-pos)))
            (widen))

          ;; Done analyzing this file.
          (cl-assert (eobp))
          (setq file-data (list file attrs LNUM (point) coding)))

      ;; Don't crash on error signal, just report and move on to next file.
      (( error )
       (setq problem (list (format-time-string "%H:%M") file (point) err)))
      ;; Catch fake `skip-file' signal and report nothing.
      (t))

    (list (if bad-path (list bad-path))
          (if file-data (list file-data))
          found-entries
          org-mem-parser--found-links
          (if problem (list problem)))))

(provide 'org-mem-parser)

;;; org-mem-parser.el ends here
