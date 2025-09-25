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

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; Tell compiler these aren't free variables
(defvar $plain-re)
(defvar $bracket-re)
(defvar $merged-re)
(defvar $default-todo-re)
(defvar $nonheritable-tags)
(defvar $inlinetask-min-level)
(defvar $use-tag-inheritance)
(defvar $structures-to-ignore) ; TODO: implement
(defvar $drawers-to-ignore) ; TODO: implement

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

(defconst org-mem-parser--max-safe-hex-digits
  (- (length (format "%x" most-positive-fixnum)) 1))

(defun org-mem-parser--mk-id (file-name pos)
  "Reduce FILE-NAME and POS into an `eq'-safe probably-unique fixnum."
  (+ (string-to-number (substring (secure-hash 'md5 file-name)
                                  (- org-mem-parser--max-safe-hex-digits))
                       16)
     pos))

(defun org-mem-parser--org-link-display-format (s)
  "Copy of `org-link-display-format'.
Format string S for display - this means replace every link inside S
with only their description if they have one, and in any case strip the
brackets."
  (replace-regexp-in-string
   $bracket-re
   (lambda (m) (or (match-string 2 m) (match-string 1 m)))
   s nil t))

(defconst org-mem-parser--org-ts-regexp0
  "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\( +[^]+0-9>\r\n -]+\\)?\\( +\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Copy of `org-ts-regexp0'.")

(defun org-mem-parser--time-string-to-int (s)
  "Parse the first Org timestamp in string S and return as integer."
  (if (not (string-match org-mem-parser--org-ts-regexp0 s))
      (if (string-search "%%(" s)
          ;; HACK: This looks like a "diary sexp" such as:
          ;;     <%%(memq (calendar-day-of-week date) '(1 2 3 4 5)))>
          ;; Return nil in this case, which should be safe because (as of
          ;; 2025-06-06) the only way this function would be called with such
          ;; a string S, is on a planning-line (CLOSED, SCHEDULED or DEADLINE),
          ;; where nil is valid.
          ;; FIXME: This is "safe" in the sense that the program won't break,
          ;; but it means `org-mem-entry-scheduled' & co are not faithful.
          ;; https://github.com/meedstrom/org-mem/issues/21
          nil
        (error "Code 11: Not an Org time string: %s" s))
    ;; Copypasta `org-parse-time-string', faster than `parse-time-string'.
    (let ((ts (list 0
	            (cond ((match-beginning 8)
                           (string-to-number (match-string 8 s)))
	                  (t 0))
	            (cond ((match-beginning 7)
                           (string-to-number (match-string 7 s)))
	                  (t 0))
	            (string-to-number (match-string 4 s))
	            (string-to-number (match-string 3 s))
	            (string-to-number (match-string 2 s))
	            nil -1 nil)))
      (and ts (time-convert (encode-time ts) 'integer)))))

(defvar org-mem-parser--found-links nil
  "Link objects found so far.")

(defvar org-mem-parser--found-active-stamps nil
  "Active timestamps found so far.")

(defconst org-mem-parser--org-ts-regexp
  "<\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?\\)>")

(defun org-mem-parser--scan-text-until (end id-here file internal-entry-id)
  "From here to buffer position END, collect links and active timestamps.

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
                     (length> LINK-PATH (1+ colon-pos))
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
                                         (error "Code 17: No closing bracket to \"[cite:\""))))
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
                          "cite"
                          ;; Replace & with @ like `org-mem--split-roam-refs-field'
                          (concat "@" (substring (match-string 0) 1))
                          LINK-DESC
                          t
                          id-here
                          internal-entry-id)
                  org-mem-parser--found-links)))))

    ;; Start over and look for active timestamps
    (goto-char beg)
    (while (re-search-forward org-mem-parser--org-ts-regexp end t)
      (push (org-mem-parser--time-string-to-int (match-string 0))
            org-mem-parser--found-active-stamps)))
  (goto-char (or end (point-max))))

(defun org-mem-parser--collect-properties (beg end)
  "Collect Org properties between BEG and END into an alist.
Assumes BEG and END are buffer positions delimiting a region in
between buffer substrings \":PROPERTIES:\" and \":END:\"."
  (let (alist START VALUE)
    (goto-char beg)
    (while (< (point) end)
      (skip-chars-forward "\s\t")
      (unless (looking-at-p ":")
        (error "Code 5: Possibly malformed property drawer"))
      (forward-char)
      (when (eolp)
        (error "Code 6: Possibly malformed property drawer"))
      (setq START (point))
      (unless (search-forward ":" (pos-eol) t)
        (error "Code 7: Possibly malformed property drawer"))
      ;; In the wild you see some :MULTIPLE:COLONS:IN:NAME: properties, which
      ;; would make this condition nil.  But they are illegal according to
      ;; `org-property-drawer-re', so don't bother to collect it.
      (when (looking-at-p " ")
        (setq VALUE (string-trim (buffer-substring (point) (pos-eol))))
        (push (cons (upcase (buffer-substring START (1- (point))))
                    ;; Emulate `org-entry-get' special case for string "nil"
                    (if (equal VALUE "nil") nil VALUE))
              alist))
      (forward-line 1))
    alist))


;;; Main

(defconst org-mem-parser--org-drawer-regexp "^[ \t]*:[_[:word:]-]+:[ \t]*$")
(defvar org-mem-parser--outline-regexp nil)
(defvar org-mem-parser--buf nil)
(defun org-mem-parser--parse-file (file)
  "Gather entries, links and other data in FILE."
  (when (and (fboundp 'org-mem--mk-work-vars)
             (fboundp 'el-job--ensure-compiled-lib)
             (boundp 'org-mem-load-features)
             (boundp 'org-mem-inject-vars))
    ;; For debugging in main process; el-job already sets these in subprocesses
    ;; (the above condition fails if subprocess is calling this).
    (dolist (var (org-mem--mk-work-vars))
      (set (car var) (cdr var)))
    (dolist (var org-mem-inject-vars)
      (when (consp var)
        (set (car var) (cdr var))))
    (dolist (lib org-mem-load-features)
      (load (el-job--ensure-compiled-lib lib))))
  (unless (eq org-mem-parser--buf (current-buffer))
    (switch-to-buffer
     (setq org-mem-parser--buf (get-buffer-create " *org-mem-parser*" t))))
  (unless org-mem-parser--outline-regexp
    (setq org-mem-parser--outline-regexp
          (if $inlinetask-min-level
              (rx-to-string
               `(seq bol (repeat 1 ,(1- $inlinetask-min-level) "*") " "))
            (rx bol (repeat 1 14 "*") " "))))
  (setq org-mem-parser--found-links nil)
  (setq org-mem-parser--found-active-stamps nil)
  (let ((case-fold-search t)
        (buffer-read-only t)
        (file-todo-option-re
         (rx bol (* space) (or "#+todo: " "#+seq_todo: " "#+typ_todo: ")))
        bad-path
        found-entries
        file-data
        problem
        coding-system
        ;; Upcased names change value a lot, take care to keep correct.
        ID ID-HERE INTERNAL-ENTRY-ID
        TAGS USE-TAG-INHERITANCE NONHERITABLE-TAGS
        TITLE HEADING-POS LNUM CRUMBS CLOCK-LINES
        TODO-STATE STATS-COOKIES INITIAL-STATS-COOKIES
        SCHED DEADLINE CLOSED PRIORITY LEVEL PROPS
        LEFT RIGHT DRAWER-BEG DRAWER-END
        (TODO-RE $default-todo-re))
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
            (error "Code 8: File not readable"))
          ;; NOTE: Don't use `insert-file-contents-literally'!  It sets
          ;; `coding-system-for-read' to `no-conversion', which results in
          ;; wrong values for HEADING-POS when the file contains any multibyte.
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert-file-contents file)
            (setq coding-system last-coding-system-used))

          ;; Apply relevant dir-locals and file-locals.
          ;; NOTE: Some variables you'd think would work in .dir-locals.el,
          ;;       such as `org-todo-keywords', don't work, so don't bother
          ;;       emulating support here.
          ;;       (For that sort of purpose, Org provides the #+SETUPFILE option,
          ;;       https://lists.gnu.org/r/emacs-orgmode/2020-05/msg00510.html)
          ;;       TODO: Read any #+SETUPFILE around the same time we read
          ;;             #+TITLE and #+FILETAGS.  While we're at it, maybe
          ;;             cache all #+keywords just because.
          (let* ((dir-or-cache (dir-locals-find-file file))
                 (dir-class-vars (dir-locals-get-class-variables
                                  (if (listp dir-or-cache)
                                      (nth 1 dir-or-cache)
                                    (dir-locals-read-from-dir
                                     (file-name-directory file)))))
                 (all-locals (append (hack-local-variables--find-variables)
                                     (hack-local-variables-prop-line)
                                     (cdr (assq 'org-mode dir-class-vars))
                                     (cdr (assq 'text-mode dir-class-vars))
                                     (cdr (assq nil dir-class-vars)))))
            (let ((x (assq 'org-use-tag-inheritance all-locals)))
              (setq USE-TAG-INHERITANCE (if x (cdr x)
                                          $use-tag-inheritance)))
            (let ((x (assq 'org-tags-exclude-from-inheritance all-locals)))
              (setq NONHERITABLE-TAGS (if x (cdr x)
                                        $nonheritable-tags))))

          ;;; Scan content before first heading, if any

          (setq INTERNAL-ENTRY-ID (org-mem-parser--mk-id file 0))
          (while (looking-at-p (rx (*? space) (or "# " "\n")))
            (forward-line))
          (unless (looking-at-p "\\*")
            ;; Narrow until first heading, if there is one
            (save-excursion
              (when (re-search-forward org-mem-parser--outline-regexp nil t)
                (narrow-to-region 1 (pos-bol))))
            ;; A bug introduced in org-node 5035a33 (fixed ~5 days later)
            ;; could insert BACKLINKS before PROPERTIES.  Add a warning so
            ;; user can fix the affected notes.
            (when (looking-at-p "^[ \t]*:BACKLINKS:[ \t]*$")
              (error "Code 12: Found BACKLINKS drawer before PROPERTIES \(likely inserted by org-node 3.4.3, bug fixed in 3.4.4)"))
            ;; We can safely assume that if there's a properties drawer,
            ;; it's the first drawer AND it comes before any #+keyword, at
            ;; least going by the behavior of `org-id-get'.
            (when (looking-at "^[ \t]*:PROPERTIES:")
              (goto-char (match-end 0))
              (unless (looking-at-p "[ \t]*$")
                (error "Code 13: Likely malformed :PROPERTIES: line"))
              (forward-line)
              (setq PROPS (org-mem-parser--collect-properties
                           (point)
                           (if (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                               (pos-bol)
                             (error "Code 14: Could not find :END: of drawer"))))
              (setq ID (cdr (assoc "ID" PROPS)))
              (forward-line))
            ;; PERF: Find tight boundaries for later searches.
            (setq LEFT (point))
            (while (or (looking-at-p (rx (*? space) (or "# " "\n")))
                       (and (looking-at-p org-mem-parser--org-drawer-regexp)
                            (re-search-forward "^[ \t]*:END:[ \t]*$")))
              (forward-line))
            (while (looking-at-p (rx (*? space) (or "#+" "# " "\n")))
              (forward-line))
            (setq RIGHT (point)) ;; End of the "front matter".

            (goto-char LEFT)
            (when (re-search-forward "^#\\+FILETAGS:" RIGHT t)
              (when (not (eolp))
                (when (= 0 (skip-chars-forward " "))
                  (error "Code 15: A #+FILETAGS: keyword is missing space"))
                (setq TAGS (split-string (buffer-substring (point) (pos-eol))
                                         ":" t))))
            (goto-char LEFT)
            (let (collected-todo-lines)
              (while (re-search-forward file-todo-option-re RIGHT t)
                (push (buffer-substring (point) (pos-eol))
                      collected-todo-lines))
              (when collected-todo-lines
                (setq TODO-RE (org-mem-parser--make-todo-regexp
                               (string-join collected-todo-lines " ")))))
            (goto-char LEFT)
            (when (re-search-forward "^#\\+TITLE:" RIGHT t)
              (when (not (eolp))
                (when (= 0 (skip-chars-forward " "))
                  (error "Code 16: A #+TITLE: keyword is missing space"))
                (setq TITLE (string-trim-right
                             (org-mem-parser--org-link-display-format
                              (buffer-substring (point) (pos-eol)))))))
            (when (string-empty-p TITLE)
              (setq TITLE nil))
            (let ((heritable-tags
                   (and USE-TAG-INHERITANCE
                        (seq-difference TAGS NONHERITABLE-TAGS))))
              (push (list 0 1 1 TITLE ID heritable-tags PROPS) CRUMBS))

            ;; OK, got file title and properties.  Now look for things of
            ;; interest in body text.
            ;; Don't look inside a BACKLINKS drawer though, because links
            ;; inside should not count as "forward links".
            (goto-char LEFT)
            (if (re-search-forward "^[ \t]*:BACKLINKS:" nil t)
                (progn
                  (unless (looking-at-p "[ \t]*$")
                    (error "Code 1: Likely malformed drawer"))
                  (setq RIGHT (point))
                  (unless (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                    (error "Code 2: Could not find :END: of drawer"))
                  ;; Scan stuff after the backlinks drawer.
                  (org-mem-parser--scan-text-until nil ID file INTERNAL-ENTRY-ID))
              (setq RIGHT (point-max)))
            ;; Scan stuff before the backlinks drawer.
            (goto-char LEFT)
            (org-mem-parser--scan-text-until RIGHT ID file INTERNAL-ENTRY-ID)
            (goto-char (point-max))
            ;; We should now be at the first heading.
            (widen))
          (push (record 'org-mem-entry
                        file
                        1
                        1
                        TITLE
                        0
                        ID
                        org-mem-parser--found-active-stamps
                        nil
                        nil
                        CRUMBS
                        nil
                        nil
                        nil
                        PROPS
                        nil
                        nil
                        nil
                        TAGS
                        nil
                        INTERNAL-ENTRY-ID)
                found-entries)

          ;; Prep
          (unless CRUMBS
            (push (list 0 1 1 nil nil nil nil) CRUMBS))
          (setq org-mem-parser--found-active-stamps nil)
          (setq LNUM (line-number-at-pos))

          ;;; Loop over the file's headings

          (while (not (eobp))
            ;; Narrow til next heading
            (narrow-to-region
             (point)
             (save-excursion
               (forward-char) ;; Prevent matching same line forever
               (if (re-search-forward org-mem-parser--outline-regexp nil t)
                   (pos-bol)
                 (point-max))))
            (setq HEADING-POS (point))
            (setq LEVEL (skip-chars-forward "*"))
            (setq STATS-COOKIES nil)
            (setq INITIAL-STATS-COOKIES nil)
            (skip-chars-forward " ")
            ;; NOTE: Org seems to expect todo and priority in a strict order,
            ;;       and before anything else.  Good for us.
            (let ((case-fold-search nil))
              (setq TODO-STATE (and (looking-at TODO-RE)
                                    (prog1 (match-string 0)
                                      (goto-char (match-end 0))
                                      (skip-chars-forward " "))))
              (setq PRIORITY (and (looking-at "\\[#[A-Z0-9]+\\]")
                                  (prog1 (match-string 0)
                                    (goto-char (match-end 0))
                                    (skip-chars-forward "\s\t")))))
            ;; NOTE: From here on, Org seems to permit tabs.
            (while (looking-at "\\[[0-9/%]+]")
              (push (match-string 0) INITIAL-STATS-COOKIES)
              (goto-char (match-end 0))
              (skip-chars-forward "\s\t"))
            (setq LEFT (point))
            ;; Any tags in heading?
            (if (re-search-forward "[ \t]+:\\([^ ]+\\):[ \t]*$" (pos-eol) t)
                (progn
                  (goto-char (match-beginning 0))
                  (setq TAGS (split-string (match-string 1) ":" t)))
              (goto-char (pos-eol))
              (setq TAGS nil))
            (setq RIGHT (point))
            (skip-chars-backward "\s\t")
            (if (< (point) LEFT)
                (setq TITLE "")
              ;; Get the rest of the stats-cookies, and make sure we will leave
              ;; trailing stats-cookies out of the title.
              ;; For example, the hypothetical heading:
              ;; "** [2/10] Foo [5/10] Bar [1/10] [20%]"
              ;; should be represented as an entry titled "Foo [5/10] Bar".
              ;; https://github.com/meedstrom/org-mem/issues/22
              (setq RIGHT (point))
              (while (re-search-backward "\\[[0-9/%]+]" LEFT t)
                (push (match-string 0) STATS-COOKIES)
                (when (eq (match-end 0) RIGHT)
                  (skip-chars-backward "\s\t")
                  (if (< (point) LEFT)
                      (goto-char LEFT)
                    (setq RIGHT (point)))))
              (setq TITLE (string-trim-right
                           (org-mem-parser--org-link-display-format
                            (buffer-substring LEFT RIGHT)))))
            ;; Put the cookies in the same order as they occurred in the heading.
            (setq STATS-COOKIES (nconc (nreverse INITIAL-STATS-COOKIES) STATS-COOKIES))
            ;; Gotta go forward 1 line, see if it is a planning-line, and
            ;; if it is, then go forward 1 more line, and if that is a
            ;; :PROPERTIES: line, then we're safe to collect properties.
            (forward-line 1)
            (setq LEFT (point))
            (setq RIGHT (pos-eol))
            (setq SCHED
                  (and (re-search-forward "[ \t]*SCHEDULED: +" RIGHT t)
                       (org-mem-parser--time-string-to-int
                        (buffer-substring
                         (point)
                         (+ (point) (skip-chars-forward "^]>\n"))))))
            (goto-char LEFT)
            (setq DEADLINE
                  (and (re-search-forward "[ \t]*DEADLINE: +" RIGHT t)
                       (org-mem-parser--time-string-to-int
                        (buffer-substring
                         (point)
                         (+ (point) (skip-chars-forward "^]>\n"))))))
            (goto-char LEFT)
            (setq CLOSED
                  (and (re-search-forward "[ \t]*CLOSED: +" RIGHT t)
                       (org-mem-parser--time-string-to-int
                        (buffer-substring
                         (point)
                         (+ (point) (skip-chars-forward "^]>\n"))))))
            (when (or SCHED DEADLINE CLOSED)
              ;; Alright, so there was a planning-line, meaning any
              ;; :PROPERTIES: are not on this line but the next.
              (forward-line 1))
            (setq PROPS
                  (if (looking-at-p "[ \t]*:PROPERTIES:")
                      (progn
                        (forward-line 1)
                        (org-mem-parser--collect-properties
                         (point)
                         (if (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                             (pos-bol)
                           (error "Code 9: Couldn't find :END: of drawer"))))
                    nil))
            (setq ID (cdr (assoc "ID" PROPS)))
            (setq INTERNAL-ENTRY-ID (org-mem-parser--mk-id file HEADING-POS))
            (setq LEFT (point))
            ;; rough start of body text (just a perf hack, fails gracefully)
            (setq RIGHT (re-search-forward "^[ \t]*[a-bd-z]" nil t))
            (goto-char LEFT)
            (while (re-search-forward "^[ \t]*CLOCK: " RIGHT t)
              (let ((clock-start
                     (org-mem-parser--time-string-to-int
                      (buffer-substring (point)
                                        (or (search-forward "--" (pos-eol) :move)
                                            (point)))))
                    (clock-end
                     (unless (eolp)
                       (org-mem-parser--time-string-to-int
                        (buffer-substring (point)
                                          (or (search-forward "=>" (pos-eol) :move)
                                              (point))))))
                    (clock-seconds
                     (and (not (eolp))
                          (search-forward ":" (pos-eol) t)
                          (* 60
                             (+ (number-at-point)
                                (progn (backward-char)
                                       (* 60 (number-at-point))))))))
                (when (null clock-start)
                  (error "Code 10: Unusual clock line: \"%s\""
                         (buffer-substring (pos-bol) (pos-eol))))
                (push (if clock-end (list clock-start clock-end clock-seconds)
                        (list clock-start))
                      CLOCK-LINES)))
            (goto-char LEFT)

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

            ;; Discussion: There are two ways we could store inherited tags
            ;; for end use.  Either put them in a "tags-inherited" field, or
            ;; put all heritable local tags inside CRUMBS that a function
            ;; "tags-inherited" can later use to figure it out.

            ;; Going with the former to simplify implementation of
            ;; `org-mem-updater-mk-entry-atpt'.

            ;; That constraint is also why we cannot just let CRUMBS be a flat
            ;; list of positions and figure out everything else in real time,
            ;; because positions change.

            ;; Suppose someone filters entries by an inherited tag for display
            ;; as completion candidates to some command, but org-mem can't
            ;; find any ancestors to a newly inserted entry in an unsaved
            ;; buffer because the positions don't match the cached positions,
            ;; and so the new entry appears to inherit nothing.  Better to
            ;; rely as little as possible on cross-referencing with other
            ;; entries' positions.

            ;; Rant: I'm not happy with hacks like `org-mem-updater-mk-entry-atpt'
            ;; though, I hope to do away with the need for it one day.
            ;; Would require saving the buffer much more often while the user
            ;; works in it, which may not suit all users, but makes things
            ;; easier to reason about.

            ;; Or if ever we switch to using Org's own parser, then we'll be
            ;; able to just translate the buffer's parse tree that is always
            ;; correct even in an unsaved buffer.  In theory.
            ;; https://lists.gnu.org/archive/html/emacs-orgmode/2025-05/msg00288.html

            (let ((heritable-tags
                   (and USE-TAG-INHERITANCE
                        (cl-loop for tag in TAGS
                                 unless (member tag NONHERITABLE-TAGS)
                                 collect tag))))
              (cl-loop until (> LEVEL (caar CRUMBS)) do (pop CRUMBS))
              (push (list LEVEL LNUM HEADING-POS TITLE ID heritable-tags PROPS)
                    CRUMBS))

            ;; Heading and properties analyzed, now seek links in entry text.

            (setq ID-HERE
                  (cl-loop for crumb in CRUMBS thereis (cl-fifth crumb)))
            ;; Ignore backlinks drawer, it would lead to double-counting.
            ;; TODO: Generalize this mechanism to use configurable lists
            ;;       `$structures-to-ignore' and `$drawers-to-ignore'.
            (setq DRAWER-BEG (re-search-forward "^[ \t]*:BACKLINKS:" nil t))
            (setq DRAWER-END
                  (and DRAWER-BEG
                       (progn
                         (unless (looking-at-p "[ \t]*$")
                           (error "Code 3: Likely malformed drawer"))
                         (or (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                             (error "Code 4: Couldn't find :END: of drawer")))))

            ;; Scan stuff inside the heading
            (goto-char HEADING-POS)
            (org-mem-parser--scan-text-until (pos-eol) ID-HERE file INTERNAL-ENTRY-ID)
            ;; Scan stuff between property drawer and backlinks drawer
            (goto-char LEFT)
            (when DRAWER-BEG
              (org-mem-parser--scan-text-until DRAWER-BEG ID-HERE file INTERNAL-ENTRY-ID))
            ;; Scan stuff until next heading
            (goto-char (or DRAWER-END LEFT))
            (org-mem-parser--scan-text-until (point-max) ID-HERE file INTERNAL-ENTRY-ID)

            (push (record 'org-mem-entry
                          file
                          LNUM
                          HEADING-POS
                          TITLE
                          LEVEL
                          ID
                          org-mem-parser--found-active-stamps
                          CLOCK-LINES
                          CLOSED
                          ;; Same as CRUMBS but without the tags or props;
                          ;; matter of perf, as printing lists is slow.
                          (mapcar (lambda (x) (take 5 x)) CRUMBS)
                          DEADLINE
                          PRIORITY
                          ;; Inherited properties
                          (cl-loop for crumb in (cdr CRUMBS)
                                   append (cl-seventh crumb))
                          PROPS
                          SCHED
                          STATS-COOKIES
                          ;; Inherited tags
                          (nreverse
                           (delete-dups
                            (cl-loop for crumb in (cdr CRUMBS)
                                     append (cl-sixth crumb))))
                          TAGS
                          TODO-STATE
                          INTERNAL-ENTRY-ID)
                  found-entries)
            (setq org-mem-parser--found-active-stamps nil)
            (setq CLOCK-LINES nil)
            (goto-char (point-max))
            ;; NOTE: Famously slow `line-number-at-pos' fast in narrow region.
            (setq LNUM (+ LNUM -1 (line-number-at-pos)))
            (widen))

          ;; Done analyzing this file.
          (cl-assert (eobp))
          (setq file-data (list file
                                (file-attributes file)
                                LNUM
                                (point)
                                coding-system)))

      ;; Don't crash on error signal, just record the problem so it can
      ;; optionally be reported to user, and move on to next file.
      (( error )
       (setq problem (list (format-time-string "%H:%M") file (point) err))
       (widen)
       (setq file-data (list file
                             (file-attributes file)
                             (line-number-at-pos (point-max))
                             (point-max)
                             coding-system)))

      ;; Catch fake `skip-file' signal.
      (t
       (cl-assert (null file-data))
       (cl-assert (null found-entries))
       (cl-assert (null org-mem-parser--found-links))
       (cl-assert (null problem))))

    (list (if bad-path (list bad-path))
          (if file-data (list file-data))
          found-entries
          org-mem-parser--found-links
          (if problem (list problem)))))

(provide 'org-mem-parser)

;;; org-mem-parser.el ends here
