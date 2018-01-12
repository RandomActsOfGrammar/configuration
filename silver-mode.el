;; Mode for editing Silver grammars

(provide 'silver)


;; automatically enter this mode for *.sv
(setq auto-mode-alist (cons '("\\.sv\\'" . silver-mode) auto-mode-alist))


;; if there are ever any keybindings for this mode, they will go in here
(defvar silver-mode-map nil
  "Local keymap for Silver grammar-mode buffers.")


;; hook for modifying the mode without modifying the mode
(defvar silver-mode-hook '()
  "*Hook for customizing Silver mode")


;; function for setting up mode
(defun silver-mode ()
  "Mode for editing Silver grammars."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode `silver-mode)
  (setq mode-name "Silver Grammar")
  (use-local-map silver-mode-map)
  (set-syntax-table silver-syntax-table)
  (set (make-local-variable 'font-lock-multiline) t) ;let us highlight multiline
  (set (make-local-variable 'font-lock-defaults)
       '(silver-font-lock-keywords))
  (add-hook 'font-lock-extend-region-functions 'silver-font-lock-extend-region)
  (turn-on-font-lock)
  (set (make-local-variable 'indent-line-function) 'silver-indent-line)
  (run-hooks 'silver-mode-hook))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Syntax Highlighting                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; syntax table--simply makes " not be a string character to not mess with
;;    comment highlighting
(defvar silver-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?\" "w" table) ;make strings not mess with comments
    table))


;; extends region for font-lock checking to allow multiline highlighting--
;;    stolen from StackOverflow
(defun silver-font-lock-extend-region ()
  "Extend the search region to include an entire block of text."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (or (re-search-backward "\n\n" nil t) (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "\n\n" nil t)
        (beginning-of-line)
        (setq font-lock-end (point)))
      (setq font-lock-beg found))))


;; stolen and unmodified from Teyjus Emacs mode file
;; appears to build a regular expression from a list of words
(defun make-regex (&rest args)
  (regexp-opt args 'words))


;; words for syntax highlighting
(defvar silver-font-lock-keywords
  (list
   ;; Comments
   (cons #'silver-comment-match font-lock-comment-face)

   ;; Variables
   ;; come before two colons; start with lowercase letters
   (cons #'silver-vars-match font-lock-variable-name-face)

   ;; Strings
   ;; "string", 'string', /regex/
   (cons
    ;;regex is a copy for each delimiter
    ;;opener ( ((not (opener or newline)) or (backslash opener))
    ;;          (not (newline or backslash)) ) optional opener
    ;;matches single-line strings with backslash-escaped quotes/slashes
    (concat "\\(" ;;start double quote group
            "\"\\(\\([^\"\n]\\|\\(\\\\\"\\)\\)*[^\n\\\\]\\)?\""
            "\\)" "\\|" ;;end double quote group; or separator
            "\\(" ;;start single quote group
            "'\\(\\([^'\n]\\|\\(\\\\'\\)\\)*[^\n\\\\]\\)?'"
            "\\)" "\\|" ;;end single quote group; or separator
            "\\(" ;;start forward slash group
            "/\\(\\([^/\n]\\|\\(\\\\/\\)\\)*[^\n\\\\]\\)?/"
            "\\)") ;;end forward slash group
    font-lock-string-face)

   ;; Headers
   (cons "grammar" font-lock-function-name-face)
   ;; TODO add highlighting in function name face to names of productions
   ;;       and functions

   ;; Builtin functions
   ;; left/right for associativity of terminals--they feel less like keywords
   ;;    to me than the other things
   (cons (make-regex "print" "left" "right" "toInt" "just" "nothing" "length"
                     "head" "tail" "toString")
         font-lock-builtin-face)

   ;; Keywords
   ;; TODO make it only match when there is not an underscore before or after
   (cons (make-regex "production" "productions" "occurs" "on" "nonterminal"
                     "atttribute" "propagate" "if" "then" "else" "option"
                     "exports" "close" "closed" "local" "global" "terminal"
                     "concrete" "abstract" "default" "ignore" "start" "lexer"
                     "class" "classes" "dominates" "submits" "aspect" "decorate"
                     "autocopy" "import" "imports" "association"
                     "precedence" "synthesized" "inherited" "functor" "with"
                     "as" "include" "only" "hiding" "using" "forwards" "to"
                     "use" "syntax" "forwarding" "function" "return"
                     ;; the ones I put here that they put elsewhere
                     "let" "attribute" "case" "end" "parser" "of" "true" "false"
                     )
         font-lock-keyword-face)

   ;; Types
   ;; with the addition of highlighting multiple types after "occurs on",
   ;;    when this was up farther it caused keywords to not be highlighted,
   ;;    but that was fixed by moving it down, so it probably isn't the most
   ;;    robust
   (cons #'silver-type-match font-lock-type-face)))


;; search through for types
(defconst silver-type-name-regex
  "\\[?[A-Z][a-zA-Z_0-9]*\\(<[a-zA-Z_0-9, ]+>\\)?\\]?"
  "Matches types for Silver; however, being a type is context-dependent.")
(defconst silver-tyvar-regex
  "\\[?[a-zA-Z][a-zA-Z_0-9]*\\(<.+>\\)?\\]?"
  "Matches types with type variables; however, they are context-dependent.")
(defun silver-type-match (limit)
  (let ( (full-regex (concat
                      ; regex for lowercase type variables
                      "\\(" silver-varname-regex "<[a-zA-Z_0-9, ]+> *:: *"
                         "\\(" silver-tyvar-regex "\\)" "\\)\\|"
                      ; regex for types for variables
                      "\\(:: *\\(" silver-type-name-regex "\\)\\)\\|"
                      ; regex for function return types
                      "\\(\\(" silver-type-name-regex "\\) *::=\\)\\|"
                      ; regex for "nonterminal Type"
                      "\\(nonterminal \\(" silver-type-name-regex "\\)\\)\\|"
                      ; regex for "occurs on Type"
                      "\\(occurs on \\(" silver-type-name-regex "\\)\\)")) )
    (if (re-search-forward full-regex limit t)
        (if (match-beginning 3) ;;check if there was a match for type variables
            (progn (goto-char (match-beginning 3))
                   (re-search-forward silver-tyvar-regex
                                      limit t))
        (progn (goto-char (match-beginning 0))
               (re-search-forward silver-type-name-regex)))
      (silver-find-extra-types-after-occurs-on limit))))
;; highlight multiple types after "occurs on"
;; e.g., synthesized atttribute ast occurs on Root, Term;
(defun silver-find-extra-types-after-occurs-on (limit)
  (let ( (start (point)) )
    (if (re-search-forward ";" limit)
        (let ( (semicolon (point)) )
          (progn (beginning-of-line)
                 (if (looking-at ".*occurs on")
                     (progn (re-search-forward "occurs on" semicolon)
                            (if (< (point) start)
                                (goto-char start)
                              nil)
                            (if (re-search-forward silver-type-name-regex
                                                   semicolon)
                                t
                              nil))
                   (progn (goto-char semicolon)
                          (silver-find-extra-types-after-occurs-on limit)))))
      nil)))


;; search through for variables
(defvar silver-varname-regex "[a-z][a-zA-Z_0-9]*\\(<[a-zA-Z_0-9, ]+>\\)?"
  "Matches variable names for Silver (context-dependent).")
(defun silver-vars-match (limit)
  (let ( (full-regular-regex
          (concat silver-varname-regex " *:: *"
                  "\\(\\(" silver-type-name-regex "\\)\\|"
                  "\\(" silver-tyvar-regex "\\)\\)")) )
    (if (re-search-forward full-regular-regex limit t)
        (progn (goto-char (match-beginning 0))
               (re-search-forward silver-varname-regex))
      nil)))


;;find comments, both single-line and multiline
;;look for the start of either kind
;;if the single line was found, true
;;if the multiline start was found, look for the end
;;   if the end was not found, go back to where you started and look for a
;;        single-line comment
;;   if the end was found, set the match data for it
;;if neither were found, nil
(defun silver-comment-match (limit)
  (let ( (start-regex (concat "\\(" "{-" "\\)\\|\\(" "--.*$" "\\)"))
         (pos (point)) )
    (if (re-search-forward start-regex limit t)
        (if (match-beginning 2)
            t
          (let ( (beg (match-beginning 0)) )
            (if (re-search-forward "-}" limit t)
                (progn (set-match-data (list beg (point)))
                       t)
              (progn (goto-char pos)
                     (re-search-forward "--.*$" limit t)))))
      nil)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                               Indentation                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;following example in https://www.emacswiki.org/emacs/ModeTutorial#toc3

;;Indentation Rules
;;1. beginning of buffer->no indent (col 0)
;;2. line has } at beginning->deindent to (previous line's level - 2)
;;      --breaks if we have {.*} all on one line
;;3. line first has } in a line before it->same as that line
;;4. line first has { in a line before it->indent to (that level + 2)
;;5. none of the above->col 0
(defun silver-indent-line ()
  "Indent current line as a Silver grammar."
  (beginning-of-line)
  (if (bobp) ;check for rule 1
      (indent-line-to 0)
    (let ( (not-indented t) cur-indent )
      (if (looking-at "^ *-?}") ;check for rule 2
          ;;(indent-line-to 0)
          (progn (save-excursion
                   (progn (forward-line -1)
                          (setq cur-indent (- (current-indentation) 2))))
                 (if (< cur-indent 0)
                     (indent-line-to 0)
                   (indent-line-to cur-indent)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at ".*} *$") ;check for rule 3
                (progn (setq cur-indent (current-indentation))
                       (setq not-indented nil))
              (if (looking-at "^ *{") ;check for rule 4
                  (progn (setq cur-indent (+ (current-indentation) 2))
                         (setq not-indented nil))
                (if (bobp) ;check for rule 5
                    (setq not-indented nil))))))
        (if cur-indent
            (if (< cur-indent 0)
                (indent-line-to 0)
              (indent-line-to cur-indent))
          (indent-line-to 0))))))
;; TODO might be nice to indent continuing from previous line (no semicolon
;;           ending the line for lines that require them, indent multiline
;;           comment on following line to match where it started before, case
;;           ends and bars line up with starting case, if-then-else line up,
;;           indent continuing inside parentheses to opening parenthesis)
