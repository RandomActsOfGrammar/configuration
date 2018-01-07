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
  (run-hooks 'silver-mode-hook))


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
   ;; Variables
   ;; come before two colons; start with lowercase letters
   (cons #'silver-vars-match font-lock-variable-name-face)

   ;; Comments
   (cons #'silver-comment-match font-lock-comment-face)

   ;; Strings
   ;; "string", 'string', /regex/
   ;; TODO not recognize strings that start inside comments--may need to be done
   ;;        on comment portion
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

   ;; Types
   ;; come after two colons and start with a capital letter--also other places?
   (cons #'silver-type-match font-lock-type-face)

   ;; Headers
   (cons (make-regex "grammar") font-lock-function-name-face)

   ;; Builtin functions
   ;; left/right for associativity of terminals--they feel less like keywords
   ;;    to me than the other things
   (cons (make-regex "print" "left" "right" "toInt" "just" "nothing" "length"
                     "head" "tail" "toString")
         font-lock-builtin-face)

   ;; Keywords
   ;; TODO change so "forwards to", "occurs on", "submits to" are
   ;;    in here as those specific phrases--"of" is also part of a two-word
   ;;    phrase
   (cons (make-regex  "synthesized" "attribute" "nonterminal" "inherited"
                      "production" "with" "case" "end" "if" "then" "else"
                      "function" "return" "decorate" "local" "closed"
                      "concrete" "terminal" "ignore" "abstract" "lexer"
                      "classes" "submits" "to" "parser" "aspect" "association"
                      "precedence" "dominates" "import" "of" "true" "false"
                      "forwards" "imports" "let" "productions" "occurs" "on")
         font-lock-keyword-face)))


;; search through for types
(defconst silver-type-name-regex
  "\\[?[A-Z][a-zA-Z_0-9]*\\(<[a-zA-Z_0-9, ]+>\\)?\\]?"
  "Matches types for Silver; however, being a type is context-dependent.")
(defconst silver-tyvar-regex
  "\\[?[a-zA-Z_0-9]+\\(<.+>\\)?\\]?"
  "Matches types with type variables; however, they are context-dependent.")
(defun silver-type-match (limit)
  (let ( (full-regex (concat
                      ; regex for lowercase type variables
                      "\\(" "[a-z][a-zA-Z_0-9]*<.*> *:: *"
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
        (if (match-beginning 2) ;;check if there was a match for type variables
            (progn (goto-char (match-beginning 2))
                   (re-search-forward silver-tyvar-regex
                                      limit t))
        (progn (goto-char (match-beginning 0))
               (re-search-forward silver-type-name-regex)))
      nil)))


;; search through for variables
(defvar silver-varname-regex "[a-z][a-zA-Z_0-9]*\\(<.*>\\)?"
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


;; stolen from
;;    https://stackoverflow.com/questions/9452615/
;;       emacs-is-there-a-clear-example-of-multi-line-font-locking
;; and then modified
(defun silver-comment-match-block (last)
  (cond ((search-forward "{-" last t)
         (let ((beg (match-beginning 0)))
           (cond ((search-forward-regexp "-}" last t)
                  (set-match-data (list beg (point)))
                  t)
                 (t nil))))
        (t nil)))
;; This currently grabs lines with -- first, which is a problem for comments
;;    beginning with {--      TODO
(defun silver-comment-match (limit)
  (if (re-search-forward "--.*$" limit t)
      t
    (silver-comment-match-block limit)))


;;;;;; TODO
;;;;;;;; Indentation (two spaces within braces) (currently getting ~5)
