;; Mode for editing Silver grammars
(require 'easymenu)
(require 'ido)

(provide 'silver-mode)


;; automatically enter this mode for *.sv
(setq auto-mode-alist (cons '("\\.sv\\'" . silver-mode) auto-mode-alist))


;; if there are ever any keybindings for this mode, they will go in here
(defvar silver-mode-map nil
  "Local keymap for Silver grammar-mode buffers.")
(cond ((not silver-mode-map)
       (setq silver-mode-map (make-sparse-keymap))
       (define-key silver-mode-map "\C-c\C-p" 'silver-insert-production)
       (define-key silver-mode-map "\C-c\C-f" 'silver-insert-function)
       (define-key silver-mode-map "\C-c\C-c" 'silver-compile)
       (define-key silver-mode-map "\C-c\C-n" 'silver-next-error)
       (define-key silver-mode-map "\C-c\C-v" 'silver-prev-error)))


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
  ;;menu
  (easy-menu-define nil silver-mode-map "Silver menu" silver-menu)
  (easy-menu-add silver-menu)
  ;;keybindings
  (use-local-map silver-mode-map)
  ;;syntax highlighting
  (set-syntax-table silver-syntax-table)
  (set (make-local-variable 'font-lock-multiline) t) ;let us highlight multiline
  (set (make-local-variable 'font-lock-defaults)
       '(silver-font-lock-keywords))
  (add-hook 'font-lock-extend-region-functions 'silver-font-lock-extend-region)
  (turn-on-font-lock)
  ;;indentation
  (set (make-local-variable 'indent-line-function) 'silver-indent-line)
  ;;hook for user changes
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
                     "use" "syntax" "forwarding" "function" "return" "when"
                     ;; the ones I put here that they put elsewhere
                     "let" "attribute" "case" "end" "parser" "of" "true" "false"
                     ;; just for monad stuff
                     "implicit" "restricted" "unrestricted"
                     "Implicit" "Restricted" "Unrestricted"
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

;;done by following example in https://www.emacswiki.org/emacs/ModeTutorial#toc3

;;find and return the starting column of the currently-open case statement
(defun find-matching-case ()
  (let ( (case-found nil) (end-count 0) (case-indent 0) )
    (save-excursion
      (while (and (not case-found) (not (bobp)))
        (forward-line -1)
        (beginning-of-line)
        (if (looking-at "^ *end[;\n ]")
            (setq end-count (+end-count 1))
          (if (looking-at "^ *case *.* *of")
              (if (= end-count 0)
                  (progn (setq case-indent (current-column))
                         (setq case-found t))
                (setq end-count (- end-count 1)))))))
    case-indent))

;;Indentation Rules
;;1. beginning of buffer->no indent (col 0)
;;2. line has } at beginning->deindent to (previous line's level - 2)
;;      --breaks if we have {.*} all on one line
;;3. line starts with then->align with if from a previous line
;;4. line starts with else->align with then from a previous line
;;5. line is the first line after a case start->align two characters past case start
;;6. line starts with |->align with matching case
;;7. line is end->align with matching case

;;8. line first has } in a line before it->same as that line
;;9. line first has { in a line before it->indent to (that level + 2)
;;10. none of the above->col 0
(defun silver-indent-line ()
  "Indent current line as a Silver grammar."
  (beginning-of-line)
  (if (bobp) ;check for rule 1
      (indent-line-to 0)
    (let ( (not-indented t) cur-indent )
      (if (looking-at "^ *-?}") ;check for rule 2
          (progn (save-excursion
                   (progn (forward-line -1)
                          (setq cur-indent (- (current-indentation) 2))))
                 (if (< cur-indent 0)
                     (indent-line-to 0)
                   (indent-line-to cur-indent)))
        (if (looking-at "^ *then ") ;check for rule 3
            (progn (save-excursion
                     (search-backward " if ")
                     (setq not-indented nil)
                                        ;need to add one because goes to beginning of word minus a space
                     (setq cur-indent (+ (current-column) 1)))
                   (indent-line-to cur-indent))
          ;;this will break if we have nested ifs
          (if (looking-at "^ *else ") ;check for rule 4
              (progn (save-excursion
                       (search-backward " then ")
                       (setq not-indented nil)
                                        ;need to add one because goes to beginning of word minus a space
                       (setq cur-indent (+ (current-column) 1)))
                     (indent-line-to cur-indent))
              (if (looking-at "^.*->")
                  (progn
                    (message "looking at ->")
                    (setq cur-indent (find-matching-case))
                    (if (looking-at "^ *|")
                        (setq cur-indent cur-indent) ;cur-indent is already correct
                      (setq cur-indent (+ cur-indent 1)))
                    (setq not-indented nil)
                    (indent-line-to cur-indent))
                (if (looking-at "^ *end[; ]$")
                    (progn
                      (message "Looking at end")
                      (setq cur-indent (find-matching-case))
                      (setq not-indented nil)
                      (indent-line-to cur-indent))
                (save-excursion
                  (message "Skipped looking-at ->")
                  (while not-indented
                    (forward-line -1)
                    (if (looking-at ".*} *$") ;check for rule 8
                        (progn (setq cur-indent (current-indentation))
                               (setq not-indented nil))
                      (if (looking-at "^ *{") ;check for rule 9
                          (progn (setq cur-indent (+ (current-indentation) 2))
                                 (setq not-indented nil))
                        (if (bobp) ;check for rule 10
                            (setq not-indented nil))))))
                (if cur-indent
                    (if (< cur-indent 0)
                        (indent-line-to 0)
                      (indent-line-to cur-indent))
                  (indent-line-to 0))))))))))
;; TODO might be nice to indent continuing from previous line (no semicolon
;;           ending the line for lines that require them, indent multiline
;;           comment on following line to match where it started before, case
;;           ends and bars line up with starting case, if-then-else line up,
;;           indent continuing inside parentheses to opening parenthesis)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                   Menu                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generally things to make it a little easier to use
;; includes inserting skeletons for functions and productions and
;;    in-Emacs compilation

;;get list of parameters for production or function
(defun silver-get-param ()
  (interactive)
  (let ( (param (read-string "Parameter name, RET: ")) )
    (if (string= param "")
        ;;remove extra space at end of parameter line
        (delete-backward-char 1)
      (progn
        (let ( (param-type (read-string "Parameter type: ")) )
          (if (string= param-type "")
              (insert (concat param " "))
            (insert (concat param (concat "::" (concat param-type " "))))))
        (silver-get-param)))))

;;insert a production, with the name, referring name, parameters, and braces
(defun silver-insert-production ()
  (interactive)
  ;;type of production
  (let ( (start (concat
                 (ido-completing-read "Type of production: "
                                      '("concrete" "abstract" "aspect"))
                 " production ")) )
    (insert start))
  ;;name of production
  (let ( (name (concat (read-string "Name of production: ") "\n")) )
    (insert name))
  ;;name for production
  (let ( (ref-name (concat (read-string "Name used to refer to production: ")
                           "::")) )
    (insert ref-name))
  ;;type of production
  (let ( (prod-type (concat (read-string "Type of production: ") " ::= ")) )
    (insert prod-type))
  ;;parameters
  (silver-get-param)
  ;;add braces
  (insert "\n{\n\n}")
  (forward-line -1)
  (silver-indent-line))

;;insert a function, with the name, referring name, parameters, and braces
(defun silver-insert-function ()
  (interactive)
  ;;type of function
  (insert "function ")
  ;;name of function
  (let ( (name (concat (read-string "Name of function: ") "\n")) )
    (insert name))
  ;;return type of function
  (let ( (ret-type (read-string "Return type of function: ")) )
    (insert (concat ret-type " ::= ")))
  ;;parameters
  (silver-get-param)
  ;;add braces
  (insert "\n{\n\n}")
  (forward-line -1)
  (silver-indent-line))

;;menu for silver functions
(defvar silver-menu
  '("Silver"
    ["Compile the current grammar"   silver-compile              t]
    ["Show next error"               silver-next-error           t]
    ["Show previous error"           silver-prev-error           t]
    ["Insert production skeleton"    silver-insert-production    t]
    ["Insert function skeleton"      silver-insert-function      t]))


;;all this compilation stuff stolen from the mode for Teyjus and modified

;; Location of last error message
(defvar silver-last-error nil)

;; Regular expression for errors/Warnings
(defvar silver-error-regexp
  "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(error\\)\\|\\(warning\\)")

;; Function to find the next error in the *silver* buffer,
;; split the window, and goto the line in the program with
;; the error.
(defun silver-next-error ()
  (interactive)
  (set-buffer "*silver*")
  (condition-case nil
      (progn
        (if silver-last-error
            (progn (goto-char silver-last-error)
                   (end-of-line))
          (goto-char (point-min)))
        (re-search-forward silver-error-regexp)
        (setq silver-last-error (point))
        (silver-show-error))
      (error (message "No more errors"))))

;; Function to find the previous error in the *silver* buffer,
;; split the window, and goto the line in the program with
;; the error.
(defun silver-prev-error ()
  (interactive)
  (set-buffer "*silver*")
  (condition-case ()
      (progn
        (if silver-last-error
            (progn (goto-char silver-last-error)
                   (beginning-of-line)))
        (re-search-backward silver-error-regexp)
        (setq silver-last-error (point))
        (silver-show-error))
    (error (message "No more errors"))))

;; Pop up the file with the error and go to the location of the error.
(defun silver-show-error ()
  (interactive)
  (delete-other-windows)
  (let ((filename
         (buffer-substring (match-beginning 1) (match-end 1)))
        (line-number
         (string-to-int
          (buffer-substring (match-beginning 2) (match-end 2))))
        (column-number
         (string-to-int
          (buffer-substring (match-beginning 3) (match-end 3)))))
    (switch-to-buffer (find-file-noselect filename))
    (goto-line line-number)
    (goto-char (+ (point) column-number))
    (set-window-buffer (split-window) "*silver*")))

;;heavily modified from its Teyjus counterpart
(defun silver-compile ()
  "Compile the current grammar with Silver"
  (interactive)
  (let ( (module (silver-get-grammar-name))
         (dirs (file-name-directory (buffer-file-name (current-buffer)))) )
    (let ( (loc (silver-get-location dirs
                                     (replace-regexp-in-string ":" "/" module)
                                     0)) )
      (if (get-buffer "*silver*") (kill-buffer "*silver*"))
      (setq silver-last-error nil)
      (delete-other-windows)
      (set-window-buffer (split-window)
                         (make-comint "silver" "silver" nil "-I" loc module)))))
;; find the location of the grammar to pass to silver
(defun silver-get-location (string-to-match grammar-dirs start)
  (if (string-match grammar-dirs string-to-match start)
      (let ( (m-start (match-beginning 0)) (m-end (match-end 0)) )
        (let ( (ret (silver-get-location string-to-match grammar-dirs m-end)) )
          (if ret
              ret
            (substring string-to-match 0 m-start)))) ;;might me m-start - 1
    nil))
;; find name of grammar to compile
(defun silver-get-grammar-name ()
  (save-excursion
    (goto-line 1)
    (if (re-search-forward "grammar *\\([^ ;]+\\)[ ;]" (point-max))
        (buffer-substring (match-beginning 1) (match-end 1))
      nil)))
