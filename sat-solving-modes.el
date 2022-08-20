;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        DIMACS (CNF) FILES                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dimacs-mode)

;; automatically enter this mode for *.dimacs and *.cnf
(setq auto-mode-alist (cons '("\\.cnf\\'" . dimacs-mode)
                            (cons '("\\.dimacs\\'" . dimacs-mode) auto-mode-alist)))

;; hook for modifying the mode without modifying the mode
(defvar dimacs-mode-hook '()
  "*Hook for customizing DIMACS mode")

;; function for setting up mode
(defun dimacs-mode ()
  "Mode for viewing DIMACS (CNF) files."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode `dimacs-mode)
  (setq mode-name "DIMACS")
  ;;syntax highlighting
  (set (make-local-variable 'font-lock-defaults)
       '(dimacs-font-lock-keywords))
  (turn-on-font-lock)
  ;;hook for user changes
  (run-hooks 'dimacs-mode-hook))

;; syntax highlighting
(defvar dimacs-font-lock-keywords
  (list
   ;; Header
   (cons "p cnf [0-9]+ [0-9]+" font-lock-type-face)
   ;; Comments
   (cons "c.*" font-lock-comment-face)
   ;; Literals
   (cons "\\(-\\)?[0-9]+[0-9]" font-lock-keyword-face)
   (cons "\\(-\\)?[0-9]*[1-9]" font-lock-keyword-face)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            LRAT FILES                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'lrat-mode)

;; automatically enter this mode for *.lrat
(setq auto-mode-alist (cons '("\\.lrat\\'" . lrat-mode) auto-mode-alist))

;; hook for modifying the mode without modifying the mode
(defvar lrat-mode-hook '()
  "*Hook for customizing LRAT mode")

;; function for setting up mode
(defun lrat-mode ()
  "Mode for editing LRAT proofs."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode `lrat-mode)
  (setq mode-name "LRAT Proof")
  ;;syntax highlighting
  (set (make-local-variable 'font-lock-defaults)
       '(lrat-font-lock))
  (turn-on-font-lock)
  ;;hook for user changes
  (run-hooks 'lrat-mode-hook))

;; syntax highlighting
(defvar lrat-font-lock
  (list
   ;; Comments
   (cons "c.*" font-lock-comment-face)
   ;; Clause ID
   (cons "^[ ]*[0-9]+" font-lock-string-face)))
;;Delete
(font-lock-add-keywords 'lrat-mode
  '(("\\(d\\([ ]+[0-9]+\\)+\\)[ ]+0[ ]*$"
     1 font-lock-builtin-face t)))
;;Literals
(font-lock-add-keywords 'lrat-mode
  '(("^[ ]*[0-9]+\\(\\([ ]+[-]?[1-9][0-9]*\\)*\\)[ ]+0"
     1 font-lock-keyword-face t)))
;;Proof hints
(font-lock-add-keywords 'lrat-mode
  '(("\\(\\([ ]+[-]?[1-9][0-9]*\\)*\\)[ ]+0[ ]*$"
     1 font-lock-function-name-face t)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            DRAT FILES                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'drat-mode)

;; automatically enter this mode for *.drat and *.cnf
(setq auto-mode-alist (cons '("\\.drat\\'" . drat-mode) auto-mode-alist))

;; hook for modifying the mode without modifying the mode
(defvar drat-mode-hook '()
  "*Hook for customizing DRAT mode")

;; function for setting up mode
(defun drat-mode ()
  "Mode for editing DRAT proofs."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode `drat-mode)
  (setq mode-name "DRAT")
  ;;syntax highlighting
  (set (make-local-variable 'font-lock-defaults)
       '(drat-font-lock-keywords))
  (turn-on-font-lock)
  ;;hook for user changes
  (run-hooks 'drat-mode-hook))

;; syntax highlighting
(defvar drat-font-lock-keywords
  (list
   ;; Comments
   (cons "c.*" font-lock-comment-face)
   ;; Literals
   (cons "\\(-\\)?[0-9]+[0-9]" font-lock-keyword-face)
   (cons "\\(-\\)?[0-9]*[1-9]" font-lock-keyword-face)))
;;Delete
(font-lock-add-keywords 'drat-mode
  '(("\\(d\\([ ]+[-]?[0-9]+\\)+\\)[ ]+0[ ]*$"
     1 font-lock-builtin-face t)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            FRAT FILES                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'frat-mode)

;;This only supports a subset of FRAT:  o, a, d, f, and t lines

;; automatically enter this mode for *.frat
(setq auto-mode-alist (cons '("\\.frat\\'" . frat-mode) auto-mode-alist))

;; hook for modifying the mode without modifying the mode
(defvar frat-mode-hook '()
  "*Hook for customizing FRAT mode")

;; function for setting up mode
(defun frat-mode ()
  "Mode for editing FRAT proofs."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode `frat-mode)
  (setq mode-name "FRAT Proof")
  ;;syntax highlighting
  (set (make-local-variable 'font-lock-defaults)
       '(frat-font-lock))
  (turn-on-font-lock)
  ;;hook for user changes
  (run-hooks 'frat-mode-hook))

;; syntax highlighting
(defvar frat-font-lock
  (list
   ;; Comments
   (cons "c.*" font-lock-comment-face)
   ;; Characters
   (cons "^[ ]*a" font-lock-variable-name-face)
   (cons "[ ]+l" font-lock-function-name-face)
   (cons "^[ ]*d" font-lock-builtin-face)
   (cons "^[ ]*o" font-lock-type-face)
   (cons "^[ ]*f" font-lock-type-face)
   ;; Todo lines
   (cons "t.*" font-lock-constant-face)))
;;Delete
(font-lock-add-keywords 'frat-mode
  '(("d[ ]+[0-9]+\\(\\([ ]+[0-9]+\\)+\\)[ ]+0[ ]*$"
     1 font-lock-builtin-face t)))
;;Literals
(font-lock-add-keywords 'frat-mode
  '(("^[ ]*[oadf][ ]+[0-9]+\\(\\([ ]+[-]?[1-9][0-9]*\\)*\\)[ ]+0"
     1 font-lock-keyword-face t)))
;;Proof hints
(font-lock-add-keywords 'frat-mode
  '(("l[ ]*\\(\\([ ]+[-]?[1-9][0-9]*\\)*\\)[ ]+0[ ]*$"
     1 font-lock-function-name-face t)))
;;Clause ID
(font-lock-add-keywords 'frat-mode
  '(("^[ ]*[oadf][ ]+\\([0-9]+\\)"
     1 font-lock-string-face t)))
