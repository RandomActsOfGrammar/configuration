(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 ;;Proof General configuration
 '(proof-disappearing-proofs t)
 '(proof-three-window-mode-policy (quote hybrid)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "WhiteSmoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal))))
 '(font-lock-builtin-face ((t (:foreground "purple"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "red"))))
 '(font-lock-comment-face ((t (:foreground "red" :slant oblique))))
 '(font-lock-constant-face ((t (:foreground "orange" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "dodger blue" :weight normal :height 1.0))))
 '(font-lock-keyword-face ((t (:foreground "dark orange"))))
 '(font-lock-string-face ((t (:foreground "lime green"))))
 '(font-lock-type-face ((t (:foreground "deep sky blue"))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
 ;; Proof General
 '(proof-locked-face ((t (:background "gray10"))))
 ;; Markdown
 '(markdown-inline-code-face ((t (:inherit (markdown-code-face font-lock-constant-face) font-lock-constant-face nil))))
 '(markdown-header-delimiter-face ((t (:inherit markdown-header-face))))
 '(markdown-header-rule-face ((t (:inherit markdown-header-face))))
 '(markdown-list-face ((t (:inherit font-lock-type-face))))
 '(markdown-hr-face ((t (:inherit font-lock-function-name-face))))
 '(markdown-markup-face ((t (:inherit font-lock-variable-name-face))))
 ;; Agda
 '(agda2-highlight-symbol-face ((t (:inherit font-lock-string-face))))
 '(agda2-highlight-coverage-problem-face ((t (:background "gray12" ))))
 '(agda2-highlight-unsolved-meta-face ((t (:background "red1"))))
 '(agda2-highlight-inductive-constructor-face ((t (:inherit font-lock-variable-name-face))))
 '(agda2-highlight-record-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-field-face ((t (:inherit font-lock-variable-name-face))))
 '(agda2-highlight-primitive-type-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-number-face ((nil)))
 '(agda2-highlight-postulate-face ((t (:inherit font-lock-builtin-face))))
 '(agda2-highlight-datatype-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-function-face ((t (:inherit font-lock-function-name-face))))
 '(agda2-highlight-keyword-face ((t (:inherit font-lock-keyword-face)))))

;;change the minibuffer color for when we are in a terminal
;;the default doesn't really show up as it is dark blue
(set-face-foreground 'minibuffer-prompt "yellow")

;;set it so the background isn't black in the terminal while the other colors
;;   remain the same("nothing" just needs to be an undefined color)
;;stolen from https://stackoverflow.com/questions/19054228/
;;                              emacs-disable-theme-background-color-in-terminal
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "nothing" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;;Trying to make it transparent--https://www.emacswiki.org/emacs/TransparentEmacs
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(75 50))
(add-to-list 'default-frame-alist '(alpha 75 50))
(eval-when-compile (require 'cl))
;;a way to toggle the transparency -- "C-c t"
(defun toggle-transparency ()
   (interactive)
   (if (/=
        (cadr (frame-parameter nil 'alpha))
        100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(75 50))))
(global-set-key (kbd "C-c t") 'toggle-transparency)
;;set the transparency to a value -- "C-c C-t"
(defun transparency (value)
  "Sets the transparency of the frame window.  0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha (list value value)))
(global-set-key (kbd "C-c C-t") 'transparency)

;;change title to match buffer title
(setq frame-title-format "%b : %f")

;;turn on column number mode automatically
(column-number-mode)

;;stop it from beeping or flashing
(setq ring-bell-function 'ignore)

;;resize horizontally because it doesn't let you use a mouse
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)
;;resize vertically to make it easier (overwrites something I never used)
(global-set-key (kbd "<C-up>") 'enlarge-window)
(global-set-key (kbd "<C-down>") 'shrink-window)

;;move the view up or down one line
(global-set-key (kbd "C-o") 'scroll-down-line)
(global-set-key (kbd "C-j") 'scroll-up-line)
;;move view down, but in LaTeX mode (overrides a different binding there)
(eval-after-load 'latex
  '(define-key LaTeX-mode-map (kbd "C-j") 'scroll-up-line))
;;making it work on my computer, too
(add-hook 'latex-mode-hook
   (lambda () (local-set-key (kbd "C-j") #'scroll-up-line)))

;;Limit width of lines in text mode--automatically shortens when you hit
;;    <enter> or do a <space>
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;tabs are spaces
(setq-default indent-tabs-mode nil)

;;put in an equivalence sign
(global-set-key (kbd "C-=") (lambda () (interactive) (insert "≡")))


;;opening *.m will put it into Octave(Matlab) mode
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;;major mode for editing Silver grammars
(load "~/configuration/silver-mode.el")

;;Open .v files with Proof General's Coq mode
(load "~/.emacs.d/lisp/PG/generic/proof-site")

;;Include Teyjus Emacs
(load "~/programs/teyjus/emacs/teyjus.el")


(defconst proof-site-file
  (expand-file-name "path/to/PG/generic/proof-site.el"))
(defconst lprolog-file
  (expand-file-name "path/to/abella/emacs/lprolog.el"))

(defmacro delete-mappings (alist key)
  `(while (assoc ,key ,alist)
     (setq ,alist (delq (assoc ,key ,alist) ,alist))))

(when (file-exists-p proof-site-file)
  (delete-mappings auto-mode-alist "\\.thm\\'")
  (setq proof-splash-enable nil)
  (setq proof-three-window-enable nil)
  (setq proof-three-window-mode-policy 'horizontal)
  (setq proof-output-tooltips nil)
  (load-file proof-site-file))

(when (file-exists-p lprolog-file)
  (autoload 'lprolog-mode lprolog-file "Major mode for Lambda Prolog" t)
  (delete-mappings auto-mode-alist "\\.mod\\'")
  (add-to-list 'auto-mode-alist '("\\.mod\\'" . lprolog-mode))
  (add-to-list 'auto-mode-alist '("\\.sig\\'" . lprolog-mode)))

(add-hook 'lprolog-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'abella-mode-hook (lambda () (electric-indent-local-mode -1)))

;;Abella response mode seems to have no highlighting by default
(font-lock-add-keywords 'abella-response-mode
  '(("Subgoal" . font-lock-keyword-face)
    ("Variables" . font-lock-keyword-face)
    ("forall" . font-lock-keyword-face)
    ("exists" . font-lock-keyword-face)
    ("Subgoal \\(\\([0-9]+\\.*\\)+\\) is" 1 font-lock-type-face)
    ("Subgoal .* \\(is\\)" 1 font-lock-keyword-face)
    ("\\([-A-Za-z^=`'?$0-9_*@+#!~\/]+\\) :" 1 font-lock-type-face)
    ("Proof completed." . font-lock-string-face)
    ;;trying to highlight variable names in declaration spots
    ("Variables: *\\(\\([-A-Za-z^=`'?$0-9_*@+#!~\/]+ *\\)+\\)" 1 font-lock-variable-name-face)
    ("\\(\\(forall\\)\\|\\(exists\\)\\) +\\(\\([-A-Za-z^=`'?$0-9_*@+#!~\/]+ *\\)+ *\\)," 4 font-lock-variable-name-face)
    )
  )

;;I'd like some of this highlighting in regular Abella mode as well
(font-lock-add-keywords 'abella-mode
  ;;Highlighting variable declarations
    ;;It turns out we another mechanism if we want to highlight the types in declarations and not have all variables there typed
  '(("\\(\\(forall\\)\\|\\(exists\\)\\|\\(nabla\\)\\) +\\(\\(\\([-A-Za-z^=`'?$0-9_*@+#!~\/]+ *\\)\\|\\((\\([-A-Za-z^=`'?$0-9_*@+#!~\/]+ *\\)+: *\\([^,\\.]+\\))\\) *\\)+\\) *," 5 font-lock-variable-name-face) ;;all variables bound at a quantifier
    ;;These two lines cause it to crash if there are too many characters in a comment between opening and closing paretheses for some reason
    ;("(\\([-A-Za-z^=`'?$0-9_*@+#!~\/]+ *\\)+ *: *\\([^,\\.]+\\))" 2 font-lock-type-face) ;;types declared with variables
    ;("(\\(\\([-A-Za-z^=`'?$0-9_*@+#!~\/]+ *\\)+\\) *: *\\([^,\\.]+\\))" 1 font-lock-variable-name-face) ;;variables declared (or used) with types
  ;;Highlighting types for kind, type, and defines
    ("Kind +\\(\\([-A-Za-z^=`'?$0-9_*@+#!~\/]+ *, *\\)*[-A-Za-z^=`'?$0-9_*@+#!~\/]+\\) +[^\\.]+ *." 1 font-lock-type-face) ;;declaring the kind of a type
    ("Type +\\([-A-Za-z^=`'?$0-9_*@+#!~\/]+ *, *\\)*[-A-Za-z^=`'?$0-9_*@+#!~\/]+ +\\([^\\.]+\\) *." 2 font-lock-type-face) ;;declaring the type of a constructor
    ("[-A-Za-z^=`'?$0-9_*@+#!~\/]+ *: *\\([^\n,]+\\) *," 1 font-lock-type-face) ;;early relation in a Define
    ("[-A-Za-z^=`'?$0-9_*@+#!~\/]+ *: *\\([^\n,]+\\) +by" 1 font-lock-type-face) ;;final relation in a Define
  ;;Highlighting name for type and defines
    ("Type +\\(\\([-A-Za-z^=`'?$0-9_*@+#!~\/]+ *, *\\)*[-A-Za-z^=`'?$0-9_*@+#!~\/]+\\) +\\([^\\.]+\\) *." 1 font-lock-variable-name-face) ;;declaring the type of a constructor
    ("\\([-A-Za-z^=`'?$0-9_*@+#!~\/]+\\) *: *\\([^\n,]+\\) *," 1 font-lock-variable-name-face) ;;early relation in a Define
    ("\\([-A-Za-z^=`'?$0-9_*@+#!~\/]+\\) *: *\\([^\n,]+\\) +by" 1 font-lock-variable-name-face) ;;final relation in a Define
    )
  )

;;OCaml mode
(load
   "~/.opam/system/share/emacs/site-lisp/tuareg-site-file")
