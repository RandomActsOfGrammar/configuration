(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4))

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
 '(proof-locked-face ((t (:background "gray22")))))

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
  (set-frame-parameter (selected-frame) 'alpha value))
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

;;Limit width of lines in text mode--automatically shortens when you hit
;;    <enter> or do a <space>
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;tabs are spaces
(setq-default indent-tabs-mode nil)


;;opening *.m will put it into Octave(Matlab) mode
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;;major mode for editing Silver grammars
(load "~/configuration/silver-mode.el")

;;Open .v files with Proof General's Coq mode
(load "~/.emacs.d/lisp/PG/generic/proof-site")

;;Include Teyjus Emacs
(load "~/programs/teyjus/emacs/teyjus.el")

;;Abella things
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

;;OCaml mode
(load
   "~/.opam/system/share/emacs/site-lisp/tuareg-site-file")
