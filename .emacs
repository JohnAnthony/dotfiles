(add-to-list 'load-path "~/.emacs.d/lisp/")

(push 'balance-windows window-size-change-functions)
(defalias 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode 0)

(setq inhibit-startup-message t
      initial-scratch-message nil
      inhibit-startup-buffer-menu t
      message-log-max 10000
      major-mode 'text-mode)
(setq split-height-threshold nil)
(tool-bar-mode 0)
(setq-default fill-column 80)
(setq fill-column 80)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'right)
(put 'downcase-region 'disabled nil)
(server-start)

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(make-directory "~/.emacs.d/autosaves/" t)
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Helpers

(defun tryload (str)
  (let ((result (load str t)))
    (when (not result)
      (message ":: Error loading %s" str))
    result))

;;; Settings for various packages

(when (symbolp 'vc-handled-backends)
  (setq vc-handled-backends nil))

(when (functionp 'scroll-bar-mode)
  (scroll-bar-mode 0))

;;; Extra loads

(when (tryload "savehist")
  (savehist-mode))

(when (tryload "package")
  (mapc (lambda (elem)
	  (add-to-list 'package-archives elem t))
        '(("tromey" . "http://tromey.com/elpa")
          ("marmalade" . "http://marmalade-repo.org/packages/")
          ("melpa" . "http://melpa.milkbox.net/packages/")))
  (package-initialize))

(if (tryload "evil")
    (evil-mode 1)
  (when (and (setq viper-mode t)
             (tryload "viper"))
    (viper-mode)))

;(when (tryload "rainbow-delimiters")
;  (add-hook 'javascript-mode-hook 'rainbow-delimiters-mode-enable)
;  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode-enable)
;  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode-enable)
;  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode-enable))

(when (tryload "linum")
  (setq linum-format "%4d ")
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'text-mode-hook 'linum-mode))

(when (tryload "cc-mode")
  (setq-default c-basic-offset     4
                tab-width          4
                indent-tabs-mode   nil
                c-default-style    "k&r"))

(when (tryload "fill-column-indicator")
  (setq fci-rule-width 1)
  (setq fci-rule-color "#333333")
  (add-hook 'prog-mode-hook 'fci-mode)
  (add-hook 'text-mode-hook 'fci-mode))

(when (tryload "color-theme")
  (color-theme-initialize)
  (when (tryload "color-theme-kilcros")
    (color-theme-kilcros)))

(when (and (tryload (expand-file-name "~/quicklisp/slime-helper.el"))
           (setq inferior-lisp-program "sbcl")
           (tryload "slime"))
  (slime-setup '(slime-fancy slime-banner))
  (defun slime-start-here ()
    (interactive)
    (slime-start)
    (delete-window)
    (switch-to-buffer "*inferior-lisp*")))

(when (tryload "haskell-mode")
  (add-to-list 'auto-mode-alist
               '("\\.hs\\'" . haskell-mode))
  (haskell-simple-indent-mode))

(when (tryload "windmove")
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings)))

(when (tryload "geiser")
  (setq geiser-guile-load-path '("/home/john/scheme"))
  (setq geiser-repl-use-other-window nil)
  (setq geiser-scheme-implementation "guile"))

(when (tryload "web-mode")
  (add-to-list 'auto-mode-alist
               '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.htm\\'" . web-mode)))

;;; Mode-specific stuff

(when (tryload "typescript")
  (defun typescript-compile ()
    (interactive)
    (shell-command (concat "tsc " (buffer-name))))
  (defalias 'tsc 'typescript-compile)
  (define-key typescript-mode-map [(control ?c) (control ?b)] #'tsc))

(when (tryload "go-mode")
  (defun go-build ()
    (interactive)
    (shell-command (concat "go build " (buffer-name))))
  (define-key go-mode-map [(control ?c) (control ?b)] #'go-build))

;; Emacs lisp mode
(define-key emacs-lisp-mode-map [(control ?c) (control ?b)] #'emacs-lisp-byte-compile)
