(add-to-list 'load-path "~/.emacs.d/lisp/")

(load "cl" t)
(push 'balance-windows window-size-change-functions)
(savehist-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq vc-handled-backends nil)
(menu-bar-mode 0)
(when (functionp 'scroll-bar-mode)
  (scroll-bar-mode 0))
(setq inhibit-startup-message t
      initial-scratch-message nil
      inhibit-startup-buffer-menu t
      message-log-max 10000
      default-major-mode 'text-mode)
(setq split-height-threshold nil)
(tool-bar-mode 0)
(setq-default fill-column 80)
(setq fill-column 80)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'right)
(setq viper-mode t)

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(make-directory "~/.emacs.d/autosaves/" t)
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;;; Extra loads

(when (load "package" t)
  (mapc (lambda (elem)
	  (add-to-list 'package-archives elem t))
        '(("tromey" . "http://tromey.com/elpa")
          ("marmalade" . "http://marmalade-repo.org/packages/")
          ("melpa" . "http://melpa.milkbox.net/packages/")))
  (package-initialize))

(when (load "viper" t)
  (viper-mode))

(when (load "rainbow-delimiters" t)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))

(when (load "linum" t)
  (setq linum-format "%4d ")
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'text-mode-hook 'linum-mode))

(when (load "cc-mode" t)
  (setq-default c-basic-offset     4
                tab-width          4
                indent-tabs-mode   nil
                c-default-style    "k&r"))

(when (load "fill-column-indicator" t)
  (setq fci-rule-width 1)
  (setq fci-rule-color "#333333")
  (add-hook 'prog-mode-hook 'fci-mode)
  (add-hook 'text-mode-hook 'fci-mode))

(when (load "color-theme" t)
  (color-theme-initialize)
  (when (load "color-theme-kilcros" t)
    (color-theme-kilcros)))

(when (load "scheme-mode" t)
  (set 'scheme-program-name "csi")
  (add-to-list 'load-path "/usr/lib/chicken/6/")
  (autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)
  (add-hook 'scheme-mode-hook (lambda ()
                                (slime-mode t))))

(load (expand-file-name "~/quicklisp/slime-helper.el") t)
(setq inferior-lisp-program "sbcl")
(when (load "slime" t)
  (slime-setup '(slime-fancy slime-banner))
  (defun slime-start-here ()
    (interactive)
    (slime-start)
    (delete-window)
    (switch-to-buffer "*inferior-lisp*")))

(when (load "haskell-mode" t)
  (add-to-list 'auto-mode-alist
               '("\\.hs\\'" . haskell-mode)))

(when (load "windmove" t)
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings))
  (put 'downcase-region 'disabled nil))
