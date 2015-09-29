(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")

(cask-initialize)

(require 'use-package)
(require 'init-loader)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Minor modes

(use-package paredit
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package undo-tree
  :ensure t)

(use-package ace-jump-mode
  :bind ("C-o" . ace-jump-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package ag)

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package cyberpunk-theme
  :ensure t)

(use-package idomenu)
(use-package flx-ido
  :ensure t
  :init
  (setq ido-create-new-buffer 'always)
  (setq ido-enable-flex-matching t)
  (setq ido-enable-prefix nil)
  (setq ido-max-prospects 8)
  (setq ido-default-file-method 'selected-window)
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  :config
  (ido-mode t)
  (icomplete-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1))

(use-package projectile
  :ensure t
  :init
  (setq projectile-switch-project-action 'projectile-dired)
  :config
  (projectile-global-mode))

;; Major modes
;;

(use-package ruby-mode)
(use-package rspec-mode)

(use-package scss-mode
  :init
  (setq scss-compile-at-save nil))

(use-package coffee-mode)
(use-package slim-mode)
(use-package markdown-mode)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

(use-package haml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode)))

(init-loader-load)
