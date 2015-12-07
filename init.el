(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)

(require 'use-package)
(require 'init-loader)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq ido-save-directory-list-file
      (concat temporary-file-directory
	      "ido.last"))

(setq make-backup-files nil)
(setq auto-save-default nil)

(add-hook 'before-save-hook 'whitespace-cleanup)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode))

(setq mac-command-key-is-meta)
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil);; Minor modes
(global-set-key "\M-`" 'other-frame)

(add-hook 'server-visit-hook 'raise-frame)

(use-package smartparens
  :ensure t)

(use-package git-gutter-fringe
  :ensure t
  :init
  (custom-set-variables '(git-gutter:update-interval 1))
  (global-git-gutter-mode 1))

(global-linum-mode 1)

(use-package paredit
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package undo-tree
  :ensure t)

(use-package ace-jump-mode
  :ensure t
  :bind ("C-o" . ace-jump-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package ag
  :ensure t)

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package cyberpunk-theme
  :ensure t)

(use-package idomenu
  :ensure t)

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

(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)

before-make-frame-hook
;; (toggle-frame-maximized)
