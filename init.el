(require 'cl)
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-pinned-packages
      '((init-loader          . "melpa-stable")
	(exec-path-from-shell . "melpa-stable")
	(browse-kill-ring     . "melpa-stable")
	(ido-vertical-mode    . "melpa-stable")
	(flx-ido              . "melpa-stable")
	(projectile           . "melpa-stable")
	(clojure-mode         . "melpa-stable")
	(cider                . "melpa-stable")
	(paredit              . "melpa-stable")
	(smartparens          . "melpa-stable")
	(rainbow-delimiters   . "melpa-stable")
	(rspec-mode           . "melpa-stable")
	(scss-mode            . "melpa-stable")
	(coffee-mode          . "melpa-stable")
	(slim-mode            . "melpa-stable")
	(markdown-mode        . "melpa-stable")
	(git-gutter-fringe    . "melpa-stable")
	(expand-region        . "melpa-stable")
	(ag                   . "melpa-stable")
	(yaml-mode            . "melpa-stable")
	(haml-mode            . "melpa-stable")
	(company              . "melpa-stable")
	(yasnippet            . "melpa-stable")
	(idomenu              . "melpa-stable")
	(web-mode             . "melpa-stable")
	(clj-refactor         . "melpa-stable")

	(base16-theme         . "melpa")
	(sql-indent           . "melpa")
	(use-package          . "melpa")
	(undo-tree            . "melpa")

	(avy                  . "gnu")))

(package-initialize)
(setq package-contents-refreshed nil)

(mapc (lambda (pinned-package)
  (let ((package (car pinned-package))
	(archive (cdr pinned-package)))
    (unless (package-installed-p package)
	    (unless package-contents-refreshed
	      (package-refresh-contents)
	      (setq package-contents-refreshed t))
      (message "Installing %s from %s" package archive)
      (package-install package))))
      package-pinned-packages)

(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

(require 'use-package)
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
(setq require-final-newline t)

(setq mac-command-key-is-meta)
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil);; Minor modes
(global-set-key "\M-`" 'other-frame)

(add-hook 'server-visit-hook 'raise-frame)

(use-package init-loader)

(use-package base16-theme)

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)))

(use-package yasnippet
  :init
  (yas-global-mode))

(use-package smartparens)

(use-package git-gutter-fringe
  :init
  (custom-set-variables '(git-gutter:update-interval 1))
  (global-git-gutter-mode 1))

(global-linum-mode 1)

(use-package paredit)

(use-package rainbow-delimiters)

(use-package undo-tree)

(use-package avy
  :bind
  ("C-;" . avy-goto-word-or-subword-1)
  ("C-M-;" . avy-goto-char-in-line)
  ("M-g g" . avy-goto-line))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package ag)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package idomenu)

(use-package flx-ido
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

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1))

(use-package projectile
  :init
  (setq projectile-switch-project-action 'projectile-dired)
  :config
  (projectile-global-mode))

(defun close-project-buffers-and-kill-frame ()
  (interactive)
  (projectile-kill-buffers)
  (delete-frame))

(global-set-key (kbd "C-x p k") 'close-project-buffers-and-kill-frame)

(use-package ruby-mode)
(use-package rspec-mode)
(use-package clojure-mode)
(use-package clj-refactor
  :init
  (setq cljr-warn-on-eval nil))

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

;; before-make-frame-hook
;; (toggle-frame-maximized)
