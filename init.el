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
	;; (clj-refactor         . "melpa-stable")
	(rubocop              . "melpa-stable")
	(rust-mode            . "melpa-stable")
	(magit                . "melpa-stable")
	(flycheck             . "merpa-stable")

	(base16-theme         . "melpa")
	(sql-indent           . "melpa")
	(use-package          . "melpa")
	(dockerfile-mode      . "melpa")

	(undo-tree            . "gnu")
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

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil);; Minor modes
(global-set-key "\M-`" 'other-frame)

(add-hook 'server-visit-hook 'raise-frame)

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package init-loader)

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package base16-theme
  :ensure t
  :config (load-theme 'base16-tomorrow-night t))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 4))


(use-package yasnippet
  :init
  (yas-global-mode))

(use-package smartparens)

(use-package git-gutter-fringe
  :init
  (custom-set-variables '(git-gutter:update-interval 1))
  (setq-default left-fringe-width 15)
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
;; (use-package clj-refactor
;;   :init
;;   (setq cljr-warn-on-eval nil))

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

(use-package dockerfile-mode)

(use-package rubocop)

(use-package rust-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


(init-loader-load)

(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)

;; before-make-frame-hook
;; (toggle-frame-maximized)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 '(git-gutter:update-interval 1)
 '(package-selected-packages
   (quote
    (yasnippet-snippets yaml-mode web-mode use-package undo-tree sql-indent smartparens slim-mode scss-mode rust-mode rubocop rspec-mode rainbow-delimiters projectile markdown-mode magit init-loader idomenu ido-vertical-mode haml-mode git-gutter-fringe flycheck flx-ido expand-region exec-path-from-shell dockerfile-mode company coffee-mode clj-refactor browse-kill-ring base16-theme avy ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
