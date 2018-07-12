(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(scroll-bar-mode -1)

(blink-cursor-mode 0)

(tool-bar-mode -1)
(setq inhibit-startup-message t)
(setq echo-keystrokes 0.1)

(set-face-attribute 'default nil :family "Liberation Mono"
				 :height 130
				 :weight 'normal)

(set-face-foreground 'git-gutter-fr:modified "#ffcc66")
(set-face-foreground 'git-gutter-fr:added "#99cc99")
(set-face-foreground 'git-gutter-fr:deleted "#d27b53")

(setq c-basic-offset 2)
(setq css-indent-offset 2)

(setq color-theme-is-global t)
(setq ring-bell-function 'ignore)
(setq init-loader-show-log-after-init nil)

(global-hl-line-mode 1)
(load-theme 'base16-eighties t)

(require 'ansi-color)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(set-fringe-mode '(4 . 0))
(setq-default truncate-lines nil)
