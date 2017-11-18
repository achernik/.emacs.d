(add-to-list 'auto-mode-alist '("\\.arb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-hook 'ruby-mode-hook 'smartparens-mode)
(add-hook 'ruby-mode-hook #'rubocop-mode)


(setq ruby-insert-encoding-magic-comment nil)
(setq ruby-deep-arglist nil)
(setq ruby-deep-indent-paren nil)

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
	indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
	(setq offset (- column (current-column)))
	(when (and (eq (char-after) ?\))
		   (not (zerop (car state))))
	  (goto-char (cadr state))
	  (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))
