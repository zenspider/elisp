(aggressive-indent-mode)
(paredit-mode +1)

(setq font-lock-keywords-case-fold-search nil)

(defun scheme-send-file ()
  (interactive)
  (scheme-send-region (point-min) (point-max)))

(define-key scheme-mode-map "\C-c\C-f" 'scheme-send-file)
