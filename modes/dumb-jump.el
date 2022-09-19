(with-eval-after-load 'dumb-jump
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

  ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  ;; (setq xref-show-definitions-function 'xref-show-definitions-buffer)

  (let ((map dumb-jump-mode-map))
    (define-key map (kbd "C-c .") 'xref-find-definitions-other-window)
    (define-key map (kbd "C-c ,") 'xref-pop-marker-stack)
    (define-key map (kbd "C-c *") 'dumb-jump-back)
    (define-key map (kbd "C-c q") 'dumb-jump-quick-look)))
