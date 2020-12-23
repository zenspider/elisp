(with-eval-after-load 'dumb-jump
  (let ((map dumb-jump-mode-map))
    (define-key map (kbd "C-c .") 'dumb-jump-go-other-window)
    (define-key map (kbd "C-c ,") 'dumb-jump-go-prefer-external-other-window)
    (define-key map (kbd "C-c *") 'dumb-jump-back)
    (define-key map (kbd "C-c q") 'dumb-jump-quick-look)))
