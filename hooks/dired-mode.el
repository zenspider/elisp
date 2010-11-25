(define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)

(if running-emacs
    (define-key dired-mode-map "k" 'dired-kill-subdir))
