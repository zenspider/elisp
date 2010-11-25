(setq shell-dirstack-query "dirs -l")

(define-key shell-mode-map (kbd "C-z")        'comint-stop-subjob)
(define-key shell-mode-map (kbd "M-<return>") 'shell-resync-dirs)
(define-key comint-mode-map [C-up]            'rwd-previous-line-6)
(define-key comint-mode-map [C-down]          'rwd-forward-line-6)
