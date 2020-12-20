(setq ansi-color-for-comint-mode 'filter)

(setq shell-dirstack-query "dirs -l")

(define-key shell-mode-map (kbd "C-c C-d") nil)  ; keeps biting me
(define-key shell-mode-map (kbd "C-z C-z")    'comint-stop-subjob)
(define-key shell-mode-map (kbd "M-<return>") 'shell-resync-dirs)

(define-key comint-mode-map (kbd "<C-return>") 'rwd/comint/send-input)
(define-key comint-mode-map (kbd "C-<up>")    'rwd-previous-line-6)
(define-key comint-mode-map (kbd "C-<down>")  'rwd-forward-line-6)
(define-key comint-mode-map (kbd "C-c e")     'rwd-shell-clear)
(define-key comint-mode-map (kbd "C-c C-t")   'rwd/comint-info-subjob)
