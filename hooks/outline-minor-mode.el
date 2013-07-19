(require 'outline-magic)

(let ((map outline-minor-mode-map))
  (define-key map (kbd "M-o a")   'show-all)

  (define-key map (kbd "M-o u")   'outline-up-heading)
  (define-key map (kbd "M-o n")   'outline-next-visible-heading)
  (define-key map (kbd "M-o p")   'outline-previous-visible-heading)
  (define-key map (kbd "M-o f")   'outline-forward-same-level)
  (define-key map (kbd "M-o b")   'outline-backward-same-level)

  ;; from outline-magic
  (define-key map (kbd "M-o M-o") 'outline-cycle-fast)
  (define-key map (kbd "M-o o")   'outline-cycle))
