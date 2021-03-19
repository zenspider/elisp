(define-key ielm-map (kbd "C-c e") 'rwd-shell-clear)

(turn-on-elisp-slime-nav-mode)
(paredit-mode +1)

(require 'eval-in-repl)
(require 'eval-in-repl-ielm)

(setq eir-jump-after-eval nil)
(setq eir-ielm-eval-in-current-buffer t)

(define-key emacs-lisp-mode-map       (kbd "<C-return>") 'eir-eval-in-ielm)
(define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
(define-key Info-mode-map             (kbd "<C-return>") 'eir-eval-in-ielm)
