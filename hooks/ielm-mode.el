;; some code adapted from https://www.n16f.net/blog/making-ielm-more-comfortable

(elisp-slime-nav-mode)
(paredit-mode +1)
(eldoc-mode)

(require 'eval-in-repl)
(require 'eval-in-repl-ielm)

(define-key ielm-map (kbd "C-c e") 'rwd-shell-clear)
(rwd-keymap-holepunch 'paredit-mode "RET" "C-j") ; this punches through paredit

(setq eir-jump-after-eval nil)
(setq eir-ielm-eval-in-current-buffer t)

(define-key emacs-lisp-mode-map       (kbd "<C-return>") 'eir-eval-in-ielm)
(define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
(define-key Info-mode-map             (kbd "<C-return>") 'eir-eval-in-ielm)

(let ((path (expand-file-name "ielm/history" user-emacs-directory)))
  (make-directory (file-name-directory path) t)
  (setq-local comint-input-ring-file-name path))
(setq-local comint-input-ring-size 10000)
(setq-local comint-input-ignoredups t)
(comint-read-input-ring)

(add-hook 'comint-input-filter-functions #'rwd/ielm/save-history)
