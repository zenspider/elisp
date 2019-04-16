(paredit-mode 1)
;; (aggressive-indent-mode)
(turn-on-elisp-slime-nav-mode)

(let ((map elisp-slime-nav-mode-map))
  (define-key map (kbd "C-c C-d d")   nil)
  (define-key map (kbd "C-c C-d C-d") nil))

(define-key emacs-lisp-mode-map       (kbd "<M-return>") 'eir-eval-in-ielm)
(define-key lisp-interaction-mode-map (kbd "<M-return>") 'eir-eval-in-ielm)
(define-key lisp-interaction-mode-map (kbd "C-M-<return>") 'eval-defun)

(defun rwd-elisp-sanity-check ()
  (interactive)
  (checkdoc)
  (byte-compile-file (buffer-file-name))
  (package-buffer-info))
