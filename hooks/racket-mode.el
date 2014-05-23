(require 'paredit)
(paredit-mode +1)

(define-key racket-mode-map (kbd "C-c r") 'racket-run)
(define-key racket-mode-map (kbd "C-c t") 'racket-test)

(defun paredit-space-for-delimiter-predicates-scheme (endp delimiter)
  "Do not automatically insert a space when a '#' precedes parentheses."
  (or endp
      (cond ((eq (char-syntax delimiter) ?\")
             (not (looking-back "#\\|#hash\\|#rx\\|#px")))
            (t t))))

(add-to-list 'paredit-space-for-delimiter-predicates
             'paredit-space-for-delimiter-predicates-scheme)
