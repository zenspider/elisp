(require 'paredit)
(paredit-mode +1)

(message "Racket mode has run!")

(define-key racket-mode-map (kbd "C-c C-r") 'racket-run)
(define-key racket-mode-map (kbd "C-c t") 'racket-test)

(defun paredit-space-for-delimiter-predicates-scheme (endp delimiter)
  "Do not automatically insert a space when a '#' precedes parentheses."
  (or endp
      (cond ((eq (char-syntax delimiter) ?\")
             (not (looking-back "#\\|#hash\\|#rx\\|#px")))
            (t t))))

(put 'test-case 'racket-indent-function 1)
(put 'test-suite 'racket-indent-function 1)
(put 'call-with-output-file* 'racket-indent-function 1)

(add-to-list 'paredit-space-for-delimiter-predicates
             'paredit-space-for-delimiter-predicates-scheme)
