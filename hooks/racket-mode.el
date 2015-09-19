(paredit-mode +1)
(aggressive-indent-mode)

(define-key racket-mode-map (kbd "C-c C-r")     'racket-run)
(define-key racket-mode-map (kbd "C-c t")       'racket-test)
(define-key racket-mode-map (kbd "C-c C-c C-d") 'racket-doc)
(define-key racket-mode-map (kbd "C-c C-d")     'delete-trailing-whitespace)

(defun paredit-space-for-delimiter-predicates-scheme (endp delimiter)
  "Do not automatically insert a space when a '#' precedes parentheses."
  (or endp
      (cond ((eq (char-syntax delimiter) ?\")
             (not (looking-back "#\\|#hash\\|#rx\\|#px")))
            (t t))))

;; (let ((defines  '(struct local struct: define-struct: ...
;;       (begins   '(case-lambda case-lambda: pcase-lambda: ...
;;       (lambdas  '(cases instantiate super-instantiate syntax/loc ...
;;       (specials '(for/fold for/fold:)))

(let ((defines  '())
      (begins   '(check-run* cond-e cond-a cond-u))
      (lambdas  '(λg λf test-case test-suite call-with-output-file*
                     lambdag@ lambdaf@ exists exist nom project run* run1
                     run2 run3 run4 run5 run6 run7 run8 run9 run10
                     case∞ case-inf take fresh))
      (specials '(run)))

  (mapc (lambda (name) (put name 'racket-indent-function 'defun)) defines)
  (mapc (lambda (name) (put name 'racket-indent-function 0))      begins)
  (mapc (lambda (name) (put name 'racket-indent-function 1))      lambdas)
  (mapc (lambda (name) (put name 'racket-indent-function 2))      specials))

;; (put 'fresh 'racket-indent-function 4)

(add-to-list 'paredit-space-for-delimiter-predicates
             'paredit-space-for-delimiter-predicates-scheme)
