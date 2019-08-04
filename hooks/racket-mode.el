(paredit-mode +1)
(racket-unicode-input-method-enable)

(define-key racket-mode-map (kbd "C-c u")       'racket-unicode-input-method-enable)
(define-key racket-mode-map (kbd "C-c C-r")     'racket-run)
(define-key racket-mode-map (kbd "C-c r")       'racket-run)
(define-key racket-mode-map (kbd "C-c t")       'racket-test)
(define-key racket-mode-map (kbd "C-c C-d")     'delete-trailing-whitespace)
(define-key racket-mode-map (kbd "<C-return>")  'eir-eval-in-racket)
;; (define-key racket-mode-map (kbd "C-c C-c C-d") 'racket-doc)

(defun paredit-space-for-delimiter-predicates-scheme (endp delimiter)
  "Do not automatically insert a space when a '#' precedes parentheses."
  (or endp
      (cond ((eq (char-syntax delimiter) ?\")
             (not (looking-back "#\\|#hash\\|#rx\\|#px" nil)))
            (t t))))

(add-to-list 'paredit-space-for-delimiter-predicates
             'paredit-space-for-delimiter-predicates-scheme)

(let ((defines (append
                '(reduction-relation)
                (mapcar (lambda (n) (intern (make-string n ?-))) ; redex
                        (number-sequence 3 10))))
      (begins   '(check-run* cond-e cond-a cond-u
                             check-equal? test-equal))
      (lambdas  '(λg λf test-case test-suite call-with-output-file*
                     lambdag@ lambdaf@ exists exist nom project run* run1
                     run2 run3 run4 run5 run6 run7 run8 run9 run10
                     case∞ case-inf take fresh
                     pattern-case-filter))
      (specials '((for/fold for/fold/1)
                  (for/list for/list/flat)
                  (for/or for/all for/none)
                  (for/sum for/count)
                  (do run))))

  (mapc (lambda (name) (put name 'racket-indent-function 'defun)) defines)
  (mapc (lambda (name) (put name 'racket-indent-function 0))      begins)
  (mapc (lambda (name) (put name 'racket-indent-function 1))      lambdas)

  ;; (put 'module 'racket-indent-function 2)

  (mapc (lambda (txs)
          (let* ((type (car txs))
                 (xs (cdr txs))
                 (dent (get type 'racket-indent-function)))
            (mapc (lambda (x)
                    (put x 'racket-indent-function dent))
                  xs)))
        specials))

(defadvice racket-test (before racket-test-clear compile activate)
  (with-racket-repl-buffer
    (rwd-shell-clear)))

(defadvice racket-run (before racket-run-clear compile activate)
  (with-racket-repl-buffer
    (rwd-shell-clear)))
