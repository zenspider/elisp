;; -*- coding: utf-8 -*-

(add-to-list 'auto-mode-alist '("\\.rkt$" . scheme-mode))

;; (autoload 'run-scheme                     "cmuscheme"        nil t)
;; (autoload 'scheme-smart-complete          "scheme-complete"  nil t)
;; (autoload 'scheme-complete-or-indent      "scheme-complete"  nil t)
;; (autoload 'scheme-get-current-symbol-info "scheme-complete"  nil t)

(let ((defines '(struct local struct: define-struct:
                        define-typed-struct define-struct/exec:
                        define: pdefine: define-type
                        define-predicate match-define))
      (begins '(case-lambda case-lambda: pcase-lambda:
                            match-lambda match-lambda* cond
                            delay unit compound-unit
                            compound-unit/sig public private
                            override inherit sequence))
      (lambdas `(cases instantiate super-instantiate syntax/loc
                       quasisyntax/loc match match* match-let
                       match-let* match-letrec λ lambda let let*
                       letrec recur lambda/kw letrec-values
                       with-syntax with-continuation-mark module
                       module* module+ match match-let
                       match-let* match-letrec let/cc let/ec
                       letcc catch let-syntax letrec-syntax
                       fluid-let-syntax letrec-syntaxes+values
                       let: letrec: let*: let-values:
                       letrec-values: let*-values: let/cc:
                       let/ec: lambda: λ: plambda: opt-lambda:
                       popt-lambda: splicing-let splicing-letrec
                       splicing-let-values
                       splicing-letrec-values
                       splicing-let-syntax
                       splicing-letrec-syntax
                       splicing-let-syntaxes
                       splicing-letrec-syntaxes
                       splicing-letrec-syntaxes+values
                       splicing-local
                       splicing-syntax-parameterize for for/list
                       for/hash for/hasheq for/hasheqv
                       for/and for/or for/lists for/first
                       for/last for/vector for/flvector
                       for/sum for/product for/set for* for*/list
                       for*/hash for*/hasheq for*/hasheqv
                       for*/and for*/or for*/lists for*/first
                       for*/last for*/fold for*/vector
                       for*/flvector for*/sum for*/product
                       for*/set for: for/list: for/hash:
                       for/hasheq: for/hasheqv: for/and: for/or:
                       for/lists: for/first: for/last:
                       for/vector: for/flvector: for/sum:
                       for/product: for/set: for*: for*/list:
                       for*/hash: for*/hasheq: for*/hasheqv:
                       for*/and: for*/or: for*/lists: for*/first:
                       for*/last: for*/fold: for*/vector:
                       for*/flvector: for*/sum: for*/product:
                       for*/set: do: kernel-syntax-case syntax-case
                       syntax-case* syntax-rules syntax-id-rules
                       let-signature fluid-let let-struct
                       let-macro let-values let*-values case
                       when unless let-enumerate class class*
                       class-asi class-asi* class*/names
                       class100 class100* class100-asi
                       class100-asi* class100*/names rec
                       make-object mixin define-some do
                       opt-lambda send* with-method
                       define-record catch shared unit/sig
                       unit/lang with-handlers interface
                       parameterize parameterize*
                       call-with-input-file
                       call-with-input-file*
                       with-input-from-file with-input-from-port
                       call-with-output-file with-output-to-file
                       with-output-to-port for-all type-case))
      (specials '(for/fold for/fold:)))

  (mapc (lambda (name) (put name 'scheme-indent-function 'defun)) defines)
  (mapc (lambda (name) (put name 'scheme-indent-function 0))      begins)
  (mapc (lambda (name) (put name 'scheme-indent-function 1))      lambdas)
  (mapc (lambda (name) (put name 'scheme-indent-function 2))      specials))

;; (mapc (lambda (l)
;;         (let ((name (car l))
;;               (val  (cadr l)))
;;           (unless (get name 'scheme-indent-function)
;;             (put name 'scheme-indent-function val))))
;;
;;       '((test-group 1)
;;         (let/cc     1)
;;         (λg         1)
;;         (λf         1)
;;         (λ          defun)
;;         (run        1)
;;         (for/fold   1)
;;         (for/and    1)
;;         (for/or     1)
;;         (for/list   1)
;;         (for*/list  1)
;;         (run*       1)
;;         (run1       1)
;;         (fresh      1)
;;         (module+    1)                  ; again, no clue
;;         (module     1)))
