;; -*- coding: utf-8 -*-

;; (autoload 'run-scheme                     "cmuscheme"        nil t)
;; (autoload 'scheme-smart-complete          "scheme-complete"  nil t)
;; (autoload 'scheme-complete-or-indent      "scheme-complete"  nil t)
;; (autoload 'scheme-get-current-symbol-info "scheme-complete"  nil t)

;; (let ((defines '(struct local struct: define-struct:
;;                         define-typed-struct define-struct/exec:
;;                         define: pdefine: define-type
;;                         define-predicate match-define))
;;       (begins '(case-lambda case-lambda: pcase-lambda:
;;                             match-lambda match-lambda* cond
;;                             delay unit compound-unit
;;                             compound-unit/sig public private
;;                             override inherit sequence))
;;       (lambdas `(cases instantiate super-instantiate syntax/loc
;;                        quasisyntax/loc match match* match-let
;;                        match-let* match-letrec λ lambda let let*
;;                        letrec recur lambda/kw letrec-values
;;                        with-syntax with-continuation-mark module
;;                        module* module+ match match-let
;;                        match-let* match-letrec let/cc let/ec
;;                        letcc catch let-syntax letrec-syntax
;;                        fluid-let-syntax letrec-syntaxes+values
;;                        let: letrec: let*: let-values:
;;                        letrec-values: let*-values: let/cc:
;;                        let/ec: lambda: λ: plambda: opt-lambda:
;;                        popt-lambda: splicing-let splicing-letrec
;;                        splicing-let-values
;;                        splicing-letrec-values
;;                        splicing-let-syntax
;;                        splicing-letrec-syntax
;;                        splicing-let-syntaxes
;;                        splicing-letrec-syntaxes
;;                        splicing-letrec-syntaxes+values
;;                        splicing-local
;;                        splicing-syntax-parameterize for for/list
;;                        for/hash for/hasheq for/hasheqv
;;                        for/and for/or for/lists for/first
;;                        for/last for/vector for/flvector
;;                        for/sum for/product for/set for* for*/list
;;                        for*/hash for*/hasheq for*/hasheqv
;;                        for*/and for*/or for*/lists for*/first
;;                        for*/last for*/fold for*/vector
;;                        for*/flvector for*/sum for*/product
;;                        for*/set for: for/list: for/hash:
;;                        for/hasheq: for/hasheqv: for/and: for/or:
;;                        for/lists: for/first: for/last:
;;                        for/vector: for/flvector: for/sum:
;;                        for/product: for/set: for*: for*/list:
;;                        for*/hash: for*/hasheq: for*/hasheqv:
;;                        for*/and: for*/or: for*/lists: for*/first:
;;                        for*/last: for*/fold: for*/vector:
;;                        for*/flvector: for*/sum: for*/product:
;;                        for*/set: do: kernel-syntax-case syntax-case
;;                        syntax-case* syntax-rules syntax-id-rules
;;                        let-signature fluid-let let-struct
;;                        let-macro let-values let*-values case
;;                        when unless let-enumerate class class*
;;                        class-asi class-asi* class*/names
;;                        class100 class100* class100-asi
;;                        class100-asi* class100*/names rec
;;                        make-object mixin define-some do
;;                        opt-lambda send* with-method
;;                        define-record catch shared unit/sig
;;                        unit/lang with-handlers interface
;;                        parameterize parameterize*
;;                        call-with-input-file
;;                        call-with-input-file*
;;                        with-input-from-file with-input-from-port
;;                        call-with-output-file with-output-to-file
;;                        with-output-to-port for-all type-case))
;;       (specials '(for/fold for/fold:)))
;;
;;   (mapc (lambda (name) (put name 'scheme-indent-function 'defun)) defines)
;;   (mapc (lambda (name) (put name 'scheme-indent-function 0))      begins)
;;   (mapc (lambda (name) (put name 'scheme-indent-function 1))      lambdas)
;;   (mapc (lambda (name) (put name 'scheme-indent-function 2))      specials))

;; need to audit against these:

;; (put 'class                  'scheme-indent-function 'defun)
;; (put 'class*                 'scheme-indent-function 'defun)
;; (put 'module                 'scheme-indent-function 'defun)
;; (put 'unit                   'scheme-indent-function 'defun)

;; (put 'let*:                  'scheme-indent-function 'quack-let-colon-indent)
;; (put 'let:                   'scheme-indent-function 'quack-let-colon-indent)

;; (put 'c-declare              'scheme-indent-function 0)
;; (put 'case-lambda            'scheme-indent-function 0)
;; (put 'compound-unit/sig      'scheme-indent-function 0)
;; (put 'dynamic-wind           'scheme-indent-function 0)

;; (put 'begin0                 'scheme-indent-function 1)
;; (put 'call-with-input-file   'scheme-indent-function 1)
;; (put 'call-with-input-file*  'scheme-indent-function 1)
;; (put 'call-with-output-file  'scheme-indent-function 1)
;; (put 'call-with-output-file* 'scheme-indent-function 1)
;; (put 'call-with-semaphore    'scheme-indent-function 1)
;; (put 'catch                  'scheme-indent-function 1)
;; (put 'chicken-setup          'scheme-indent-function 1)
;; (put 'defform                'scheme-indent-function 1)
;; (put 'defform*               'scheme-indent-function 1)
;; (put 'defform/none           'scheme-indent-function 1)
;; (put 'defidform              'scheme-indent-function 1)
;; (put 'define-runtime-path    'scheme-indent-function 1)
;; (put 'define-sequence-id     'scheme-indent-function 1)
;; (put 'defproc*               'scheme-indent-function 1)
;; (put 'deftogether            'scheme-indent-function 1)
;; (put 'filebox                'scheme-indent-function 1)
;; (put 'for                    'scheme-indent-function 1)
;; (put 'for*                   'scheme-indent-function 1)
;; (put 'for*/and               'scheme-indent-function 1)
;; (put 'for*/first             'scheme-indent-function 1)
;; (put 'for*/hash              'scheme-indent-function 1)
;; (put 'for*/hasheq            'scheme-indent-function 1)
;; (put 'for*/hasheqv           'scheme-indent-function 1)
;; (put 'for*/last              'scheme-indent-function 1)
;; (put 'for*/list              'scheme-indent-function 1)
;; (put 'for*/or                'scheme-indent-function 1)
;; (put 'for*/product           'scheme-indent-function 1)
;; (put 'for*/sum               'scheme-indent-function 1)
;; (put 'for*/vector            'scheme-indent-function 1)
;; (put 'for*/vector            'scheme-indent-function 1)
;; (put 'for/and                'scheme-indent-function 1)
;; (put 'for/first              'scheme-indent-function 1)
;; (put 'for/hash               'scheme-indent-function 1)
;; (put 'for/hasheq             'scheme-indent-function 1)
;; (put 'for/hasheqv            'scheme-indent-function 1)
;; (put 'for/last               'scheme-indent-function 1)
;; (put 'for/list               'scheme-indent-function 1)
;; (put 'for/or                 'scheme-indent-function 1)
;; (put 'for/product            'scheme-indent-function 1)
;; (put 'for/sum                'scheme-indent-function 1)
;; (put 'for/vector             'scheme-indent-function 1)
;; (put 'interface              'scheme-indent-function 1)
;; (put 'lambda/kw              'scheme-indent-function 1)
;; (put 'let*-values            'scheme-indent-function 1)
;; (put 'let+                   'scheme-indent-function 1)
;; (put 'let-values             'scheme-indent-function 1)
;; (put 'let/ec                 'scheme-indent-function 1)
;; (put 'letrec-values          'scheme-indent-function 1)
;; (put 'match                  'scheme-indent-function 1)
;; (put 'match-let              'scheme-indent-function 1)
;; (put 'module+                'scheme-indent-function 1)
;; (put 'opt-lambda             'scheme-indent-function 1)
;; (put 'parameterize           'scheme-indent-function 1)
;; (put 'parameterize*          'scheme-indent-function 1)
;; (put 'parameterize-break     'scheme-indent-function 1)
;; (put 'quasisyntax/loc        'scheme-indent-function 1)
;; (put 'send*                  'scheme-indent-function 1)
;; (put 'sigaction              'scheme-indent-function 1)
;; (put 'specform               'scheme-indent-function 1)
;; (put 'specspecsubform        'scheme-indent-function 1)
;; (put 'specsubform            'scheme-indent-function 1)
;; (put 'struct                 'scheme-indent-function 1)
;; (put 'sxml-match             'scheme-indent-function 1)
;; (put 'syntax-parse           'scheme-indent-function 1)
;; (put 'syntax/loc             'scheme-indent-function 1)
;; (put 'test-section           'scheme-indent-function 1)
;; (put 'unless                 'scheme-indent-function 1)
;; (put 'when                   'scheme-indent-function 1)
;; (put 'while                  'scheme-indent-function 1)
;; (put 'with-handlers          'scheme-indent-function 1)
;; (put 'with-handlers*         'scheme-indent-function 1)
;; (put 'with-method            'scheme-indent-function 1)
;; (put 'with-syntax            'scheme-indent-function 1)

;; (put 'c-lambda               'scheme-indent-function 2)
;; (put 'defboolparam           'scheme-indent-function 2)
;; (put 'defform*/subs          'scheme-indent-function 2)
;; (put 'defform/subs           'scheme-indent-function 2)
;; (put 'defproc                'scheme-indent-function 2)
;; (put 'defstruct              'scheme-indent-function 2)
;; (put 'defstruct*             'scheme-indent-function 2)
;; (put 'defthing               'scheme-indent-function 2)
;; (put 'do                     'scheme-indent-function 2)
;; (put 'for*/fold              'scheme-indent-function 2)
;; (put 'for*/lists             'scheme-indent-function 2)
;; (put 'for/fold               'scheme-indent-function 2)
;; (put 'for/fold               'scheme-indent-function 2)
;; (put 'for/lists              'scheme-indent-function 2)
;; (put 'instantiate            'scheme-indent-function 2)
;; (put 'mixin                  'scheme-indent-function 2)
;; (put 'module                 'scheme-indent-function 2)
;; (put 'module*                'scheme-indent-function 2)
;; (put 'receive                'scheme-indent-function 2)
;; (put 'specspecsubform/subs   'scheme-indent-function 2)
;; (put 'specsubform/subs       'scheme-indent-function 2)
;; (put 'syntax-case            'scheme-indent-function 2)
;; (put 'unit/sig               'scheme-indent-function 2)
;; (put 'define:                'scheme-indent-function 3)
;; (put 'defparam               'scheme-indent-function 3)
;; (put 'for*/fold/derived      'scheme-indent-function 3)
;; (put 'for/fold/derived       'scheme-indent-function 3)
