;;; stolen from https://github.com/sigma/el-x/blob/master/lisp/dflet.el

(when-idle rwd-idle-time
  (require 'cl-lib)

  (defmacro dflet (bindings &rest body)
    "Make temporary overriding function definitions.
This is an analogue of a dynamically scoped `let' that operates on the function
cell of FUNCs rather than their value cell.

\(fn ((FUNC ARGLIST BODY...) ...) FORM...)"
    (declare (indent 1) (debug cl-flet))
    `(cl-letf ,(mapcar
                (lambda (x)
                  (list
                   (list 'symbol-function (list 'quote (car x)))
                   (cons 'lambda (cons (cadr x) (cddr x)))))
                bindings)
       ,@body))

  ;; end of stolen code

  (require 'ffap)

  (defun rwd/ffap-like-emacs27 (orig-fun &rest args)
    ;; put simply, replace that last find-file with ffap-file-finder
    (dflet ((find-file (filename &optional wildcards)
                       (funcall ffap-file-finder (expand-file-name filename))))
      (apply orig-fun args)))

  (advice-add 'find-file-at-point :around #'rwd/ffap-like-emacs27))
