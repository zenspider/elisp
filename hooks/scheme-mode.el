(require 'paredit)
(require 'cmuscheme)
;; (require 'scheme-complete)
;; (require 'cluck)

(paredit-mode +1)
;; (define-key scheme-mode-map "\t" 'scheme-complete-or-indent)

(blank-mode)

(make-local-variable 'eldoc-documentation-function)
(setq eldoc-documentation-function 'scheme-get-current-symbol-info)
(eldoc-mode)

(mapc (lambda (l)
        (let ((name (car l))
              (val  (cadr l)))
          (unless (get name 'scheme-indent-function)
            (put name 'scheme-indent-function val))))

      '((test-group 1)
        (let/cc     1)
        (λg         1)
        (λf         1)
        (λ          1)
        (run        1)
        (run*       1)
        (run1       1)
        (fresh      1)
        (module     0)))

;; (symbol-plist 'λ) ; check with this

(setq font-lock-keywords-case-fold-search nil)

(defun scheme-send-file ()
  (interactive)
  (scheme-send-region (point-min) (point-max)))

(define-key scheme-mode-map "\C-c\C-f" 'scheme-send-file)

;; (eval-when-compile
;;   (add-to-list 'load-path "~/Work/cvs/slime/")
;;   (require 'slime))
;; (setq slime-csi-path "/usr/local/bin/csi")
;; (setq swank-chicken-path nil)
;; (add-to-list 'load-path "~/Work/cvs/slime/")
;; (add-to-list 'load-path "~/Work/git/swank-chicken/")
;; (require 'slime)
;; (slime-setup '(slime-fancy slime-banner slime-autodoc))
;; (autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)
;; (slime-mode t)
