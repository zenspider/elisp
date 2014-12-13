;; paredit-forward-slurp-sexp - also on C-), which is what I use
(define-key paredit-mode-map (kbd "C-<right>") nil)

;; paredit-forward-barf-sexp - also on C-}, which I apparently never use
(define-key paredit-mode-map (kbd "C-<left>") nil)
(define-key paredit-mode-map (kbd "C-M-)") 'paredit-forward-barf-sexp)

;; normally M-s -- no, you don't get that
(define-key paredit-mode-map (kbd "C-c j") 'paredit-splice-sexp)
(define-key paredit-mode-map (kbd "M-s") 'fixup-whitespace)
