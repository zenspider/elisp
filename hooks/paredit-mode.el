;; paredit-forward-slurp-sexp - also on C-), which is what I use
(define-key paredit-mode-map (kbd "C-<right>") nil)

;; paredit-forward-barf-sexp - also on C-}, which I apparently never use
(define-key paredit-mode-map (kbd "C-<left>") nil)
(define-key paredit-mode-map (kbd "C-M-)") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "{") 'paredit-open-curly)
(define-key paredit-mode-map (kbd "}") 'paredit-close-curly)
;; normally M-s -- no, you don't get that
(define-key paredit-mode-map (kbd "C-c j") 'paredit-splice-sexp)
(define-key paredit-mode-map (kbd "M-s") 'fixup-whitespace)

;; (global-set-key (kbd "C-M-u") #'paredit-backward-up)
;; (global-set-key (kbd "C-M-n") #'paredit-forward-up)
;; ;; This one's surpisingly useful for writing prose.
;; (global-set-key "\M-S" #'paredit-splice-sexp-killing-backward)
;; (global-set-key "\M-R" #'paredit-raise-sexp)
;; (global-set-key "\M-(" #'paredit-wrap-round)
;; (global-set-key "\M-[" #'paredit-wrap-square)
;; (global-set-key "\M-{" #'paredit-wrap-curly)
