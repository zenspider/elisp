
(define-key mediawiki-mode-map (kbd "C-<down>") 'rwd-forward-line-6)
(define-key mediawiki-mode-map (kbd "C-<up>")   'rwd-previous-line-6)
;; (define-key mediawiki-mode-map (kbd "M-<down>") nil)
;; (define-key mediawiki-mode-map (kbd "M-<up>") nil)
(outline-minor-mode -1)
(local-set-key [(control up)] nil)
(local-set-key [(control down)] nil)
