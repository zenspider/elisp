(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
(define-key magit-mode-map        (kbd "M-w") nil)
(define-key magit-mode-map        (kbd "V") #'endless/visit-pull-request-url)

;; TODO: "hub pull-request --no-edit -o"
;; TODO: "git absorb"

(endless/add-PR-fetch)
;; (turn-on-magit-gh-pulls) ; broken on current versions of magit

(defadvice magit-visit-item (around always-other-window compile activate)
  (ad-set-arg 0 t)
  ad-do-it)

(setq ghub-username "zenspider")
(setq ghub-token (get-hub-token)) ;; your personal access token
