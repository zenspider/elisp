(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
(define-key magit-mode-map        (kbd "M-w") nil)

(endless/add-PR-fetch)

(defadvice magit-visit-item (around always-other-window compile activate)
  (ad-set-arg 0 t)
  ad-do-it)

(setq ghub-username "zenspider")
(setq ghub-token (get-hub-token)) ;; your personal access token
