(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

(defadvice magit-visit-item (around always-other-window compile activate)
  (ad-set-arg 0 t)
  ad-do-it)

(define-key magit-mode-map "\M-w" nil)

(require 'json)

(defun yaml-to-json (path)
  (let* ((rb "ruby -ryaml -rjson -e 'puts JSON.dump YAML.load File.read ARGV.shift' ")
         (cmd (concat rb path)))
    (json-read-from-string (shell-command-to-string cmd))))

(defun get-hub-token ()
  (let* ((json (yaml-to-json "~/.config/hub"))
         (gh   (alist-get 'github.com json)))
    (alist-get 'oauth_token (aref gh 0))))

(get-hub-token)

(setq ghub-username "zenspider")
(setq ghub-token (get-hub-token)) ;; your personal access token
