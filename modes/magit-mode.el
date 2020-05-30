(require 'transient)
(transient-bind-q-to-quit)

;; from http://www.ogre.com/node/447

(defun git-grep (search)
  "git-grep the entire current repo"
  (interactive
   (list (completing-read "Search for: " nil nil nil (current-word))))
  (grep-find (concat "git --no-pager grep -P -n "
                     search
                     " `git rev-parse --show-toplevel`")))

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

(require 'json)

(defun yaml-to-json (path)
  (let* ((rb "ruby -ryaml -rjson -e 'puts JSON.dump YAML.load File.read ARGV.shift' ")
         (cmd (concat rb path)))
    (condition-case nil
        (json-read-from-string (shell-command-to-string cmd))
      (json-error nil))))

(defun get-hub-token ()
  (let* ((json (yaml-to-json "~/.config/hub"))
         (gh   (and json (alist-get 'github.com json))))
    (and gh (alist-get 'oauth_token (aref gh 0)))))

(setq magit-last-seen-setup-instructions "1.4.0")

;; http://endlessparentheses.com/automatically-configure-magit-to-access-github-prs.html

(defcustom zenspider/magit-exclude-projects '()
  "A list of regexp patterns to exclude from the endless/add-PR-fetch hook."
  :group 'rwd
  :type '(repeat string))

(defun endless/add-PR-fetch ()
  "If refs/pull is not defined on a GH repo, define it."
  (require 'dash)                       ; ironic, magit uses this
  (let ((fetch-address
         "+refs/pull/*/head:refs/pull/origin/*")
        (magit-url     (magit-get "remote" "origin" "url"))
        (magit-remotes
         (magit-get-all "remote" "origin" "fetch")))
    (unless (or (not magit-remotes)
                (--any? (string-match-p it magit-url)
                        zenspider/magit-exclude-projects)
                (member fetch-address magit-remotes))
      (when (string-match
             "github" magit-url)
        (magit-git-string
         "config" "--add" "remote.origin.fetch"
         fetch-address)))))

;; https://endlessparentheses.com/easily-create-github-prs-from-magit.html
(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (let ((url (format "https://github.com/%s/pull/new/%s"
                     (replace-regexp-in-string
                      "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                      (magit-get "remote" (magit-get-push-remote) "url"))
                     (magit-get-current-branch))))
    (message "Visiting PR @ %s" url)
    (browse-url url)))

;;; https://magit.vc/manual/magit/Performance.html

;; (setq magit-refresh-status-buffer nil)

;; (setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p)

;; magit-refresh-buffer-hook
;; magit-status-refresh-hook
;; magit-pre-refresh-hook
;; magit-post-refresh-hook

;; magit-revision-insert-related-refs
;; magit-diff-highlight-indentation
;; magit-diff-highlight-trailing
;; magit-diff-paint-whitespace
;; magit-diff-highlight-hunk-body
;; magit-diff-refine-hunk

;; (remove-hook 'server-switch-hook 'magit-commit-diff)

;; (setq vc-handled-backends nil)
;; (setq vc-handled-backends (delq 'Git vc-handled-backends))

;; (require 'magithub)
;; (magithub-feature-autoinject t)
