(eval-when-compile
  (require 'json)
  (require 'magit)
  (require 'magit-diff))

(with-eval-after-load 'transient
  (message "TRANSIENT!")
  (transient-bind-q-to-quit))

;; M1 macs install into /opt/homebrew/bin:
;; intel macs install into /usr/local/bin
(with-eval-after-load 'magit
  (unless (file-executable-p magit-git-executable)
    (let* ((paths '("/opt/homebrew/bin/git" "/usr/local/bin/git"))
           (paths (mapcar   #'expand-file-name paths))
           (found (seq-find #'file-exists-p    paths)))
      (if found
          (setq magit-git-executable found)
        (message "git executable not found!")))))

(with-eval-after-load 'with-editor
  (unless (file-executable-p with-editor-emacsclient-executable)
    (setq with-editor-emacsclient-executable "/opt/homebrew/bin/emacsclient")))

;; from http://www.ogre.com/node/447

(defun git-grep (search)
  "git-grep the entire current repo"
  (interactive
   (list (completing-read "Search for: " nil nil nil (current-word))))
  (grep-find (concat "git --no-pager grep -P -n "
                     search
                     " `git rev-parse --show-toplevel`")))

(with-eval-after-load 'magit
  (let ((maps (list magit-file-section-map magit-hunk-section-map)))
    (--each maps
      (define-key it (kbd "RET") 'magit-diff-visit-worktree-file-other-window))))

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

(defun magit-master/main ()
  (magit-name-remote-branch "origin/HEAD"))

(defun rwd/magit-diff-file-master ()
  (interactive)
  (magit-diff-range (format "%s..." (magit-master/main))
                    (quote ("--no-ext-diff" "--stat"))
                    (list (buffer-file-name))))

(defun rwd/magit-diff-branch-master ()
  (interactive)
  (magit-diff-range (format "%s..." (magit-master/main))
                    (quote ("--no-ext-diff" "--stat"))))

(defun rwd/magit-log-main (&optional args files)
  "Show log for `main...'."
  (interactive (magit-log-arguments))
  (magit-log-setup-buffer (list (format "%s..." (magit-master/main)))
                          args files))

(defun rwd/magit-bisect-all ()
  (interactive)
  (magit-bisect-start "HEAD" (magit-master/main) '()))

(defun rwd/magit-show-refs-head-sorted (&optional args)
  "List and compare references in a dedicated buffer, but sorted by -commiterdate
Compared with `HEAD'."
  (interactive (list (magit-show-refs-arguments)))
  (magit-refs-setup-buffer "HEAD" (cons "--sort=-committerdate" args)))

;; https://with-emacs.com/posts/tutorials/almost-all-you-need-to-know-about-variables/

(defun rwd/toggle-custom-variable (symbol)
  (if (custom-variable-p symbol)
      (let ((current-value  (default-value symbol))
            (saved-value    (eval (car (get symbol 'saved-value)))))
        (if (equal current-value saved-value)
            (let ((set            (or (get symbol 'custom-set) 'set-default))
                  (standard-value (get symbol 'standard-value)))
              (funcall set symbol (eval (car standard-value))))
          (custom-reevaluate-setting symbol)))))

(defun rwd/magit/toggle-refs-filter ()
  "Toggle filtering regexp for magit-refs-filter-alist"
  (interactive)
  (rwd/toggle-custom-variable 'magit-refs-filter-alist))

;; (setq vc-git-annotate-switches '("--no-show-name"))

;; (defun rwd/magit-diff-visit-file/toggle-other (orig-fn file other-window)
;;   (apply orig-fn (list file (not other-window))))
;;
;; (advice-add 'magit-diff-visit-file :around #'rwd/magit-diff-visit-file/toggle-other)
;; ;; (advice-remove 'magit-diff-visit-file 'rwd/magit-diff-visit-file/toggle-other)

;;; https://magit.vc/manual/magit/Performance.html

;; (setq magit-refresh-status-buffer nil)

;; (magit-toggle-verbose-refresh)

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
