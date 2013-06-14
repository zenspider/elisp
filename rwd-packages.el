(require 'package)

(dolist (repo '(("elpa"      . "http://tromey.com/elpa/")
                ("marmalade" . "http://marmalade-repo.org/packages/")
                ("melpa"     . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives repo))

(unless (fboundp 'package-cleanup)
  (require 'cl)

  (defun package-version-for (package)
    (package-desc-vers (cdr (assoc package package-alist))))

  (defun package-delete-by-name (package)
    (package-delete (symbol-name package)
                    (package-version-join (package-version-for package))))

  (defun package-maybe-install (name)
    (or (package-installed-p name) (package-install name)))

  (defun package-cleanup (packages)
    "Remove packages not explicitly declared"
    (let ((removes (set-difference (mapcar 'car package-alist) packages)))
      (mapc 'package-delete-by-name removes))))

(defun rwd-package-manifest (&rest packages)
  (package-initialize)
  (mapc 'package-maybe-install packages)
  (package-cleanup packages))

(rwd-package-manifest 'ag
                      'expand-region
                      'magit
                      'mark-more-like-this
                      'mark-multiple
                      'p4
                      'paredit
                      'ruby-mode
                      'ssh
                      'window-number

                      ;; experimenting
                      'goto-last-change
                      'smex
                      'fuel

                      ;; questionable
                      'cl-lib
                      'coffee-mode
                      'color-theme
                      'crontab-mode
                      'dash
                      'eproject
                      'ess
                      'find-file-in-git-repo
                      'find-file-in-project
                      'find-things-fast
                      'findr
                      'flycheck
                      'gh
                      'haml-mode
                      'htmlize
                      'inf-ruby
                      'json
                      'keyfreq
                      'kill-ring-search
                      'levenshtein
                      'light-symbol
                      'logito
                      'lua-mode
                      'magit-gh-pulls
                      'magithub
                      'markdown-mode
                      'melpa
                      'multiple-cursors
                      'pabbrev
                      'pcache
                      'popwin
                      'project
                      'project-local-variables
                      'project-mode
                      'prolog
                      'robe
                      'ruby-test-mode
                      'ruby-tools
                      's
                      'scala-mode
                      'shell-here
                      'shell-switcher
                      'smart-tab
                      'ssh-config-mode
                      'yagist
                      'yaml-mode
                      'yari
                      'yasnippet
                      )

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
