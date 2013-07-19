(require 'package)

(dolist (repo '(("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives repo))

(unless (fboundp 'package-cleanup)
  (require 'cl)

  (defun package-version-for (package)
    (package-desc-vers (cdr (assoc package package-alist))))

  (defun package-delete-by-name (package)
    (package-delete (symbol-name package)
                    (package-version-join (package-version-for package))))

  (defun package-maybe-install (name)
    (or (package-installed-p name)
        (progn
          (message "Installing %s" name)
          (package-install name))))

  (defun package-cleanup (packages)
    "Remove packages not explicitly declared"
    (let ((removes (set-difference (mapcar 'car package-alist) packages)))
      (mapc 'package-delete-by-name removes))))

(defun rwd-package-manifest (&rest packages)
  (package-initialize)

  (unless package-archive-contents      ; why do I need this? package-install has it
    (package-refresh-contents))

  (condition-case err
      (mapc 'package-maybe-install packages)
    (error (message "Couldn't install package: %s" err)))
  (package-cleanup packages))

(rwd-package-manifest 'ag
                      'expand-region
                      'htmlize
                      'keyfreq
                      'magit
                      'melpa
                      'multiple-cursors
                      'p4
                      'paredit
                      'popwin
                      'ruby-mode
                      'ssh
                      'window-number

                      ;; new

                      ;; shitty dependencies:

                      'cl-lib           ; required by magit, but not declared

                      ;; trying to decide:

                      'simp

                      'dash
                      's

                      ;; questionable:

                      ;; 'coffee-mode
                      ;; 'color-theme
                      ;; 'crontab-mode
                      ;; 'ess
                      ;; 'flycheck
                      ;; 'haml-mode
                      ;; 'inf-ruby
                      ;; 'json
                      ;; 'kill-ring-search
                      ;; 'levenshtein
                      ;; 'light-symbol
                      ;; 'logito
                      ;; 'lua-mode
                      ;; 'magithub
                      ;; 'markdown-mode
                      ;; 'pabbrev
                      ;; 'pcache
                      ;; 'prolog
                      ;; 'robe
                      ;; 'ruby-test-mode
                      ;; 'ruby-tools
                      ;; 'scala-mode
                      ;; 'shell-here
                      ;; 'shell-switcher
                      ;; 'smart-tab
                      ;; 'ssh-config-mode
                      ;; 'yagist
                      ;; 'yaml-mode
                      ;; 'yari
                      ;; 'yasnippet
                      )

;; (package-refresh-contents)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
