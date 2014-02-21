(require 'package)
(add-to-list 'load-path (expand-file-name "~/Work/git/package+") t)
(require 'package+)

(dolist (repo '(("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives repo))

(package-manifest 'ag
                  'expand-region
                  'htmlize
                  'keyfreq
                  'kill-ring-search
                  'litable
                  'magit
                  'magit-log-edit
                  'melpa
                  'multiple-cursors
                  'p4
                  'paredit
                  'popwin
                  'ruby-mode
                  'ssh
                  'w3m
                  'window-number

                  ;; new

                  'yagist
                  'geiser
                  'racket-mode
                  ;; 'quack ; not in packages?!?

                  ;; trying to decide:

                  'simp
                  'dash
                  's
                  'f
                  'phi-search
                  'phi-search-mc
                  'github-browse-file

                  ;; questionable:

                  ;; 'coffee-mode
                  ;; 'color-theme
                  ;; 'crontab-mode
                  ;; 'ess
                  ;; 'flycheck
                  ;; 'haml-mode
                  ;; 'inf-ruby
                  ;; 'json
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
                  ;; 'yaml-mode
                  ;; 'yari
                  ;; 'yasnippet
                  )

;; (package-refresh-contents)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
