(require 'package)
(add-to-list 'load-path (expand-file-name "~/Work/git/package+") t)
(require 'package+)

(dolist (repo '(("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives repo))

(package-manifest 'ag
                  'dash
                  'elscreen
                  'expand-region
                  'f
                  'geiser
                  'git-timemachine
                  'github-browse-file
                  'htmlize
                  'keyfreq
                  'kill-ring-search
                  'litable
                  'magit
                  'magit-log-edit
                  'multiple-cursors
                  'p4
                  'paredit
                  'phi-search
                  'phi-search-mc
                  'popwin
                  'racket-mode
                  'ruby-mode
                  's
                  'ssh
                  'w3m
                  'window-number
                  'yagist

                  ;; trying out:

                  'sml-mode

                  'outline-magic
                  'wgrep
                  'wgrep-ag

                  'find-file-in-project
                  'swift-mode

                  'browse-kill-ring

                  'perspective
                  'smartrep
                  'bind-key
                  'aggressive-indent
                  )

;; (package-refresh-contents)
;; (rwd-recompile-init)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
