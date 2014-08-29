(require 'package)
(add-to-list 'load-path (expand-file-name "~/Work/git/package+") t)
(require 'package+)

(dolist (repo '(("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives repo))

(package-manifest 'ag
                  'dash
                  'elscreen
                  ;; 'ess ; only when needed
                  'expand-region
                  'f
                  'geiser
                  'github-browse-file
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

                  'git-timemachine
                  )

;; (package-refresh-contents)
;; (rwd-recompile-init)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
