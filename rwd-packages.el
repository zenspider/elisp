(require 'package)

(dolist (repo '(("melpa" . "http://melpa.milkbox.net/packages/")
                ("melpa-stable" . "http://stable.melpa.org/packages/")))
  (add-to-list 'package-archives repo))

(package-initialize)

(unless (package-installed-p 'package+)
  (package-refresh-contents)
  (package-install 'package+))

(require 'package+)

(package-manifest 'package+

                  'ag
                  'browse-kill-ring
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
                  'multiple-cursors
                  'outline-magic
                  'p4
                  'paredit
                  'phi-search
                  'phi-search-mc
                  'popwin
                  'racket-mode
                  'ruby-mode
                  's
                  'sml-mode
                  'ssh
                  'w3m
                  'window-number
                  'yagist
                  'shell-command

                  ;; trying out:

                  'elisp-slime-nav

                  'outline-magic
                  'wgrep
                  'wgrep-ag

                  'find-file-in-project
                  'swift-mode

                  'perspective
                  'smartrep
                  'bind-key
                  'aggressive-indent

                  'phi-grep

                  'company
                  'grizzl
                  'names

                  'bison-mode

                  'shrink-whitespace

                  'markdown-mode
                  )

;; (package-refresh-contents)
;; (rwd-recompile-init)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
