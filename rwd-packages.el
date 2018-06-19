(require 'package)

;; TODO: consider ONLY using stable? do any packages disappear?
(dolist (repo '(("melpa"        . "http://melpa.org/packages/")
                ("melpa-stable" . "http://stable.melpa.org/packages/")))
  (add-to-list 'package-archives repo))

(package-initialize)

(unless (package-installed-p 'package+)
  (package-refresh-contents)
  (package-install 'package+))

(add-to-list 'load-path
             (expand-file-name "~/Work/git/zenspider/package+"))

(require 'package+)

(apply 'package-manifest
       (setq rwd-manifest '(package+    ; duh

                            ag          ; the silver surfer
                            browse-kill-ring
                            dash
                            elscreen
                            expand-region
                            f
                            geiser
                            git-timemachine
                            github-browse-file
                            htmlize
                            keyfreq
                            kill-ring-search
                            litable
                            magit
                            multiple-cursors
                            org
                            outline-magic
                            p4
                            paredit
                            phi-search
                            phi-search-mc
                            popwin
                            racket-mode
                            s
                            shell-command                 ; tab-completion for `shell-command
                            sml-mode
                            ssh                           ; ssh-mode
                            w3m
                            window-number                 ; Jump to window by M-number
                            yagist                        ; gist
                            yaml-mode

                            ;; trying out:

                            elisp-slime-nav

                            outline-magic
                            wgrep
                            wgrep-ag

                            find-file-in-project
                            swift-mode

                            perspective
                            aggressive-indent

                            phi-grep

                            names

                            bison-mode

                            markdown-mode
                            web-mode
                            smart-jump
                            inf-ruby

                            bts
                            bts-github
                            magit-gh-pulls
                            let-alist

                            golden-ratio
                            ;; magithub
                            magit-p4

                            dockerfile-mode

                            ;; haskell stuff
                            haskell-mode
                            flycheck
                            shm
                            hindent
                            dante
                            company

                            string-edit

                            json-mode
                            jq-mode

                            eval-in-repl)))

;; (with-current-buffer "*scratch*" (cl-prettyprint (package-manifest-with-deps rwd-manifest)) (display-buffer "*scratch*"))

;; (package-refresh-contents)
;; (rwd-recompile-init)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
