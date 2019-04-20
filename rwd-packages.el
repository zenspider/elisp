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

(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("gnu"          . 10)
        ("melpa"        .  0)))

(apply 'package-manifest
       (setq rwd-manifest '(package+    ; duh

                            ag          ; the silver surfer
                            browse-kill-ring
                            dash
                            expand-region
                            f
                            geiser
                            git-timemachine
                            github-browse-file
                            htmlize
                            keyfreq
                            kill-ring-search
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
                            ssh                           ; ssh-mode
                            string-edit                   ; AMAZING string-edit-at-point
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
                            flycheck-swift3
                            flycheck-swiftlint
                            flycheck-package
                            package-lint

                            perspective ; buffers-per-perspective
                            cyphejor    ; mode renaming
                            delight     ; ditto--I might have to merge these two

                            aggressive-indent

                            phi-grep

                            bison-mode

                            markdown-mode
                            web-mode
                            smart-jump
                            inf-ruby

                            bts
                            bts-github
                            magit-gh-pulls
                            let-alist

                            ;; magithub
                            magit-p4

                            dockerfile-mode

                            flycheck
                            company

                            json-mode
                            jq-mode

                            eval-in-repl)))

;; removes anything I hand-install to test out.
(add-hook 'after-init-hook
          (lambda ()
            (unless (equal package-selected-packages rwd-manifest)
              (customize-save-variable 'package-selected-packages rwd-manifest))))

;; (with-help-window "my-packages" (with-current-buffer "my-packages" (cl-prettyprint (package-manifest-with-deps rwd-manifest))))

;; (package-refresh-contents)
;; (rwd-recompile-init)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
