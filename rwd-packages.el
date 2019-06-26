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
       (setq rwd-manifest '(package+           ; duh

                            ag                 ; the silver surfer
                            browse-kill-ring
                            dash
                            expand-region      ; mandatory
                            f
                            geiser
                            git-timemachine    ; fantastic spelunking tool
                            github-browse-file ; jump to code browser on github
                            htmlize
                            keyfreq
                            kill-ring-search
                            magit              ; the only reason I can use git
                            multiple-cursors   ; mandatory
                            org
                            outline-magic
                            p4                 ; mandatory
                            paredit            ; mandatory
                            phi-search
                            phi-search-mc
                            popwin
                            racket-mode        ; mandatory
                            s
                            shell-command      ; tab-completion for `shell-command
                            ssh                ; ssh-mode
                            string-edit        ; AMAZING string-edit-at-point
                            w3m
                            window-number      ; Jump to window by M-number
                            yagist             ; gist
                            yaml-mode

                            ;; trying out:

                            aggressive-indent
                            bison-mode
                            company
                            cyphejor           ; mode renaming--FIX delight
                            delight            ; mode renaming--FIX cyphejor
                            default-text-scale
                            dockerfile-mode
                            elisp-slime-nav
                            eval-in-repl
                            find-file-in-project
                            flycheck           ; smoother than flyspell
                            flycheck-package
                            flycheck-swift3
                            flycheck-swiftlint
                            inf-ruby           ; trying to use more
                            jq-mode
                            json-mode
                            let-alist
                            magit-p4
                            markdown-mode
                            outline-magic
                            package-lint
                            ;; perspective     ; buffers-per-perspective
                            phi-grep
                            projectile
                            xr                 ; regexp -> xr (lispy regexps)
                            ;; smart-jump      ; seemed troublesome. not smart?
                            swift-mode
                            web-mode
                            wgrep
                            wgrep-ag
                            which-key
                            xterm-color        ; used in modes/shall-mode.el
                            )))

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
