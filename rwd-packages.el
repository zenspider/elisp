(eval-when-compile
  (require 'gnutls))

(require 'package)

(setq package-archives '())             ; remove elpa... fuck that thing

(dolist (repo '(("melpa"        . "http://melpa.org/packages/")
                ("gnu"          . "https://elpa.gnu.org/packages/")
                ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                ))
  (add-to-list 'package-archives repo))

(unless (package-installed-p 'package+)
  (when (<= emacs-major-version 25)
    (setq package-check-signature nil))
  (when (< emacs-major-version 27)
    (package-initialize)                ; is this needed?
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")) ; HACK
  (package-refresh-contents)
  (package-install 'package+))

(add-to-list 'load-path
             (expand-file-name "~/Work/git/zenspider/package+"))

(require 'package+)

(setq package-archive-priorities
      '(("melpa-stable" . -20)
        ("gnu"          .   0)
        ("nongnu"       .   5)
        ("melpa"        .  10)))

(apply 'package-manifest
       '(package+                ; duh

         ansible-doc             ; basic ansible documentation lookup
         auto-dim-other-buffers  ; makes current buffer "pop"
         browse-at-remote        ; like github-browse-file
         browse-kill-ring        ; M-y kill ring browsing
         coterm                  ; Terminal emulation for comint
         cyphejor                ; mode renaming. TODO: nuke
         dash                    ; better api for lists
         dedicated               ; "stick" buffers to windows
         delight                 ; mode renaming.
         dispwatch               ; auto-switching on monitor changes
         expand-region           ; mandatory
         f                       ; better api for files/dirs
         gcmh                    ; Garbage Collector Magic Hack
         gist                    ; post to gist
         git-timemachine         ; fantastic spelunking tool
         helpful                 ; better help system
         htmlize                 ; colorizing as html
         find-file-in-project    ; ffip alternative to projectile FIX?
         inf-ruby                ; trying to use more
         keyfreq                 ; stats on actual usage FIX?
         kill-ring-search        ; C-M-y kill ring search FIX
         lua-mode
         magit                   ; the only reason I can use git
         mode-line-bell          ; visual bell
         multiple-cursors        ; mandatory
         outline-magic           ; outline-cycle FIX?
         p4                      ; mandatory
         paredit                 ; mandatory
         perspective
         phi-search              ; TODO
         phi-search-mc           ; TODO
         racket-mode
         rg                      ; ripgrep
         s                       ; better api for strings
         shell-command           ; tab-completion for `shell-command
         ;; ssh                  ; ssh-mode -- USING MY OWN FORK
         string-edit-at-point    ; AMAZING string-edit-at-point
         w3m                     ; strange web browser FIX
         window-number           ; Jump to window by M-number
         yaml-mode
         zig-mode

         ;; trying out:          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

         mct                     ; minimal completing-read enhancement

         copy-as-format
         default-text-scale      ; change font sizes globally
         diff-at-point
         dockerfile-mode         ; for Dockerfile files
         dumber-jump             ; very fast dumb definition search
         elisp-slime-nav         ; M-. M-, for elisp
         eval-in-repl            ; C-RET to send to repls
         flycheck                ; smoother than flyspell
         flycheck-color-mode-line; colors modeline based on status
         ;; flycheck-package        ; flycheck elisp package files
         github-review           ; interesting workflow for reviews
         goto-chg
         magit-delta
         magit-gh-pulls
         ;; package-lint            ; for checking elisp packages
         phi-grep                ; TODO
         ;; smartrep                ; better repeatable key sequencing -- forked
         wgrep                   ; editable grep results
         which-key               ; show options in sub-keys
         xterm-color             ; used in modes/shell-mode.el
         ;; xr                      ; regexp -> xr (lispy regexps)

         ;; benchmark-init       ; for measuring and speeding up boot time

         ;;; file formats
         bison-mode              ; for yacc/bison files
         jq-mode                 ; live view of jq queries
         json-mode               ; for json files
         markdown-mode           ; for md files
         web-mode                ; for web template files (do I use this?)

         winnow                  ; for refining ag results

         orderless               ; experimenting
         ))

(when (fboundp 'package-quickstart-refresh)
    (package-quickstart-refresh))

;; (package-refresh-contents)
;; (package-quickstart-refresh)
;; (rwd-recompile-init)
;; (package-view-manifest)

(provide 'rwd-packages)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
