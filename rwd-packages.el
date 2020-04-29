(require 'package)

;; TODO: need to keep elpa for delight. Want to remove that
;; (setq package-archives '())

;; TODO: consider ONLY using stable? do any packages disappear?
(dolist (repo '(("melpa"        . "http://melpa.org/packages/")
                ("melpa-stable" . "http://stable.melpa.org/packages/")))
  (add-to-list 'package-archives repo))

(package-initialize)

(unless (package-installed-p 'package+)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ; HACK
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
       '(package+                ; duh

         ag                      ; the silver surfer
         auto-dim-other-buffers  ; makes current buffer "pop"
         browse-kill-ring        ; M-y kill ring browsing
         dash                    ; better api for lists
         expand-region           ; mandatory
         f                       ; better api for files/dirs
         git-timemachine         ; fantastic spelunking tool
         github-browse-file      ; jump to code browser on github
         helpful                 ; better help system
         htmlize                 ; colorizing as html
         inf-ruby                ; trying to use more
         keyfreq                 ; stats on actual usage FIX?
         kill-ring-search        ; C-M-y kill ring search FIX
         magit                   ; the only reason I can use git
         multiple-cursors        ; mandatory
         outline-magic           ; outline-cycle FIX?
         p4                      ; mandatory
         paredit                 ; mandatory
         phi-search              ; TODO
         phi-search-mc           ; TODO
         popwin                  ; popup window manager FIX
         racket-mode             ; mandatory
         s                       ; better api for strings
         shell-command           ; tab-completion for `shell-command
         ;; ssh                  ; ssh-mode -- USING MY OWN FORK
         string-edit             ; AMAZING string-edit-at-point
         w3m                     ; strange web browser FIX
         window-number           ; Jump to window by M-number
         yagist                  ; gist
         yaml-mode               ; for yaml files.

         ;; trying out:

         aggressive-indent       ; apparently used in scheme-mode
         bison-mode              ; for yacc/bison files
         company                 ; a completer--use or FIX?
         cyphejor                ; mode renaming--FIX delight
         dedicated               ; "stick" buffers to windows
         delight                 ; mode renaming--FIX cyphejor
         default-text-scale      ; change font sizes globally
         dockerfile-mode         ; for Dockerfile files
         elisp-slime-nav         ; M-. M-, for elisp
         eval-in-repl            ; C-RET to send to repls
         find-file-in-project    ; ffip alternative to projectile FIX?
         flycheck                ; smoother than flyspell
         flycheck-color-mode-line; colors modeline based on status
         flycheck-package        ; flycheck elisp package files
         github-review           ; interesting workflow for reviews
         jq-mode                 ; live view of jq queries
         json-mode               ; for json files
         markdown-mode           ; for md files
         package-lint            ; for checking elisp packages
         shackle                 ; for me... ugh.
         phi-grep                ; TODO
         xr                      ; regexp -> xr (lispy regexps)
         web-mode                ; TODO
         wgrep                   ; editable grep results
         wgrep-ag                ; editable ag results
         which-key               ; show options in sub-keys
         window-purpose          ; trying to find a workflow that works
         xr                      ; regexp -> xr (lispy regexps)
         xterm-color             ; used in modes/shall-mode.el

         goto-chg
         copy-as-format
         ))

;; (package-refresh-contents)
;; (rwd-recompile-init)
;; (package-view-manifest)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
