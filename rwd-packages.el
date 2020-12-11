(require 'package)

(when (<= emacs-major-version 25)
  (setq package-check-signature nil))

(setq package-archives '())             ; remove elpa... fuck that thing

;; TODO: consider ONLY using stable? do any packages disappear?
(dolist (repo '(("melpa"        . "http://melpa.org/packages/")
                ("melpa-stable" . "http://stable.melpa.org/packages/")))
  (add-to-list 'package-archives repo))

(when (< emacs-major-version 27)
  (package-initialize))

(defun rwd/customize-save-variable/packages (oldfn variable value &optional comment)
  (if (eq variable 'package-selected-packages)
      (setq package-selected-packages value) ; prevent writing to custom.el
    (apply oldfn variable value comment)))

(advice-add 'customize-save-variable :around #'rwd/customize-save-variable/packages)

(unless (package-installed-p 'package+)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ; HACK
  (package-refresh-contents)
  (package-install 'package+))

(add-to-list 'load-path
             (expand-file-name "~/Work/git/zenspider/package+"))

(require 'package+)

(setq package-archive-priorities
      '(("melpa-stable" . -20)
        ("gnu"          .   0)
        ("melpa"        .  10)))

(apply 'package-manifest
       '(package+                ; duh

         auto-dim-other-buffers  ; makes current buffer "pop"
         browse-kill-ring        ; M-y kill ring browsing
         dash                    ; better api for lists
         dispwatch               ; auto-switching on monitor changes
         expand-region           ; mandatory
         f                       ; better api for files/dirs
         gcmh                    ; Garbage Collector Magic Hack
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
         perspective
         phi-search              ; TODO
         phi-search-mc           ; TODO
         racket-mode             ; mandatory
         rg                      ; ripgrep
         s                       ; better api for strings
         shell-command           ; tab-completion for `shell-command
         ;; ssh                  ; ssh-mode -- USING MY OWN FORK
         string-edit             ; AMAZING string-edit-at-point
         w3m                     ; strange web browser FIX
         window-number           ; Jump to window by M-number
         yagist                  ; gist
         yaml-mode               ; for yaml files.

         ;; trying out:

         magit-delta

         bison-mode              ; for yacc/bison files
         company                 ; a completer--use or FIX?
         cyphejor                ; mode renaming--FIX delight
         dedicated               ; "stick" buffers to windows
         ;; delight              ; now in third-party. TODO: phase out
         default-text-scale      ; change font sizes globally
         dockerfile-mode         ; for Dockerfile files
         elisp-slime-nav         ; M-. M-, for elisp
         eval-in-repl            ; C-RET to send to repls
         find-file-in-project    ; ffip alternative to projectile FIX?
         flycheck                ; smoother than flyspell
         flycheck-color-mode-line; colors modeline based on status
         ;; flycheck-package        ; flycheck elisp package files
         github-review           ; interesting workflow for reviews
         jq-mode                 ; live view of jq queries
         json-mode               ; for json files
         markdown-mode           ; for md files
         ;; package-lint            ; for checking elisp packages
         shackle                 ; for me... ugh.
         phi-grep                ; TODO
         web-mode                ; TODO
         wgrep                   ; editable grep results
         which-key               ; show options in sub-keys
         window-purpose          ; trying to find a workflow that works
         ;; xr                      ; regexp -> xr (lispy regexps)
         xterm-color             ; used in modes/shell-mode.el

         dumb-jump
         diff-at-point
         goto-chg
         copy-as-format
         magit-gh-pulls
         gist
         github-review
         ))

;; (package-refresh-contents)
;; (rwd-recompile-init)
;; (package-view-manifest)

(provide 'rwd-packages)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
