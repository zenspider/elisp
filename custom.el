(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list
   (quote
    ("/usr/share/info" "~/Bin/elisp/info" "/Developer/usr/share/info")))
 '(apropos-do-all t)
 '(auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
 '(auto-dim-other-buffers-mode t)
 '(auto-revert-use-notify nil)
 '(backup-by-copying-when-linked t)
 '(blink-cursor-mode nil)
 '(browse-kill-ring-display-duplicates nil)
 '(bs-attributes-list
   (quote
    (("" 1 1 left bs--get-marked-string)
     ("M" 1 1 left bs--get-modified-string)
     ("R" 2 2 left bs--get-readonly-string)
     ("Buffer" bs--get-name-length 10 left bs--get-name)
     ("" 1 1 left " ")
     ("Mode" 4 8 right bs--get-mode-name)
     ("" 2 2 left "  ")
     ("File" 12 12 left bs--get-file-name)
     ("" 2 2 left "  "))))
 '(bs-cycle-configuration-name "current-mode")
 '(bs-default-configuration "persp")
 '(column-number-mode t)
 '(comint-input-ignoredups t)
 '(comment-empty-lines (quote (quote eol)))
 '(comment-style (quote extra-line))
 '(company-idle-delay nil)
 '(compilation-error-regexp-alist
   (quote
    (bash clang-include gnu gcc-include ruby-Test::Unit)))
 '(completion-styles (quote (basic partial-completion initials emacs22)))
 '(dired-recursive-deletes (quote top))
 '(ediff-split-window-function
   (lambda
     (&optional x)
     (if
         (>
          (frame-width)
          150)
         (quote split-window-horizontally)
       (quote split-window-vertically))) nil nil "TODO: phase out? I think this might not be needed with the general window work")
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(elscreen-display-tab nil)
 '(emerge-diff-options "--ignore-all-space")
 '(enh-ruby-check-syntax nil)
 '(enh-ruby-program "/Users/ryan/.rubies/ruby-2.6.5/bin/ruby")
 '(erc-fools
   (quote
    ("Welkin" "konsolebox" "`slikts" "slack-irc-bot" "Aeyrix" "Hanmac" "yorickpeterse" "ianm_" "Bish" "dminuoso" "cthulchu" "Pateros")))
 '(erc-join-buffer (quote bury))
 '(erc-keywords
   (quote
    ("autotest\\|zentest\\|inline\\|parse_?tree\\|minitest\\|parser")))
 '(erc-kill-buffer-on-part t)
 '(erc-kill-queries-on-quit t)
 '(erc-kill-server-buffer-on-quit t)
 '(erc-mode-line-format "%t %a")
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)))
 '(erc-server-auto-reconnect nil)
 '(erc-services-mode t)
 '(erc-track-exclude-types (quote ("JOIN" "NICK" "PART" "QUIT")))
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(erc-track-position-in-mode-line (quote after-modes))
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(explicit-bash-args (quote ("--noediting" "--login" "-i")))
 '(ffap-file-finder (quote find-file-other-window))
 '(ffap-lax-url nil)
 '(ffap-machine-p-known (quote accept))
 '(find-file-visit-truename t)
 '(flycheck-display-errors-function (quote flycheck-display-error-messages-unless-error-list))
 '(flyspell-delayed-commands (quote (kmacro-call-macro kmacro-end-and-call-macro)))
 '(git-commit-summary-max-length 78)
 '(global-auto-revert-mode t)
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-all-abbrevs try-expand-dabbrev-visible try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill rwd-try-expand-tag try-complete-lisp-symbol-partially try-complete-lisp-symbol try-complete-file-name-partially try-complete-file-name try-expand-list)))
 '(history-length 1000)
 '(imenu-max-items 50)
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inf-ruby-default-implementation "pry")
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-dictionary "english")
 '(ispell-extra-args (quote ("\"--sug-mode=fast\"")))
 '(ispell-program-name "aspell")
 '(kill-ring-max 1000)
 '(line-move-visual nil)
 '(lua-indent-level 2)
 '(mac-pass-command-to-system nil)
 '(mac-pass-control-to-system nil)
 '(magit-delete-by-moving-to-trash nil)
 '(magit-diff-refine-hunk t)
 '(magit-emacsclient-executable "/usr/local/bin/emacsclient")
 '(magit-git-executable "/usr/local/bin/git")
 '(magit-log-cutoff-length 250)
 '(magit-push-always-verify nil)
 '(magit-repository-directories
   (quote
    (("~/Work/git/zenspider" . 1)
     ("~/Work/git/searbsg" . 1)
     ("~/Work/git/inspec" . 1)
     ("~/Work/git/chef" . 1))))
 '(magit-save-repository-buffers nil)
 '(magit-stashes-margin (quote (t age-abbreviated magit-log-margin-width nil 18)))
 '(magit-status-margin (quote (t age-abbreviated magit-log-margin-width nil 18)))
 '(magithub-api-low-threshold 30)
 '(magithub-api-timeout 3)
 '(markdown-fontify-code-blocks-natively t)
 '(mediawiki-site-alist
   (quote
    (("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" "Main Page")
     ("devchix" "http://www.wiki.devchix.com/" "zenspider" "" "Main Page"))))
 '(mediawiki-site-default "devchix")
 '(ns-alternate-modifier (quote none))
 '(ns-command-modifier (quote meta))
 '(ns-pop-up-frames nil)
 '(ns-use-native-fullscreen nil)
 '(oddmuse-directory "~/Library/Caches/oddmuse")
 '(oddmuse-username "RyanDavis")
 '(org-display-custom-times t)
 '(org-time-stamp-custom-formats (quote ("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>")))
 '(org-use-sub-superscripts (quote {}))
 '(outline-minor-mode-prefix "\357")
 '(override-keymap-rules
   (quote
    (("\230" bury-buffer
      (ruby python emacs-lisp racc))
     ("\214" rwd-scroll-top
      (shell comint)))))
 '(p4-do-find-file nil)
 '(package-selected-packages
   (quote
    (ag aggressive-indent auto-dim-other-buffers bison-mode browse-kill-ring company copy-as-format cyphejor dash dedicated default-text-scale delight diff-at-point dockerfile-mode elisp-slime-nav eval-in-repl expand-region f find-file-in-project flycheck flycheck-color-mode-line flycheck-package git-timemachine github-browse-file github-review goto-chg helpful htmlize inf-ruby jq-mode json-mode keyfreq kill-ring-search magit markdown-mode multiple-cursors outline-magic p4 package+ package-lint paredit phi-grep phi-search phi-search-mc popwin racket-mode s shackle shell-command string-edit w3m web-mode wgrep wgrep-ag which-key window-number window-purpose xr xr xterm-color yagist yaml-mode)))
 '(package-selected-packagse nil t)
 '(persp-initial-frame-name "*")
 '(persp-mode-prefix-key "")
 '(persp-sort (quote created))
 '(purpose-user-regexp-purposes (quote (("^magit" . magit))))
 '(racket-images-system-viewer "open")
 '(read-buffer-completion-ignore-case t)
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((ffip-project-root . "~/Work/git/zenspider/minitest-lint/")
     (flycheck-checker . chefstyle))))
 '(save-interprogram-paste-before-kill t)
 '(save-place-file "~/.emacs.d/emacs-places")
 '(save-place-limit 100)
 '(save-place-save-skipped nil)
 '(save-place-skip-check-regexp
   "\\`/\\(cdrom\\|floppy\\|mnt\\|\\([^@/:]*@\\)?[^@/:]*[^@/:.]:\\)")
 '(savehist-additional-variables
   (quote
    (kill-ring compile-command search-ring regexp-search-ring)))
 '(savehist-ignored-variables (quote (yes-or-no-p-history)))
 '(savehist-mode t)
 '(scheme-program-name "csi -I ../lib")
 '(scroll-bar-mode nil)
 '(search-whitespace-regexp nil)
 '(seeing-is-believing-executable "~/Bin/seeing_is_believing")
 '(seeing-is-believing-max-length 80)
 '(seeing-is-believing-max-results 3)
 '(seeing-is-believing-timeout 2)
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(smtpmail-smtp-server "envy.zenspider.com")
 '(smtpmail-smtp-service 25)
 '(smtpmail-smtp-user "ryan")
 '(sql-sqlite-program "sqlite3")
 '(ssh-directory-tracking-mode t)
 '(tab-always-indent (quote complete))
 '(tab-width 8)
 '(tags-case-fold-search nil)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-mode nil)
 '(tramp-copy-size-limit 1024)
 '(tramp-default-method "rsync")
 '(tramp-use-ssh-controlmaster-options nil)
 '(truncate-partial-width-windows nil)
 '(use-dialog-box nil)
 '(user-mail-address "ryand-ruby@zenspider.com")
 '(vc-annotate-display-mode (quote scale))
 '(vc-command-messages t)
 '(vc-handled-backends (quote (CVS SVN Git)))
 '(vc-p4-require-p4config t)
 '(vc-svn-program-name "/usr/bin/svn")
 '(visible-bell t)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(wdired-allow-to-change-permissions (quote advanced))
 '(web-mode-markup-indent-offset 2)
 '(whitespace-global-modes
   (quote
    (c-mode diff-mode emacs-lisp-mode enh-ruby-mode haskell-mode markdown-mode racc-mode racket-mode ruby-mode rust-mode scheme-mode sml-mode text-mode)))
 '(whitespace-style
   (quote
    (face tabs trailing lines-tail space-before-tab empty))))

;; TODO: figure out why this doesn't work in git-blame frames
;; '(cursor ((((background light)) (:background "black"))
;;           (((background dark))  (:background "white"))
;;           (t                    (:background "white"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "gray95"))))
 '(cursor ((((type ns)) (:background "Blue")) (((type tty)) (:background "Green")) (t (:background "Blue"))))
 '(diff-added ((t (:inherit diff-changed :foreground "green4"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red4"))))
 '(ediff-current-diff-C ((t (:background "#ddddff"))))
 '(ediff-even-diff-B ((t (:background "Grey" :foreground "black"))))
 '(ediff-fine-diff-A ((t (:background "#ffbbbb" :foreground "black"))))
 '(ediff-fine-diff-C ((t (:background "#aaaaff" :foreground "black"))))
 '(ediff-odd-diff-A ((t (:background "Grey" :foreground "black"))))
 '(ediff-odd-diff-C ((t (:background "Grey" :foreground "black"))))
 '(erc-input-face ((t (:foreground "dark green"))))
 '(erc-my-nick-face ((t (:foreground "dark green" :weight bold))))
 '(flycheck-color-mode-line-error-face ((t (:background "#f99"))))
 '(flycheck-color-mode-line-info-face ((t (:background "pale green"))))
 '(flycheck-color-mode-line-warning-face ((t (:background "gold"))))
 '(flyspell-incorrect ((t (:underline "red"))))
 '(font-lock-comment-face ((((type ns)) (:foreground "Dark Blue")) (((type tty)) (:foreground "Light Blue"))))
 '(font-lock-constant-face ((((type ns)) (:foreground "SlateBlue4")) (((type tty)) (:foreground "RoyalBlue1"))))
 '(font-lock-string-face ((((type ns)) (:foreground "Forest Green")) (((type tty)) (:foreground "green4"))))
 '(magit-item-highlight ((t (:background "#eee"))))
 '(mode-line ((t (:background "grey75" :foreground "black"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey20" :weight light))))
 '(racket-keyword-argument-face ((t (:foreground "dark blue"))))
 '(region ((((type tty)) (:background "color-240"))))
 '(sh-heredoc ((t (:foreground "DarkOliveGreen4"))))
 '(shm-current-face ((t (:background "#eee"))))
 '(shm-quarantine-face ((t (:background "#fcc"))))
 '(whitespace-line ((((type ns)) (:background "gray90")) (((type tty)) (:background "gray20")) (t (:background "red"))))
 '(whitespace-tab ((((type tty)) (:background "grey22")) (((type ns)) (:background "beige")) (t (:background "beige")))))
