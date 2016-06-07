(require 'autoload)                     ; = ;;;###autoload

(fmakunbound 'kbd)                      ; HACK for emacs <24.4
(defun kbd (keys)
    "Convert KEYS to the internal Emacs key representation.
KEYS should be a string constant in the format used for
saving keyboard macros (see `edmacro-mode')."
      (read-kbd-macro keys))

(unless user-init-file                  ; if running w/: -q --debug-init
  (setq user-init-file (expand-file-name "~/.emacs.el")))

(defvar user-init-dir (file-name-directory
                       (or (file-symlink-p user-init-file)
                           (or user-init-file
                               (expand-file-name "~/.emacs.el"))))
  "Root directory of emacs.el, after following symlinks, etc.")

(add-to-list 'load-path user-init-dir t)
(add-to-list 'load-path (concat user-init-dir "third-party") t) ; TODO: remove

(let* ((host-cmd "hostname | /usr/bin/perl -pe 's/^.*?([^\.]+\.[^\.]+)$/$1/'")
       (nl          (string-to-char "\n"))
       (os-name     (symbol-name system-type))
       (domain-name (remove nl (shell-command-to-string host-cmd)))
       (host-name   (car (split-string (system-name) "\\."))))
  (load (concat "os/" os-name) t)         ;; os/darwin
  (load (concat "domain/" domain-name) t) ;; domain/zenspider.com
  (load (concat "host/" host-name) t))    ;; host/greed

(load "rwd-autoloads")
(load "rwd-packages")

(rwd-autoloads)
(rwd-autohooks)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list
   (quote
    ("/usr/share/info" "~/Bin/elisp/info" "/Developer/usr/share/info")))
 '(apropos-do-all t)
 '(backup-by-copying-when-linked t)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(comint-input-ignoredups t)
 '(comment-empty-lines (quote (quote eol)))
 '(comment-style (quote extra-line))
 '(compilation-error-regexp-alist (quote (bash java gnu gcc-include)))
 '(completion-styles (quote (basic partial-completion initials emacs22)))
 '(dired-recursive-deletes (quote top))
 '(ediff-split-window-function
   (lambda
     (x)
     (if
         (>
          (frame-width)
          150)
         (quote split-window-horizontally)
       (quote split-window-vertically))))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(elscreen-display-tab nil)
 '(emerge-diff-options "--ignore-all-space")
 '(enh-ruby-check-syntax nil)
 '(enh-ruby-program "/Users/ryan/.rbenv/versions/2.3.1/bin/ruby")
 '(erc-fools (quote ("Aeyrix" "Hanmac" "yorickpeterse" "ianm_" "Bish")))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)))
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(explicit-bash-args (quote ("--noediting" "--login" "-i")))
 '(ffap-file-finder (quote find-file-other-window))
 '(find-file-visit-truename t)
 '(flyspell-delayed-commands (quote (kmacro-call-macro kmacro-end-and-call-macro)))
 '(fuel-listener-factor-binary
   "/MyApplications/dev/factor/Factor.app/Contents/MacOS/factor")
 '(fuel-listener-factor-image "/MyApplications/dev/factor/factor.image")
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
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-dictionary "english")
 '(ispell-extra-args (quote ("\"--sug-mode=fast\"")))
 '(line-move-visual nil)
 '(lua-indent-level 2)
 '(magit-diff-refine-hunk t)
 '(magit-emacsclient-executable "/usr/local/bin/emacsclient")
 '(magit-log-cutoff-length 250)
 '(magit-push-always-verify nil)
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
 '(outline-minor-mode-prefix "\357")
 '(override-keymap-rules
   (quote
    (("\230" bury-buffer
      (ruby python emacs-lisp racc))
     ("\214" rwd-scroll-top
      (shell comint)))))
 '(p4-do-find-file nil)
 '(pastebin-default-domain "zenspider.pastebin.com")
 '(pastebin-default-subdomain "zenspider")
 '(pastebin-domain-versions
   (quote
    (("pastebin.com" "/api_public.php")
     ("zenspider.pastebin.com" "/api_public.php")
     ("pastebin.example.com" "/pastebin.php"))))
 '(quack-default-program "racket")
 '(quack-pltcollect-dirs (quote ("/MyApplications/dev/lisp/Racket/collects/")))
 '(read-buffer-completion-ignore-case t)
 '(safe-local-variable-values
   (quote
    ((ffip-project-file . "Rakefile")
     (ffip-prune-patterns . "tmp/isolate")
     (encoding . utf-8)
     (backup-inhibited . t)
     (racc-token-length-max . 14))))
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/emacs-places")
 '(save-place-limit 100)
 '(save-place-save-skipped nil)
 '(save-place-skip-check-regexp
   "\\`/\\(cdrom\\|floppy\\|mnt\\|\\([^@/:]*@\\)?[^@/:]*[^@/:.]:\\)")
 '(savehist-additional-variables (quote (kill-ring)))
 '(savehist-ignored-variables (quote (yes-or-no-p-history)))
 '(savehist-mode t nil (savehist))
 '(scheme-program-name "csi -I ../lib")
 '(scroll-bar-mode nil)
 '(search-whitespace-regexp nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(smex-save-file "~/.emacs.d/.smex-items")
 '(smtpmail-smtp-server "envy.zenspider.com")
 '(smtpmail-smtp-service 25)
 '(smtpmail-smtp-user "ryan")
 '(sql-sqlite-program "sqlite3")
 '(tab-width 8)
 '(tags-case-fold-search nil)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-mode nil)
 '(tramp-copy-size-limit 1024)
 '(tramp-default-method "rsync")
 '(tramp-use-ssh-controlmaster-options nil)
 '(transient-mark-mode (quote (only . t)))
 '(truncate-partial-width-windows nil)
 '(use-dialog-box nil)
 '(user-mail-address "ryand-ruby@zenspider.com")
 '(vc-command-messages t)
 '(vc-handled-backends (quote (CVS SVN GIT)))
 '(vc-p4-require-p4config t)
 '(vc-svn-program-name "/usr/bin/svn")
 '(visible-bell t)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(wdired-allow-to-change-permissions (quote advanced))
 '(whitespace-global-modes
   (quote
    (ruby-mode emacs-lisp-mode scheme-mode enh-ruby-mode c-mode racket-mode sml-mode)))
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
 '(cursor ((((type ns)) (:background "Blue")) (((type tty)) (:background "Green")) (t (:background "Blue"))))
 '(diff-added ((t (:inherit diff-changed :foreground "green4"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red4"))))
 '(enh-ruby-heredoc-delimiter-face-xxx ((t (:foreground "Brown"))))
 '(erc-input-face ((t (:foreground "dark green"))))
 '(erc-my-nick-face ((t (:foreground "dark green" :weight bold))))
 '(flyspell-incorrect ((t (:underline "red"))))
 '(font-lock-comment-face ((((type ns)) (:foreground "Dark Blue")) (((type tty)) (:foreground "Light Blue"))))
 '(font-lock-constant-face ((((type ns)) (:foreground "SlateBlue4")) (((type tty)) (:foreground "RoyalBlue1"))))
 '(font-lock-string-face ((((type ns)) (:foreground "Forest Green")) (((type tty)) (:foreground "green4"))))
 '(magit-item-highlight ((t (:background "#eee"))))
 '(mode-line ((t (:background "grey75" :foreground "black"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey20" :weight light))))
 '(racket-keyword-argument-face ((t (:foreground "dark blue"))))
 '(region ((((type tty)) (:background "color-240"))))
 '(whitespace-line ((((type ns)) (:background "gray90")) (((type tty)) (:background "gray20")) (t (:background "red"))))
 '(whitespace-tab ((((type tty)) (:background "grey22")) (((type ns)) (:background "beige")) (t (:background "beige")))))

(put 'narrow-to-region 'disabled nil)
