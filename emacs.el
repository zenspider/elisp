(require 'autoload)                     ; = ;;;###autoload

(setq running-osx (or (featurep 'mac-carbon) (eq 'ns window-system)))

(autoload 'find-lisp-find-files "find-lisp" nil t)
(autoload 'find-lisp-find-files-internal "find-lisp" nil t)

(defvar user-init-dir (file-name-directory
                       (or (file-symlink-p user-init-file) user-init-file))
  "Root directory of emacs.el, after following symlinks, etc.")

(add-to-list 'load-path user-init-dir t)
(add-to-list 'load-path (concat user-init-dir "third-party") t) ; TODO: remove
(add-to-list 'load-path (concat user-init-dir "third-party/magit") t)
(add-to-list 'load-path (concat user-init-dir "third-party/fuel") t)
(add-to-list 'load-path (expand-file-name "~/Sites/emacs/elisp") t)
(add-to-list 'load-path (expand-file-name "~/Work/git/mark-multiple")) ; TODO nuke?

(defun rwd-recompile-init ()
  (interactive)
  (byte-recompile-directory (expand-file-name user-init-dir) 0))

(setq generated-autoload-file "loaddefs.el") ;; HACK for emacs 24.1

(defun rwd-autoloads ()
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive)
  (let* ((autoload-file (concat user-init-dir generated-autoload-file)))
    (if (or (not (file-exists-p autoload-file))
            (catch 'newer
              (dolist (file (find-lisp-find-files user-init-dir "\\.el$"))
                (if (file-newer-than-file-p file autoload-file)
                    (throw 'newer file)))))
        (let ((generated-autoload-file autoload-file)
              (el-root-subdirs (find-lisp-find-files-internal
                                user-init-dir
                                'find-lisp-file-predicate-is-directory
                                'find-lisp-default-directory-predicate)))
          (apply 'update-directory-autoloads (cons user-init-dir el-root-subdirs))
          (load autoload-file) ; helps rwd-recompile-init dependencies
          (rwd-recompile-init)))
    (message "loading autoloads")
    (load autoload-file)
    (message "done loading autoloads")))

(require 'package)
(dolist (repo '(("elpa"      . "http://tromey.com/elpa/")
                ("marmalade" . "http://marmalade-repo.org/packages/")
                ;; ("melpa"     . "http://melpa.milkbox.net/packages/")
                ))
  (add-to-list 'package-archives repo))

(package-initialize)

(defun rwd-install-package (name)
  (or (package-installed-p name) (package-install name)))

(rwd-autoloads)
(rwd-autohooks)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote ("/usr/share/info" "~/Bin/elisp/info" "/Developer/usr/share/info")))
 '(apropos-do-all t)
 '(backup-by-copying-when-linked t)
 '(blink-cursor-mode nil)
 '(cluck-fontify-style nil)
 '(cluck-global-menu-p nil)
 '(column-number-mode t)
 '(comint-input-ignoredups t)
 '(comment-empty-lines (quote (quote eol)))
 '(compilation-error-regexp-alist (quote (bash java gnu gcc-include)))
 '(completion-styles (quote (basic partial-completion initials emacs22)))
 '(dired-recursive-deletes (quote top))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(emerge-diff-options "--ignore-all-space")
 '(erc-fools (quote ("yorickpeterse" "ianm_")))
 '(erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)))
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(explicit-bash-args (quote ("--noediting" "--login" "-i")))
 '(ffap-file-finder (quote find-file-other-window))
 '(find-file-visit-truename t)
 '(flyspell-delayed-commands (quote (kmacro-call-macro kmacro-end-and-call-macro)))
 '(fuel-listener-factor-binary "/MyApplications/dev/factor/Factor.app/Contents/MacOS/factor")
 '(fuel-listener-factor-image "/MyApplications/dev/factor/factor.image")
 '(global-auto-revert-mode t)
 '(hippie-expand-try-functions-list (quote (try-expand-all-abbrevs try-expand-list try-expand-dabbrev-visible try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill rwd-try-expand-tag try-complete-lisp-symbol-partially try-complete-lisp-symbol try-complete-file-name-partially try-complete-file-name)))
 '(history-length 1000)
 '(imenu-max-items 50)
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(line-move-visual nil)
 '(lua-indent-level 2)
 '(magit-log-cutoff-length 250)
 '(mediawiki-site-alist (quote (("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" "Main Page") ("devchix" "http://www.wiki.devchix.com/" "zenspider" "" "Main Page"))))
 '(mediawiki-site-default "devchix")
 '(ns-alternate-modifier (quote none))
 '(ns-command-modifier (quote meta))
 '(ns-pop-up-frames nil)
 '(oddmuse-directory "~/Library/Caches/oddmuse")
 '(oddmuse-username "RyanDavis")
 '(override-keymap-rules (quote (("\230" bury-buffer (ruby python emacs-lisp)) ("\214" rwd-scroll-top (shell comint)))))
 '(pastebin-default-domain "zenspider.pastebin.com")
 '(pastebin-default-subdomain "zenspider")
 '(pastebin-domain-versions (quote (("pastebin.com" "/api_public.php") ("zenspider.pastebin.com" "/api_public.php") ("pastebin.example.com" "/pastebin.php"))))
 '(read-buffer-completion-ignore-case t)
 '(enh-ruby-check-syntax nil)
 '(safe-local-variable-values (quote ((encoding . utf-8) (backup-inhibited . t) (racc-token-length-max . 14))))
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-limit 100)
 '(save-place-save-skipped nil)
 '(save-place-skip-check-regexp "\\`/\\(cdrom\\|floppy\\|mnt\\|\\([^@/:]*@\\)?[^@/:]*[^@/:.]:\\)")
 '(savehist-ignored-variables (quote (yes-or-no-p-history)))
 '(savehist-mode t nil (savehist))
 '(scheme-program-name "csi -I ../lib")
 '(scroll-bar-mode nil)
 '(search-whitespace-regexp nil)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(sql-sqlite-program "sqlite3")
 '(tab-width 8)
 '(tags-case-fold-search nil)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-mode nil)
 '(tramp-copy-size-limit 1024)
 '(tramp-default-method "rsyncc")
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(use-dialog-box nil)
 '(user-mail-address "ryand-ruby@zenspider.com")
 '(vc-command-messages t)
 '(vc-handled-backends (quote (CVS SVN GIT)))
 '(vc-p4-require-p4config t)
 '(vc-path (quote ("/opt/local/bin" "/usr/local/bin")))
 '(vc-svn-program-name "/opt/local/bin/svn")
 '(visible-bell t)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(wdired-allow-to-change-permissions (quote advanced))
 '(whitespace-global-modes (quote (ruby-mode emacs-lisp-mode scheme-mode enh-ruby-mode)))
 '(whitespace-style (quote (face tabs trailing lines-tail space-before-tab empty))))

;; TODO: figure out why this doesn't work in git-blame frames
;; '(cursor ((((background light)) (:background "black"))
;;           (((background dark))  (:background "white"))
;;           (t                    (:background "white"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((((background light)) (:background "Blue")) (((background dark)) (:background "Blue")) (t (:background "Blue"))))
 '(diff-added ((t (:inherit diff-changed :foreground "green4"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red4"))))
 '(erc-input-face ((t (:foreground "dark green"))))
 '(erc-my-nick-face ((t (:foreground "dark green" :weight bold))))
 '(flyspell-incorrect ((t (:underline "red"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "Dark Blue"))))
 '(font-lock-constant-face ((((class color) (min-colors 88) (background light)) (:foreground "SlateBlue4"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "Forest Green"))))
 '(enh-ruby-heredoc-delimiter-face-xxx ((t (:foreground "Brown"))))
 '(whitespace-line ((((background light)) (:background "gray80")) (((type tty)) (:background "gray20")) (t (:background "red")))))

(put 'narrow-to-region 'disabled nil)
