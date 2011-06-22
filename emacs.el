(require 'autoload)                     ; = ;;;###autoload

(autoload 'find-lisp-find-files "find-lisp" nil t)
(autoload 'find-lisp-find-files-internal "find-lisp" nil t)

(defvar user-init-dir (file-name-directory
                       (or (file-symlink-p user-init-file) user-init-file))
  "Root directory of emacs.el, after following symlinks, etc.")

(add-to-list 'load-path user-init-dir t)
(add-to-list 'load-path (concat user-init-dir "third-party") t) ; TODO: remove
(add-to-list 'load-path (concat user-init-dir "third-party/magit") t)
(add-to-list 'load-path (expand-file-name "~/Sites/emacs/elisp") t)

(defun rwd-recompile-init ()
  (interactive)
  (byte-recompile-directory (expand-file-name user-init-dir) 0))

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

(when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
  (add-to-list 'package-archives
               '("elpa" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize))

(defun rwd-install-package (name)
  (or (package-installed-p name) (package-install name)))

(rwd-autoloads)
(rwd-autohooks)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote ("/usr/share/info" "~/Bin/elisp/info" "/opt/local/share/info/")))
 '(apropos-do-all t)
 '(backup-by-copying-when-linked t)
 '(blank-chars (quote (tabs trailing lines space-before-tab)))
 '(blank-line-length 82)
 '(blank-style (quote (color)))
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
 '(erc-fools (quote ("ianm_")))
 '(erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)))
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(ffap-file-finder (quote find-file-other-window))
 '(find-file-visit-truename t)
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
 '(magit-log-cutoff-length 250)
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
 '(safe-local-variable-values (quote ((encoding . utf-8) (backup-inhibited . t) (racc-token-length-max . 14))))
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
 '(tab-width 2)
 '(tags-case-fold-search nil)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-mode nil)
 '(tramp-copy-size-limit 1024)
 '(tramp-default-method "rsyncc")
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(use-dialog-box nil)
 '(vc-command-messages t)
 '(vc-handled-backends (quote (CVS SVN GIT)))
 '(vc-p4-require-p4config t)
 '(vc-path (quote ("/opt/local/bin" "/usr/local/bin")))
 '(vc-svn-program-name "/opt/local/bin/svn")
 '(visible-bell t)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(wdired-allow-to-change-permissions (quote advanced)))

;; TODO: figure out why this doesn't work in git-blame frames
;; '(cursor ((((background light)) (:background "black"))
;;           (((background dark))  (:background "white"))
;;           (t                    (:background "white"))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blank-line ((((background light)) (:background "gray80")) (((type tty)) (:background "gray20")) (t (:background "red"))))
 '(blank-line-face ((t (:background "gray90"))))
 '(cursor ((((background light)) (:background "Blue")) (((background dark)) (:background "Blue")) (t (:background "Blue"))))
 '(diff-added ((t (:inherit diff-changed :foreground "green4"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red4"))))
 '(erc-input-face ((t (:foreground "dark green"))))
 '(erc-my-nick-face ((t (:foreground "dark green" :weight bold))))
 '(flyspell-incorrect ((t (:underline "red"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "Dark Blue"))))
 '(font-lock-constant-face ((((class color) (min-colors 88) (background light)) (:foreground "SlateBlue4"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "Forest Green")))))

(put 'narrow-to-region 'disabled nil)
