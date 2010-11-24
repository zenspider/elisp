(require 'autoload)                     ; = ;;;###autoload

(autoload 'find-lisp-find-files "find-lisp" nil t)
(autoload 'find-lisp-find-files-internal "find-lisp" nil t)

(defvar el-root-dir                     ; TODO: there has to be a better way
  (let* ((el-file (or (buffer-file-name) load-file-name)))
    (file-name-directory
     (or (file-symlink-p el-file) el-file)))
  "Root directory of emacs.el, after following symlinks, etc.")

(add-to-list 'load-path el-root-dir t)
(add-to-list 'load-path (concat el-root-dir "third-party") t) ; TODO: remove
(add-to-list 'load-path (expand-file-name "~/Sites/emacs/elisp") t)

;; Compatibility Layer (TODO: remove):

(setq running-xemacs (featurep 'xemacs))
(setq running-emacs  (not running-xemacs))

(defun rwd-recompile-init ()
  (interactive)
  (byte-recompile-directory (expand-file-name el-root-dir) 0))

;; from technomancy with some tweaks
(defun rwd-autoloads ()
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive)
  (let* ((autoload-file (concat el-root-dir generated-autoload-file)))
    (if (or (not (file-exists-p autoload-file))
            (catch 'newer
              (dolist (file (find-lisp-find-files el-root-dir "\\.el$"))
                (if (file-newer-than-file-p file autoload-file)
                    (throw 'newer file)))))
        (let ((generated-autoload-file autoload-file)
              (el-root-subdirs (find-lisp-find-files-internal
                                el-root-dir
                                'find-lisp-file-predicate-is-directory
                                'find-lisp-default-directory-predicate)))
          (apply 'update-directory-autoloads (cons el-root-dir el-root-subdirs))
          (load autoload-file) ; helps rwd-recompile-init dependencies
          (rwd-recompile-init)))
    (message "loading autoloads")
    (load autoload-file)
    (message "done loading autoloads")))

(rwd-autoloads)

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
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(comint-input-ignoredups t)
 '(comment-empty-lines (quote (quote eol)))
 '(compilation-error-regexp-alist (quote (bash java gnu gcc-include)))
 '(dired-recursive-deletes (quote top))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)))
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(ffap-file-finder (quote find-file-other-window))
 '(find-file-visit-truename t)
 '(global-auto-revert-mode t)
 '(hippie-expand-try-functions-list (quote (try-expand-all-abbrevs try-expand-list try-expand-dabbrev-visible try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-tag try-complete-file-name-partially try-complete-file-name)))
 '(history-length 1000)
 '(imenu-max-items 50)
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(line-move-visual nil)
 '(ns-alternate-modifier (quote none))
 '(ns-command-modifier (quote meta))
 '(ns-pop-up-frames nil)
 '(oddmuse-directory "~/Library/Caches/oddmuse")
 '(oddmuse-username "RyanDavis")
 '(override-keymap-rules (quote (("\230" bury-buffer (ruby python emacs-lisp)) ("\214" rwd-scroll-top (shell comint)))))
 '(pastebin-default-subdomain "zenspider")
 '(read-buffer-completion-ignore-case t)
 '(safe-local-variable-values (quote ((backup-inhibited . t) (racc-token-length-max . 14))))
 '(save-place t nil (saveplace))
 '(save-place-limit 100)
 '(save-place-save-skipped nil)
 '(save-place-skip-check-regexp "\\`/\\(cdrom\\|floppy\\|mnt\\|\\([^@/:]*@\\)?[^@/:]*[^@/:.]:\\)")
 '(savehist-ignored-variables (quote (yes-or-no-p-history)))
 '(savehist-mode t nil (savehist))
 '(scheme-program-name "mzscheme")
 '(scroll-bar-mode nil)
 '(search-whitespace-regexp nil)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tab-width 2)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-mode nil)
 '(tramp-copy-size-limit 1024)
 '(tramp-default-method "rsyncc")
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(use-dialog-box nil)
 '(vc-handled-backends (quote (CVS SVN GIT)))
 '(vc-p4-require-p4config t)
 '(vc-path (quote ("/opt/local/bin" "/usr/local/bin")))
 '(vc-svn-program-name "/opt/local/bin/svn")
 '(visible-bell t)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(wdired-allow-to-change-permissions (quote advanced)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blank-line ((((background light)) (:background "gray80")) (((type tty)) (:background "gray20")) (t (:background "red"))))
 '(blank-line-face ((t (:background "gray90"))))
 '(cursor ((t (:background "black"))))
 '(diff-added ((t (:inherit diff-changed :foreground "green4"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red4"))))
 '(erc-input-face ((t (:foreground "dark green"))))
 '(erc-my-nick-face ((t (:foreground "dark green" :weight bold))))
 '(flyspell-incorrect ((t (:underline "red"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "Dark Blue"))))
 '(font-lock-constant-face ((((class color) (min-colors 88) (background light)) (:foreground "SlateBlue4"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "Forest Green")))))
