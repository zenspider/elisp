; Compatibility Layer:

(setq running-xemacs (featurep 'xemacs))
(setq running-emacs (not running-xemacs))

(if running-xemacs
    (add-to-list 'Info-directory-list "/usr/share/info"))

(if running-emacs
    (progn
      (eval-when-compile
        (when (< emacs-major-version 20) (require 'cl)))
      (require 'vc-hooks)
      (setq apropos-do-all t)
      (transient-mark-mode 1)

      (add-hook 'comint-output-filter-functions
                'comint-watch-for-password-prompt)))

; Pathing:

(dolist (path (split-string
               (shell-command-to-string
                "find ~/Bin/elisp -maxdepth 2 -type d | sort -u") nil))
  (add-to-list 'load-path path))

; My libs:

(load "setup-aliases")
(load "setup-keys")
(load "setup-mail-and-news")
(load "setup-misc")
(load "setup-modes")

(add-hook 'after-init-hook (lambda () (small)) t)

;; (require 'icicles)
;; (icicle-mode 1)))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(dired-recursive-deletes (quote top))
 '(ecb-layout-window-sizes (quote (("left9" (0.325 . 0.975)))))
 '(ecb-options-version "2.32")
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(explicit-shell-file-name "/bin/bash")
 '(find-file-visit-truename t)
 '(global-font-lock-mode t nil (font-core))
 '(icicle-reminder-prompt-flag 0)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-splash-screen t)
 '(locate-command "mdfind")
 '(mouse-wheel-mode t nil (mwheel))
 '(pop-up-windows nil)
 '(save-place t nil (saveplace))
 '(search-highlight t)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-mode nil)
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(vc-handled-backends (quote (RCS CVS SVN SCCS MCVS)))
 '(vc-path (quote ("/opt/local/bin" "/usr/local/bin")))
 '(vc-svn-program-name "/opt/local/bin/svn")
 '(visible-bell t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "Dark Blue"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "Forest Green")))))
