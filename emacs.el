; Stupid compatibility checking

(setq running-xemacs (featurep 'xemacs))

(if (featurep 'xemacs)
    (progn
      (message "you are running xemacs... good")
      ;; Extend Info directories - TODO: needs counterpart for GNU emacs
      (mapc '(lambda (x)
	       (pushnew x Info-directory-list :test 'string=))
	    (list 
	     "/usr/share/info"
	     (expand-file-name "~/Bin/elisp/third-party/tramp")
	     )))
  (progn
    (message "Have I ever mentioned that GNU emacs sucks?")
    (defalias 'mapc 'mapcar)
    (load-library "cl")
    (setq apropos-do-all t)
    (transient-mark-mode 1)
    (add-hook 'comint-output-filter-functions
	      'comint-watch-for-password-prompt)
    ))

;; EVIL - can't cntl-c a proccess in a shell! (setq process-connection-type nil)
(put 'erase-buffer 'disabled nil) ;; nukes stupid warning

;;; Add my elisp directory to pathing

(mapc '(lambda (path)
	 (pushnew (expand-file-name path) load-path :test 'string=))
      (list 
       "/usr/share/emacs/site-lisp/"
       (if (featurep 'xemacs) "/usr/local/lib/xemacs/site-lisp/" "/usr/local/lib/emacs/site-lisp/")
       "~/Bin/elisp/"
       "~/Bin/elisp/third-party/"
       "~/Bin/elisp/third-party/cedet"
       "~/Bin/elisp/third-party/ecb"
       "~/Bin/elisp/third-party/jde/lisp"
       "~/Bin/elisp/third-party/mmm"
       "~/Bin/elisp/third-party/semantic"
       "~/Bin/elisp/third-party/tramp"
       ))

(if (featurep 'xemacs)
    t
  (require 'vc-hooks))

;;; Load My Stuff
(load "toggle")
(load "setup-aliases")
(load "setup-keys")
(load "setup-mail-and-news")
(load "setup-misc")
(load "setup-modes")
(load "bs")

(setq explicit-shell-file-name "/bin/bash")

;; Options Menu Settings
;; =====================
(cond
 ((and (string-match "XEmacs" emacs-version)
       (boundp 'emacs-major-version)
       (or (and
            (= emacs-major-version 19)
            (>= emacs-minor-version 14))
           (= emacs-major-version 20))
       (fboundp 'load-options-file))
  (load-options-file "/home/ryand/.xemacs-options")))
;; ============================
;; End of Options Menu Settings

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode nil nil (cua-base))
 '(dired-recursive-deletes (quote top))
 '(ecb-layout-window-sizes (quote (("left9" (0.325 . 0.975)))))
 '(ecb-options-version "2.32")
 '(global-font-lock-mode t nil (font-core))
 '(global-semantic-decoration-mode nil nil (semantic-decorate-mode))
 '(global-semantic-highlight-edits-mode nil nil (semantic-util-modes))
 '(global-semantic-idle-completions-mode nil nil (semantic-idle))
 '(global-semantic-idle-scheduler-mode nil nil (semantic-idle))
 '(global-semantic-idle-summary-mode t nil (semantic-idle))
 '(global-semantic-show-parser-state-mode nil nil (semantic-util-modes))
 '(global-semantic-show-unmatched-syntax-mode nil nil (semantic-util-modes))
 '(global-semantic-stickyfunc-mode nil nil (semantic-util-modes))
 '(global-senator-minor-mode t nil (senator))
 '(inhibit-splash-screen t)
 '(mouse-wheel-mode t nil (mwheel))
 '(save-place t nil (saveplace))
 '(semanticdb-global-mode t nil (semanticdb))
 '(show-paren-mode t)
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode t)
 '(which-function-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "slate blue")))))
