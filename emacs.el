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

(mapc '(lambda (path)
	 (pushnew (expand-file-name path) load-path :test 'string=))
      (list 
       "/usr/share/emacs/site-lisp/"
       (if (featurep 'xemacs)
	   "/usr/local/lib/xemacs/site-lisp/"
	 "/usr/local/lib/emacs/site-lisp/")
       "~/Bin/elisp/"
       "~/Bin/elisp/third-party/"
       ))

(mapc '(lambda (path) (pushnew (expand-file-name path) load-path :test 'string=))
      (remove-if-not (lambda (o)
		       (and (file-directory-p o)
			    (not (string-match "\\.$" o))))
		     (append (directory-files "~/Bin/elisp/third-party" t)
			     (directory-files "~/Bin/elisp/third-party/cedet-1.0pre3" t)
			     (directory-files "~/Bin/elisp/third-party/cedet-1.0pre3/semantic" t))))

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
 '(icicle-reminder-prompt-flag 0)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-splash-screen t)
 '(mouse-wheel-mode t nil (mwheel))
 '(quack-programs (quote ("/usr/local/bin/mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(quack-smart-open-paren-p nil)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-mode nil)
 '(transient-mark-mode t)
 '(vc-path (quote ("/opt/local/bin" "/usr/local/bin")))
 '(vc-svn-program-name "/opt/local/bin/svn")
 '(which-function-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "slate blue")))))
