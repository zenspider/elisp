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
       "~/Bin/elisp/third-party/tramp"
       "~/Bin/elisp/third-party/semantic"
       "~/Bin/elisp/third-party/jde/lisp"
       ))

(if (featurep 'xemacs)
    t
  (require 'vc-hooks))

;;; Load My Stuff
(load "setup-aliases")
(load "setup-keys")
(load "setup-mail-and-news")
(load "setup-misc")
(load "setup-modes")
(load "bs")



(setenv "ESHELL" "/bin/bash")



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

