(setq inhibit-startup-message t) ;; avoid splash screen

;; Add my elisp directory to pathing
(pushnew (expand-file-name "~/Bin/elisp/")
	 load-path :test 'string=)
(pushnew (expand-file-name "~/Bin/elisp/cc-mode/")
	 load-path :test 'string=)

(byte-recompile-directory (expand-file-name "~/Bin/elisp") 0)

;; Load My Stuff
(load "setup-aliases")
(load "setup-keys")
(load "setup-mail-and-news")
(load "setup-misc")
(load "setup-modes")

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
  (load-options-file "/toasters/toaster5/users/ryand/.xemacs-options")))
;; ============================
;; End of Options Menu Settings

(custom-set-variables
 '(bar-cursor 2)
 '(time-stamp-format "%02y-%02m-%02d %02H:%02M:%02S %u")
 '(user-mail-address "ryand@amazon.com" t)
 '(query-user-mail-address nil))
(custom-set-faces)
