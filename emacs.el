; Stupid compatibility checking

(setq apropos-do-all t)

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

;;; Add my elisp directory to pathing

(mapc '(lambda (path)
	 (pushnew (expand-file-name path) load-path :test 'string=))
      (list 
       "/usr/share/emacs/site-lisp/"
       "/usr/local/lib/xemacs/site-lisp/"
       "~/Bin/elisp/"
       "~/Bin/elisp/third-party/"
       "~/Bin/elisp/third-party/tramp"
       "~/Bin/elisp/third-party/semantic"
       "~/Bin/elisp/third-party/jde/lisp"
       ))

(require 'vc-hooks)

;;; Load My Stuff
(load "setup-aliases")
(load "setup-keys")
(load "setup-mail-and-news")
(load "setup-misc")
(load "setup-modes")
(load "bs")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(bar-cursor (quote (quote other)))
 '(browse-url-browser-function (quote browse-url-w3))
 '(c-echo-syntactic-information-p t)
 '(column-number-mode t)
 '(cperl-help t)
 '(delete-key-deletes-forward nil)
 '(eldoc-mode t t)
 '(gutter-buffers-tab-visible-p nil)
 '(gutter-visible-p nil)
 '(inhibit-startup-message t)
 '(jde-auto-parse-buffer-interval 60)
 '(jde-gen-get-set-var-template (quote ("(end-of-line) '&" "(P \"Variable type: \" type) \" \"" "(P \"Variable name: \" name) \" ;\" '>'n '>'n" "\"/**\" '>'n" "\"* Get the value of \" (s name) \".\" '>'n" "\"* @return value of \" (s name) \".\" '>'n" "\"**/\" '>'n '>'n" " \"public \" (s type)" "(if (string= \"boolean\" (jde-gen-lookup-named 'type) ) " "\" is\" " "\" \" ) " "(s name)" "\"() \"" "(if jde-gen-k&r " "()" "'>'n)" "\"{\" '>'n" "\"return this.\" (s name) \";\" '>'n \"}\"" "'>'n '>'n" "\"/**\" '>'n" "\"* Set the value of \" (s name) \".\" '>'n" "\"* @param \" (s name) \" Value to assign to \" (s name) \".\" '>'n" "\"**/\" '>'n '>'n" "\"public void \" (s name)" "\"(\" (s type) \" \" (s name) \") \" " "(if jde-gen-k&r " "()" "'>'n)" "\"{\" '>'n" "'>'n \"this.\" (s name) \" = \" (s name) \";\" '>'n \"}\" '>'n'>")))
 '(jde-jdk-doc-url "file:///usr/share/doclib/java/index.html")
 '(jde-make-program "gmake")
 '(lazy-lock-mode t t (lazy-lock))
 '(line-number-mode t)
 '(p4-executable "/Users/ryan/Bin/p4")
 '(p4-strict-complete nil)
 '(paren-mode (quote paren) nil (paren))
 '(query-user-mail-address nil t)
 '(revert-without-query (quote ("\\.html")))
 '(save-place t nil (saveplace))
 '(shell-multiple-shells t)
 '(show-paren-mode t nil (paren))
 '(sql-postgres-options (quote ("-P pager=off")))
 '(toolbar-visible-p nil)
 '(user-mail-address (concat "ryand@" (getenv "DOMAIN"))))

(setenv "ESHELL" "/bin/bash")

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-string-face ((((class color) (background light)) (:foreground "CadetBlue")))))

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

(put 'erase-buffer 'disabled nil)
