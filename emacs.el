; Stupid compatibility checking

(unless (featurep 'xemacs)

  (message "Have I ever mentioned that GNU emacs sucks?")

  (defun add-to-list (list-var element)
    "Stolen from xemac's subr.el"
    (or (member element (symbol-value list-var))
	(set list-var (cons element (symbol-value list-var))))) 

  (defalias 'mapc 'mapcar)

  (load-library "cl")

  (setq apropos-do-all t)

  (defalias 'Info-directory-list 'Info-default-directory-list)
  )

(if (featurep 'xemacs)

    (message "you are running xemacs... good")

    ;; Extend Info directories
    (mapc '(lambda (x)
	     (pushnew x Info-directory-list :test 'string=))
	  (list 
	   "/usr/share/info"
	   (expand-file-name "~/Bin/elisp/third-party/tramp")
	   ))
)

;;; Add my elisp directory to pathing

(mapc '(lambda (path)
	 (pushnew (expand-file-name path) load-path :test 'string=))
      (list 
       "/usr/local/lib/xemacs/site-lisp/"
       "~/Bin/elisp/"
       "~/Bin/elisp/third-party/"
       "~/Bin/elisp/third-party/tramp"
       "~/Bin/elisp/third-party/semantic"
       "~/Bin/elisp/third-party/jde/lisp"
       ))

;(byte-recompile-directory (expand-file-name "~/Bin/elisp") 0 t)

(require 'vc-hooks)

;;; Load My Stuff
(load "setup-aliases")
(load "setup-keys")
(load "setup-mail-and-news")
(load "setup-misc")
(load "setup-modes")
(load "bs")

(custom-set-variables
 '(gutter-buffers-tab-visible-p nil)
 '(paren-mode (quote paren) nil (paren))
 '(gutter-visible-p nil)
 '(jde-make-program "gmake")
 '(jde-auto-parse-buffer-interval 60)
 '(delete-key-deletes-forward nil)
 '(column-number-mode t)
 '(lazy-lock-mode t nil (lazy-lock))
 '(c-echo-syntactic-information-p t)
 '(sql-postgres-options (quote ("-P pager=off")))
 '(browse-url-browser-function (quote browse-url-w3))
 '(inhibit-startup-message t t)
 '(cperl-help t)
 '(bar-cursor (quote (quote other)))
 '(jde-jdk-doc-url "file:///usr/share/doclib/java/index.html")
 '(eldoc-mode t)
 '(jde-gen-get-set-var-template (quote ("(end-of-line) '&" "(P \"Variable type: \" type) \" \"" "(P \"Variable name: \" name) \" ;\" '>'n '>'n" "\"/**\" '>'n" "\"* Get the value of \" (s name) \".\" '>'n" "\"* @return value of \" (s name) \".\" '>'n" "\"**/\" '>'n '>'n" " \"public \" (s type)" "(if (string= \"boolean\" (jde-gen-lookup-named 'type) ) " "\" is\" " "\" \" ) " "(s name)" "\"() \"" "(if jde-gen-k&r " "()" "'>'n)" "\"{\" '>'n" "\"return this.\" (s name) \";\" '>'n \"}\"" "'>'n '>'n" "\"/**\" '>'n" "\"* Set the value of \" (s name) \".\" '>'n" "\"* @param \" (s name) \" Value to assign to \" (s name) \".\" '>'n" "\"**/\" '>'n '>'n" "\"public void \" (s name)" "\"(\" (s type) \" \" (s name) \") \" " "(if jde-gen-k&r " "()" "'>'n)" "\"{\" '>'n" "'>'n \"this.\" (s name) \" = \" (s name) \";\" '>'n \"}\" '>'n'>")))
 '(shell-multiple-shells t)
 '(toolbar-visible-p nil)
 '(line-number-mode t)
 '(revert-without-query (quote ("\\.html")))
 '(query-user-mail-address nil)
 '(font-lock-mode t nil (font-lock))
 '(user-mail-address (concat "ryand@" (getenv "DOMAIN"))))

(custom-set-faces)

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
