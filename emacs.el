; Stupid compatibility checking
(unless (featurep 'xemacs)

  (message "Have I ever mentioned that GNU emacs sucks?")
  (defun add-to-list (list-var element)
    "Stolen from xemac's subr.el"
    (or (member element (symbol-value list-var))
	(set list-var (cons element (symbol-value list-var))))) 
  (defalias 'mapc 'mapcar)
  (load-library "cl")
  (eval "(setq apropos-do-all t)") ; FIX not sure this works
  (defalias 'Info-directory-list 'Info-default-directory-list)
  )

; REFACTOR: generalize both lambdas into pushnew-string=(list, ...)
(if (featurep 'xemacs)
    ;; Extend Info directories
    (map nil (function (lambda (path)
			 (pushnew path Info-directory-list :test 'string=)))
	 (list 
	  "/usr/share/info"
	  (expand-file-name "~/Bin/elisp/third-party/tramp")
	  ))
  )

; FIX: function should use something other that pushnew for GNU emacs compat.
(map nil (function (lambda (path)
		     (pushnew (expand-file-name path)
			      load-path :test 'string=)))
     (list
      "/usr/local/lib/xemacs/site-lisp"
      "~/Bin/elisp/"
      "~/Bin/elisp/third-party/"
      "~/Bin/elisp/third-party/tramp"
      "~/Bin/elisp/third-party/semantic"
      "~/Bin/elisp/third-party/jde/lisp"
      ))

;; Extend Info directories
(setq Info-directory-list (cons "/usr/share/info" Info-directory-list))

; in the case of loading from a tty, I'm assuming that we are looking
; for as much speed as possible as our lifetime is probably
; short-lived.

(cond 
 ((eq 'tty (device-type))
  (setq delete-key-deletes-forward nil)
  (message "Console setup loaded")
  )
 ((eq 'x (device-type))

; Compile everything in elisp, but not recursively, as that seems to
; crash a lot.
  (byte-recompile-directory (expand-file-name "~/Bin/elisp") 0 t)
  
  (custom-set-variables
; '(bar-cursor (quote (quote other)))
; '(browse-url-browser-function (quote browse-url-w3))
   '(c-echo-syntactic-information-p t)
   '(column-number-mode t)
; '(cperl-help t)
   '(delete-key-deletes-forward nil)
   '(eldoc-mode t)
   '(font-lock-mode t nil (font-lock))
; '(gutter-buffers-tab-visible-p nil)
; '(gutter-visible-p nil)
   '(inhibit-startup-message t t)
; '(jde-auto-parse-buffer-interval 60)
; '(jde-make-program "gmake")
; '(lazy-lock-mode t nil (lazy-lock))
   '(line-number-mode t)
   '(paren-mode (quote paren) nil (paren))
   '(query-user-mail-address nil)
; '(revert-without-query (quote ("\\.html")))
   '(shell-multiple-shells t)
; '(sql-postgres-options (quote ("-P pager=off")))
; '(toolbar-visible-p nil)
   '(user-mail-address (concat "ryand@" (getenv "DOMAIN")))
   )
  
  (custom-set-faces);
  
  (load "setup-aliases")
  (load "setup-keys")
  (load "setup-mail-and-news")
  (load "setup-misc")
  (load "setup-modes")
  (require 'bs)
  (require 'vc-hooks)
  )
 (t (message "Unknown device-type, no extra initialization"))
 )
