;;; Add my elisp directory to pathing
(pushnew (expand-file-name "~/Bin/elisp/")
	 load-path :test 'string=)
(pushnew (expand-file-name "~/Bin/elisp/third-party/")
	 load-path :test 'string=)
(pushnew (expand-file-name "~/Bin/elisp/third-party/semantic")
	 load-path :test 'string=)
(pushnew (expand-file-name "~/Bin/elisp/third-party/jde/lisp")
	 load-path :test 'string=)

(byte-recompile-directory (expand-file-name "~/Bin/elisp") 0 t)
; (byte-recompile-directory (expand-file-name "~/Bin/elisp") 0)

(require 'vc-hooks)
;(require 'project)

;;; Load My Stuff
(load "setup-aliases")
(load "setup-keys")
(load "setup-mail-and-news")
(load "setup-misc")
(load "setup-modes")
(load "bs")

(custom-set-variables
 '(gutter-buffers-tab-visible-p nil)
 '(backup-by-copying-when-linked t)
 '(paren-mode (quote paren) nil (paren))
 '(gutter-visible-p nil)
 '(jde-make-program "gmake")
 '(jde-auto-parse-buffer-interval 60)
 '(delete-key-deletes-forward nil)
 '(lazy-shot-mode t nil (lazy-shot))
 '(c-echo-syntactic-information-p t)
 '(browse-url-browser-function (quote browse-url-w3))
 '(inhibit-startup-message t t)
 '(cperl-help t)
 '(bar-cursor (quote (quote other)))
 '(jde-jdk-doc-url "file:///usr/share/doclib/java/index.html")
 '(eldoc-mode t)
 '(jde-gen-get-set-var-template (quote ("(end-of-line) '&" "(P \"Variable type: \" type) \" \"" "(P \"Variable name: \" name) \";\" '>'n '>'n" "\"/**\" '>'n" "\"* Get the value of \" (s name) \".\" '>'n" "\"* @return value of \" (s name) \".\" '>'n" "\"**/\" '>'n '>'n" " \"public \" (s type)" "(if (string= \"boolean\" (jde-gen-lookup-named 'type) ) " "\" is\" " "\" \" ) " "(s name)" "\"() \"" "(if jde-gen-k&r " "()" "'>'n)" "\"{\" '>'n" "\"return this.\" (s name) \";\" '>'n \"}\"" "'>'n '>'n" "\"/**\" '>'n" "\"* Set the value of \" (s name) \".\" '>'n" "\"* @param \" (s name) \" Value to assign to \" (s name) \".\" '>'n" "\"**/\" '>'n '>'n" "\"public void \" (s name)" "\"(\" (s type) \" \" (s name) \") \" " "(if jde-gen-k&r " "()" "'>'n)" "\"{\" '>'n" "'>'n \"this.\" (s name) \" = \" (s name) \";\" '>'n \"}\" '>'n'>")))
 '(shell-multiple-shells t)
 '(toolbar-visible-p nil)
 '(jde-build-use-make t)
 '(time-stamp-format "%02y-%02m-%02d %02H:%02M:%02S %u" t)
 '(line-number-mode t)
 '(calendar-time-display-form (quote (24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))))
 '(cperl-hairy t)
 '(user-mail-address "ryand@amazon.com" t)
 '(query-user-mail-address nil))
(custom-set-faces
 '(default ((t (:foreground "black" :background "white"))) t)
 '(dired-face-symlink ((((class color)) (:italic t)))))

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
