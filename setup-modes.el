(require 'func-menu)

(require 'compile)

; (require 'jde) ; FIX: lost eieio, need to reinstall

(require 'p4)
(setq p4-strict-complete nil)
(p4-set-my-clients '(ryand ryand-itsy ryand-greed ryand-amzn ryand-amzn2))
(p4-toggle-vc-mode-off)
(p4-set-p4-port (concat "perforce." (getenv "DOMAIN") ":1666"))
(p4-set-client-name "ryand")

(if (file-exists-p (expand-file-name "~/Bin/p4"))
    (p4-set-p4-executable (expand-file-name "~/Bin/p4")))

(defun p4-amazon () (interactive) (p4-set-p4-port "perforce:6791"))
(defun p4-brazil () (interactive) (p4-set-p4-port "perforce:9591"))
(defun p4-normal () (interactive) (p4-set-p4-port "perforce:1666"))

(if (equal (getenv "DOMAIN") "amazon.com")
    (p4-amazon)
  (p4-normal))

(resize-minibuffer-mode 1)

;;; func-menu is a package that scans your source file for function
;;; definitions and makes a menubar entry that lets you jump to any
;;; particular function definition by selecting it from the menu.  The
;;; following code turns this on for all of the recognized languages.
;;; Scanning the buffer takes some time, but not much.
;;;
;;; Send bug reports, enhancements etc to:
;;; David Hughes <ukchugd@ukpmr.cs.philips.nl>

(cond (running-xemacs
       (require 'func-menu)
       ;(define-key global-map 'f8 'function-menu)
       (add-hook 'find-file-hooks 'fume-add-menubar-entry)
       (define-key global-map "\C-cl" 'fume-list-functions)      
       (define-key global-map "\C-cg" 'fume-prompt-function-goto) 
       (define-key global-map '(button3) 'mouse-function-menu)
       
       ;; For descriptions of the following user-customizable variables,
       ;; type C-h v <variable>
       (setq
	fume-fn-window-position 3
	fume-auto-position-popup t
	fume-display-in-modeline-p t
	fume-buffer-name "*Function List*"
	fume-no-prompt-on-valid-default nil)
       ))

;;====================================================================

; (require 'antlr-mode)
; (autoload 'antlr-set-tabs "antlr-mode")
; (add-hook 'java-mode-hook 'antlr-set-tabs)

(add-hook 'java-mode-hook 'my-java-mode-handler)
(defun my-java-mode-handler ()
  "Set C-c C-r to recompile in java mode"
  (local-set-key "\C-\c\C-r" 'recompile)
  )

 (setq auto-mode-alist
       (append
       '(
 	("\\.bash.*$"				. ksh-mode)
 	("^I?\\(M\\|m\\|GNUm\\)akefile.*$"	. makefile-mode)
; 	("\\.g$"				. antlr-mode)
 	) auto-mode-alist))

;; Perl support
(load "setup-perl")

;;============================================================
;; Taken from the comment section in inf-ruby.el


(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
	     (define-key ruby-mode-map "\M-\C-b" 'bury-buffer)
	     (setq ruby-program-name "/usr/local/bin/ruby")
             ))

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
          '(lambda ()
	     (define-key text-mode-map "\M-s" 'fixup-whitespace)
             ))

(add-hook 'c-mode-hook 'my-c-mode-handler)
(defun my-c-mode-handler ()
  "Set C-c C-r to recompile in c mode"
  (local-set-key "\C-\c\C-r" 'recompile)
  )

(require 'autorevert)
(turn-on-auto-revert-mode)
(global-auto-revert-mode)

;; Xrefactory configuration part ;;
;; some Xrefactory defaults can be set here
;(defvar xref-current-project nil);; can be also "my_project_name"
;(defvar xref-key-binding 'global);; can be also 'local or 'none
;(setq load-path (append load-path '("/usr/home/ryand/xref/xemacs")))
;(load "xrefactory")
;; end of Xrefactory configuration part ;;
