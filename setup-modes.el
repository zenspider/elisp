; (require 'setnu)
(require 'func-menu)

(require 'compile)
; (require 'jdok)
(require 'p4)
(require 'jde)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun p4-setup ()
  "Initializes p4"
  (interactive)
  (progn
    (load-library "p4")
    (p4-set-my-clients '(ryand))
    (p4-set-client-name "ryand")))

;; fixes compile regex coredump on DEC UNIX

(push '(java ("\\([^ \n	]+\\)\\([0-9]+\\):" 1 2)) 
	 compilation-error-regexp-alist-alist)


(setq compilation-error-regexp-systems-list '(gnu java))
(compilation-build-compilation-error-regexp-alist)

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

(require 'antlr-mode)
(autoload 'antlr-set-tabs "antlr-mode")
(add-hook 'java-mode-hook 'antlr-set-tabs)

(add-hook 'java-mode-hook 'my-java-mode-handler)
(defun my-java-mode-handler ()
  "Set C-c C-r to recompile in java mode"
  (local-set-key "\C-\c\C-r" 'recompile)
  )

 (setq auto-mode-alist
       (append
       '(
; 	("\\.[cCiIhH]$"				. cc-mode)
; 	("\\.cc$"				. cc-mode)
; 	("\\.[chi]xx$"				. cc-mode)
 	("\\.bash.*$"				. ksh-mode)
 	("^I?\\(M\\|m\\|GNUm\\)akefile.*$"	. makefile-mode)
 	("\\.g$"				. antlr-mode)
 	) auto-mode-alist))

;; Perl support
(load "setup-perl")

;;============================================================
;; Taken from the comment section in inf-ruby.el

(setq ruby-program-name "/usr/local/bin/ruby")

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
             ))

(autoload 'auto-revert-mode "autorevert" nil t)
(autoload 'turn-on-auto-revert-mode "autorevert" nil nil)
(autoload 'global-auto-revert-mode "autorevert" nil t)
