(require 'setnu)

(add-hook 'text-mode-hook 'turn-on-auto-fill) ;;'(lambda() (auto-fill-mode 1)))
(add-hook 'text-mode-hook 'turn-on-setnu-mode)

(require 'compile)

(setq default-major-mode 'text-mode)

;; fixes compile regex coredump on DEC UNIX
(setq compilation-error-regexp-systems-list '(gnu))
(compilation-build-compilation-error-regexp-alist)

;; cc-mode setup, according to info pages
(fmakunbound 'c-mode)
(fmakunbound 'c++-mode)
(makunbound  'c-mode-map)
(makunbound  'c++-mode-map)
(makunbound  'c-style-alist)
(require 'cc-mode)

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

(setq auto-mode-alist
      (append
      '(
	("\\.[cCiIhH]$"				. c++-mode)
	("\\.cc$"				. c++-mode)
	("\\.[chi]xx$"				. c++-mode)
	("\\.bash.*$"				. ksh-mode)
	("^I?\\(M\\|m\\|GNUm\\)akefile.*$"	. makefile-mode)
	) auto-mode-alist))

;; Perl support
(load "setup-perl")
