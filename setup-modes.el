(add-hook 'text-mode-hook '(lambda() (auto-fill-mode 1)))

(require 'compile)

(setq 
 default-major-mode 'text-mode
 compilation-error-regexp-systems-list '(gnu)
 )

(compilation-build-compilation-error-regexp-alist)

;;(turn-on-font-lock)
;;(turn-on-lazy-lock)
;;(setq-default font-lock-maximum-decoration t)

;; cc-mode setup, according to info pages
(fmakunbound 'c-mode)
(makunbound  'c-mode-map)
(fmakunbound 'c++-mode)
(makunbound  'c++-mode-map)
(makunbound  'c-style-alist)

(require 'cc-mode)

;;====================================================================
;; auto-insert

(require 'auto-insert-tkld)
(setq auto-insert-path '("/home/ryand/Bin/elisp/autoinsert")
      auto-insert-automatically t)

(pushnew '("\.java'" . "Java")
	 auto-insert-alist :test 'equal)
(pushnew '("Java" . "java-insert.java")
	 auto-insert-type-alist :test 'equal)
(pushnew '("\\.pm$" . "PerlModule")
	 auto-insert-alist :test 'equal)
(pushnew '("PerlModule" . "perl-insert.pm")
	 auto-insert-type-alist :test 'equal)

(global-set-key "\C-ci" 'insert-auto-insert-type)

;;====================================================================
;; imenu
;(defun my-imenu-add-to-menubar ()
;  (imenu-add-to-menubar "Index"))
;(add-hook 'emacs-lisp-mode-hook 'my-imenu-add-to-menubar)
;(add-hook 'cperl-mode-hook      'my-imenu-add-to-menubar)
;(add-hook 'c-mode-hook          'my-imenu-add-to-menubar)
;(add-hook 'c++-mode-hook        'my-imenu-add-to-menubar)
;(add-hook 'java-mode-hook       'my-imenu-add-to-menubar)

;(autoload 'imenu "imenu" "Goto buffer index position." t)
;(setq imenu-max-items 44)
;(setq imenu-sort-function 'imenu--sort-by-name)
;(define-key global-map [(shift button3)] 'imenu) ;; Or some other key

;; FIX
;(defvar c-hanging-braces-alist '((brace-list-open)
;                                 (substatement-open before)
;                                 ;(block-close . c-snug-do-while)
;                                 ;(extern-lang-open after)
;				 ))

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
