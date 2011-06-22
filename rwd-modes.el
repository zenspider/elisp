;; ============================================================
;; Requiresx:


;;;###autoload
(setq my-usual-programming-modes
      '(ruby-mode lisp-mode scheme-mode emacs-lisp-mode))

;;;###autoload
(dolist (spec '(("\\.bash.*$"                . ksh-mode)
                ("\\.js$"                    . ecmascript-mode)
                ("\\.haml$"                  . haml-mode)
                ("\\.rkt$"                   . scheme-mode)
                ("^\\(GNUm\\|M\\)akefile.*$" . makefile-mode)))
  (add-to-list 'auto-mode-alist spec))

;; ============================================================
;; Misc Modes/Stuff:
