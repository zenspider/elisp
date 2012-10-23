;;;###autoload
(dolist (spec '(("\\.bash.*$"                . ksh-mode)
                ("\\.js$"                    . ecmascript-mode)
                ("\\.haml$"                  . haml-mode)
                ("\\.rkt$"                   . scheme-mode)
                ("\\.coffee$"                . coffee-mode)
                ("Cakefile"                  . coffee-mode)
                ("^\\(GNUm\\|M\\)akefile.*$" . makefile-mode)
                ("\\.io$"                    . io-mode)
                ))
  (add-to-list 'auto-mode-alist spec))

;;;###autoload
(progn
  (rwd-install-package 'expand-region))
