;;;###autoload
(dolist (spec '(("\\.bash.*$"                . ksh-mode)
                ("\\.js$"                    . ecmascript-mode)
                ("\\.haml$"                  . haml-mode)
                ("\\.rkt$"                   . scheme-mode)
                ("\\.coffee$"                . coffee-mode)
                ("Cakefile"                  . coffee-mode)
                ("^\\(GNUm\\|M\\)akefile.*$" . makefile-mode)))
  (add-to-list 'auto-mode-alist spec))

;;;###autoload
(progn
  (add-to-list 'load-path (expand-file-name "~/Work/git/enhanced-ruby-mode"))
  (setq enh-ruby-program (expand-file-name "~/.multiruby/install/1.9.3-p194/bin/ruby"))

  (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
  (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

  (rwd-install-package 'expand-region))
