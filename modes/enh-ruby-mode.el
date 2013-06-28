(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)

(add-to-list 'auto-mode-alist        '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby"   . enh-ruby-mode))

(add-to-list 'load-path (expand-file-name "~/Work/git/enhanced-ruby-mode"))
(setq enh-ruby-program (expand-file-name "~/.multiruby/install/2.0.0-p195/bin/ruby"))
