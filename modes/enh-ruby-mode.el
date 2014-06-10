(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)

(dolist (spec '(("\\.mab$"   . enh-ruby-mode)
                ("\\.rb$"    . enh-ruby-mode)
                ("Rakefile"  . enh-ruby-mode)
                ("Gemfile"   . enh-ruby-mode)
                ("\\.rake$"  . enh-ruby-mode)))
  (add-to-list 'auto-mode-alist spec))

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(add-to-list 'load-path (expand-file-name "~/Work/git/enhanced-ruby-mode"))
(setq enh-ruby-program (expand-file-name "~/.multiruby/install/2.0.0-p195/bin/ruby"))
