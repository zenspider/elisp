(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)

(dolist (spec '(("\\.mab$"   . enh-ruby-mode)
                ("\\.rb$"    . enh-ruby-mode)
                ("Rakefile"  . enh-ruby-mode)
                ("Gemfile$"  . enh-ruby-mode)
                ("\\.rake$"  . enh-ruby-mode)))
  (add-to-list 'auto-mode-alist spec))

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(add-to-list 'load-path (expand-file-name "~/Work/git/zenspider/enhanced-ruby-mode"))
(add-to-list 'load-path (expand-file-name "~/Work/git/jcinnamond/seeing-is-believing"))

(when (require 'seeing-is-believing nil t)
  (defun zenspider/sib/erm+refresh ()
    (erm-reset-buffer))
  (add-hook 'seeing-is-believing-after-run-hooks
	    'zenspider/sib/erm+refresh))
