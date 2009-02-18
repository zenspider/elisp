
;;;###autoload
(progn
  (autoload 'autotest                        "autotest"        "doco" t)
  (autoload 'autotest-switch                 "autotest"        "doco" t)
  (autoload 'blank-mode                      "blank-mode"      "doco" t) ; TODO move
  (autoload 'inf-ruby-keys                   "inf-ruby"        "doco"  )
  (autoload 'racc-mode                       "racc-mode"       "doco" t)
  (autoload 'rcov-buffer                     "rcov-overlay.el" "doco" t)
  (autoload 'ri                              "ri.el"           "doco" t)
  (autoload 'ri-show-term-at-point           "ri.el"           "doco" t)
  (autoload 'ri-show-term-composite-at-point "ri.el"           "doco" t)
  (autoload 'ruby-index                      "ri.el"           "doco" t)
  (autoload 'ruby-mode                       "ruby-mode"       "doco" t)
  (autoload 'run-ruby                        "inf-ruby"        "doco" t)
  (autoload 'yaml-mode                       "yaml-mode"       "doco" t))

;;;###autoload
(dolist (spec '(("\\.mab$"   . ruby-mode)
                ("\\.rb$"    . ruby-mode)
                ("Rakefile"  . ruby-mode)
                ("\\.rake$"  . ruby-mode)
                ("\\.rhtml$" . html-mode)
                ("\\.xhtml$" . html-mode)
                ("\\.yml$"   . yaml-mode)
                ("\\.gem$"   . tar-mode)))
  (add-to-list 'auto-mode-alist spec))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;;;###autoload
(setq ruby-program-name "/usr/bin/irb")

;;;###autoload
(defun ruby-run-buffer-clean ()
  "Send the entire buffer to the inferior Ruby process.
Then switch to the process buffer."
  (interactive)
  (require 'inf-ruby)
  (save-excursion
    (if (and ruby-buffer (get-buffer ruby-buffer))
        ;; (delete-process ruby-buffer)
        (kill-buffer ruby-buffer))
    (if (get-process "ruby")
        (delete-process (get-process "ruby")))
    (run-ruby ruby-program-name)
    (make-local-variable 'inferior-ruby-first-prompt-pattern)
    (make-local-variable 'inferior-ruby-prompt-pattern)
    (setq inferior-ruby-first-prompt-pattern ">>")
    (setq inferior-ruby-prompt-pattern       "\\?>"))
  (ruby-send-region-and-go (point-min) (point-max)))

;;;###autoload
(hook-after-load-new ruby-mode nil
  (inf-ruby-keys)
  (define-key ruby-mode-map (kbd "C-c C-a") 'autotest-switch)
  (define-key ruby-mode-map (kbd "C-c C-r") 'rcov-buffer)
  (define-key ruby-mode-map (kbd "C-c C-b") 'ruby-run-buffer-clean)
  (define-key ruby-mode-map (kbd "C-c C-t") 'ri-show-term-composite-at-point)

  ;; TODO: fix this to only be the which-func-modes stuff. preferably elsewhere
  (require 'which-func)
  (add-to-list 'which-func-modes 'ruby-mode)
  (which-func-mode 1)
  
  (imenu-add-menubar-index)
  (blank-mode))
