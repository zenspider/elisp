(progn
  (autoload 'autotest                        "autotest"        "doco" t)
  (autoload 'autotest-switch                 "autotest"        "doco" t)
  (autoload 'haml-mode                       "haml-mode"       "doco" t)
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

(dolist (spec '(("\\.rhtml$" . html-mode)
                ("\\.xhtml$" . html-mode)
                ("\\.yml$"   . yaml-mode)
                ("\\.gem$"   . tar-mode)))
  (add-to-list 'auto-mode-alist spec))

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(setq ruby-program-name "/usr/bin/irb")
