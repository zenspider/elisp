
;;;###autoload
(progn
  (autoload 'etags-select-find-tag          "etags-select")
  (autoload 'etags-select-find-tag-at-point "etags-select")
  (autoload 'magit-status                   "magit"            nil t)
  (autoload 'run-scheme                     "cmuscheme"        nil t)
  (autoload 'scheme-smart-complete          "scheme-complete"  nil t)
  (autoload 'scheme-complete-or-indent      "scheme-complete"  nil t)
  (autoload 'scheme-get-current-symbol-info "scheme-complete"  nil t)
  (autoload 'ssh                            "ssh"              nil t)
  (autoload 'kill-ring-search               "kill-ring-search" nil t))
