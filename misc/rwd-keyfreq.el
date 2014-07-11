;;;###autoload
(progn
  (when (require 'keyfreq nil t)
    (setq keyfreq-file "~/.emacs.d/emacs.keyfreq")
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)))
