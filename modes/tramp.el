(eval-when-compile
  (require 'tramp)
  (require 'tramp-sh))

(with-eval-after-load 'tramp
  ;; (setq tramp-verbose 9)
  (setq tramp-ssh-controlmaster-options
        "-o ControlPath=~/.ssh/tramp.%%C -o ControlMaster=auto -o ControlPersist=no"))
