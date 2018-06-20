;; (setq tramp-verbose 9)
(require 'tramp)
(require 'tramp-sh)
(setq tramp-ssh-controlmaster-options
      "-o ControlPath=~/.ssh/tramp.%%C -o ControlMaster=auto -o ControlPersist=no")
