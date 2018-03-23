;;;###autoload
(dolist (pair '((?e "~/Bin/elisp/emacs.el")
                (?o "~/Bin/elisp/hooks")
                (?h "~/Work/p4/zss/src/hoe/dev/lib/hoe.rb")
                (?a "~/Bin/elisp/rwd-aliases.el")
                (?k "~/Bin/elisp/rwd-keys.el")
                (?m "~/Work/p4/zss/src/minitest/dev/lib/minitest.rb") ;
                (?s "~/Desktop/steak.org")
                (?p "~/Bin/elisp/rwd-packages.el")
                (?w "~/Work/p4/zss/usr/ryand/superslow.txt")))
  ;; To jump to a register, use C-x r j followed by the letter of the register.
  (set-register (car pair) `(file . ,(cadr pair))))
