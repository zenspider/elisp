;;;###autoload
(dolist (pair '((?h "~/Work/p4/zss/src/hoe/dev/lib/hoe.rb")
                (?w "~/Work/p4/zss/usr/ryand/superslow.txt")
                (?m "~/Work/p4/zss/src/minitest/dev/lib/minitest.rb")))
  ;; To jump to a register, use C-x r j followed by the letter of the register.
  (set-register (car pair) `(file . ,(cadr pair))))
