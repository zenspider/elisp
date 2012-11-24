;; ============================================================
;; Perforce:

;;;###autoload
(progn
  (require 'p4)
  (p4-set-p4config ".p4config")
  (p4-set-p4-executable "~/Bin/p4"))
