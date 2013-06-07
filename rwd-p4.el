;; ============================================================
;; Perforce:

;;;###autoload
(progn
  (require 'p4)
  (p4-set-client-config ".p4config")
  (p4-set-p4-executable (expand-file-name "~/Bin/p4")))
