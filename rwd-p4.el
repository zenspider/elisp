;; ============================================================
;; Perforce:

;;;###autoload
(progn
  (when (require 'p4 nil t)
    (p4-set-p4-config ".p4config")
    (p4-set-p4-executable (expand-file-name "/usr/local/bin/p4"))))
