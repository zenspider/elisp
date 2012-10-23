;; ============================================================
;; Perforce:

;;;###autoload
(progn
  (require 'p4)
  (p4-set-my-clients '(ryand ryand-itsy ryand-greed ryand-amzn ryand-amzn2))
  (p4-toggle-vc-mode-off)
  (p4-set-client-config ".p4config")
  (p4-set-client-name "ryand")
  (p4-set-p4-executable "~/Bin/p4"))

;; TODO:
;; (require 'p4-lowlevel)
;; (require 'vc-p4)

;;;###autoload
(defun p4-normal ()
  (interactive)
  (p4-set-p4-port "perforce:1666"))

