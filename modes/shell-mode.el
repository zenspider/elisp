(defun comint-scroll-to-bottom-on-output ()
  (interactive)
  ;; (set 'comint-scroll-to-bottom-on-output 'this)

  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t))
