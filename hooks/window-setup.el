(when window-system
  (rwd-require 'modes/dispwatch "modes/dispwatch.el")

  (add-hook 'dispwatch-display-change-hooks 'rwd/dispwatch-display-change-hook)
  ;; (remove-hook 'dispwatch-display-change-hooks 'rwd/display-change)

  (dispwatch-mode 1)
  (rwd/display-reset))

(message "Emacs ready in %.2f seconds with %d garbage collections."
         (float-time (time-subtract after-init-time before-init-time))
         gcs-done)
