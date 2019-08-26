(when normal-gui-startup
  (rwd-lappy))

(message "Emacs ready in %.2f seconds with %d garbage collections."
         (float-time (time-subtract after-init-time before-init-time))
         gcs-done)

(when normal-gui-startup
  (rwd/display-reset))                  ; re-trigger dispwatch
