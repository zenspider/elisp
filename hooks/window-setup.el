(when normal-gui-startup
  (rwd-resize-full)
  (rwd/display-change dispwatch-current-display))

(message "Emacs ready in %.2f seconds with %d garbage collections."
         (float-time (time-subtract after-init-time before-init-time))
         gcs-done)
