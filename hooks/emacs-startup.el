(rwd-load-modes)                        ; delayed loading

(rwd-load "./modes/persp-mode" nil t)   ; force early load

(when (and normal-gui-startup (not (getenv "INSIDE_EMACS")))
  (message "Starting SERVER")
  (server-start))
