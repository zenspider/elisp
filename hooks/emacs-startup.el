(rwd-load-modes)

(when (and normal-gui-startup (not (getenv "INSIDE_EMACS")))
  (server-start))
