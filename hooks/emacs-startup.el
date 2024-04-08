(rwd-load-modes)

(when (and normal-gui-startup (not (getenv "INSIDE_EMACS")))
  (server-start))

(when-idle rwd-idle-time
  (setq file-name-handler-alist rwd-file-name-handler-alist) ; see early-init.el

  (setq gc-cons-percentage 0.2
        gc-cons-threshold 1600000))
