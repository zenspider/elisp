(when (and running-osx (string= "/" default-directory))
  (cd "~"))

(global-whitespace-mode 1)

(rwd-load-modes)
