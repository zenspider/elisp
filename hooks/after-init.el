(when (and (boundp 'running-osx) running-osx (string= "/" default-directory))
  (cd "~"))

(rwd-load-modes)
