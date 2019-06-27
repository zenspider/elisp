(when (and (boundp 'running-osx) running-osx (string= "/" default-directory))
  (cd "~"))

(rwd-load-modes)

(when (eq 1 (length command-line-args))
  (server-start))
