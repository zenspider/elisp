(when (and (boundp 'running-osx) running-osx (string= "/" default-directory))
  (cd "~"))

(global-whitespace-mode 1)

(rwd-load-modes)

(unless window-system
  (global-set-key (kbd "C-z C-z") 'suspend-frame)
  (global-set-key (kbd "C-Z") 'suspend-frame)) ; for shell-mode in terms
