(unless window-system
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z C-z") 'suspend-frame)
  (global-set-key (kbd "C-Z") 'suspend-frame)) ; for shell-mode in terms
