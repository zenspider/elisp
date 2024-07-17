(unless window-system
  (keymap-global-unset "C-z")
  (keymap-global-set (kbd "C-z C-z") 'suspend-frame)
  (keymap-global-set (kbd "C-Z") 'suspend-frame)) ; for shell-mode in terms
