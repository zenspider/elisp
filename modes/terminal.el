;; -*- lexical-binding: t; -*-

(unless window-system
  (keymap-global-unset "C-z")
  (keymap-global-set (kbd "C-z C-z") 'suspend-frame)
  (keymap-global-set (kbd "C-S-z") 'suspend-frame)) ; for shell-mode in terms:
