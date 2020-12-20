(when-idle rwd-idle-time
  (windmove-default-keybindings)
  (winner-mode 1)
  (rwd-require 'window-number))

(with-eval-after-load 'window-number
  (window-number-meta-mode 1))
