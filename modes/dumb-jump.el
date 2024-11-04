(when-idle rwd-idle-time
  (add-hook 'xref-backend-functions #'dumber-jump-xref-activate))
