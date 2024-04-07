(when-idle rwd-idle-time
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
