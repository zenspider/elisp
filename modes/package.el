(add-to-list 'load-path
             (expand-file-name "~/Work/git/zenspider/package+"))

(when-idle rwd-idle-time
  (package+-disable-package-selected-packages))
