(with-eval-after-load 'p4
  (p4-set-p4-config ".p4config")

  (let* ((paths '("/usr/local/bin/p4" "/opt/perforce/bin/p4"))
         (paths (mapcar   #'expand-file-name paths))
         (found (seq-find #'file-exists-p    paths)))
    (p4-set-p4-executable found))

  (define-key p4-prefix-map (kbd "A") 'p4-diff-all-opened))

(when-idle rwd-idle-time
  (rwd-require 'p4 nil t))
