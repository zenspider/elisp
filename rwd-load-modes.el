(defun rwd-load-modes ()
  (require 'subr-x)
  (report-time 'rwd-load-modes
    (let* ((default-directory user-init-dir)
           (paths (file-expand-wildcards "./modes/*.el"))
           (clean (lambda (s) (string-remove-suffix ".el" s)))
           (idle-time (if normal-gui-startup 1 0))
           (names (mapcar clean paths)))
      (dolist (name (reverse names))
        (rwd-load name nil t)))))

(provide 'rwd-load-modes)
