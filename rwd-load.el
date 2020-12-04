(defun rwd-load (&rest args)
  (let ((t1 (current-time)))
    (apply 'load args)
    (message "DONE: %-50S in %.2f sec" `(rwd-load ,@args) (float-time (time-since t1)))))

(provide 'rwd-load)
