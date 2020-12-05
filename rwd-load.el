(defmacro report-time (name &rest body)
  (declare (indent defun))
  `(let ((t1 (current-time))
         (result (progn ,@body)))
     (message "DONE: %-50S in %.2f sec" ,name (float-time (time-since t1)))
     result))

(defmacro when-idle (delay &rest body)
  (declare (indent defun))
  `(run-with-idle-timer ,delay nil (lambda () ,@body)))

(defun rwd-load (&rest args)
  (report-time `(rwd-load ,@args)
    (apply 'load args)))

(provide 'rwd-load)
