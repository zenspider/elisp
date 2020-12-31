(defun rwd/require/handler (orig-fun &rest args)
  (let ((file-name-handler-alist nil))
    (apply orig-fun args)))
(advice-add 'require :around #'rwd/require/handler)
;; (advice-remove 'require 'rwd/require/handler)

(defvar normal-startup
  (and (not noninteractive)
       (not (cdr command-line-args))))

(defvar normal-gui-startup
  (and window-system
       normal-startup)
  "t if this is a plain GUI emacs startup (eg not batch nor task oriented).")

(defconst rwd-idle-time (if normal-gui-startup 1 0))

(defmacro report-time (name &rest body)
  (declare (indent defun))
  `(let ((t1 (current-time))
         (float-output-format nil)
         (result (progn ,@body)))
     (message "DONE: %-50S in %.3f sec" ,name (float-time (time-since t1)))
     result))

(defmacro when-idle (delay &rest body)
  (declare (indent defun))
  `(run-with-idle-timer ,delay nil (lambda () ,@body)))

(defun rwd-load (&rest args)
  (report-time `(rwd-load ,@args)
    (apply 'load args)))

(defun rwd-require (&rest args)
  (report-time `(rwd-require ,@args)
    (apply 'require args)))

(defun rwd-packages-up-to-date ()
  (and
   (file-readable-p package-quickstart-file)
   (file-directory-p package-user-dir)
   (file-newer-than-file-p package-quickstart-file
                           package-user-dir)
   (file-newer-than-file-p package-quickstart-file
                           (concat user-init-dir "rwd-packages.el"))))

(provide 'rwd-load)
