;;;###autoload
(defun rwd-autohooks ()
  (dolist (path (directory-files user-hooks-dir t ".*el$"))
    (let* ((mode          (file-name-nondirectory (file-name-sans-extension path)))
           (hook-name     (intern (concat mode "-hook")))
           (hook-path     (concat "hooks/" mode ".el"))
           (lisp          (read-file-to-string path)))
      (message "*** defining autohook: %s" hook-name)
      (add-hook hook-name
                `(lambda ()
                   (message "**** loading %s hook" ,mode)
                   (eval (read (concat "(progn " ,lisp ")"))))))))
