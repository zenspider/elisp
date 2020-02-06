;;;###autoload
(defun rwd-autohooks ()
  (interactive)
  (message "START: rwd-autohooks")
  (dolist (path (directory-files (concat user-init-dir "hooks") t ".*el$"))
    (let* ((mode       (file-name-nondirectory (file-name-sans-extension path)))
           (hook-name  (intern (concat mode "-hook")))
           (raw-name   (concat "rwd-" mode "-hook"))
           (defun-name (intern raw-name))
           (lisp       (read-file-to-string path))
           (debug      nil))

      (eval (read (concat "(defun " raw-name " () "
                          (if debug
                              (format "%S" `(message "running %s" ,raw-name))
                            "")
                          lisp ")")))
      (and (functionp defun-name)
           (remove-hook hook-name defun-name))
      (message "add-hook %s %s" hook-name defun-name)
      (add-hook hook-name defun-name)))
  (message "DONE: rwd-autohooks"))
