(defun rwd-recompile-init ()
  (interactive)
  (byte-recompile-directory (expand-file-name user-init-dir) 0))

(setq generated-autoload-file "loaddefs.el") ;; HACK for emacs 24.1

(autoload 'find-lisp-find-files          "find-lisp" nil t)
(autoload 'find-lisp-find-files-internal "find-lisp" nil t)

(defun rwd-autoloads ()
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive)
  (let* ((autoload-file (concat user-init-dir generated-autoload-file)))
    (if (or (not (file-exists-p autoload-file))
            (catch 'newer
              (dolist (file (find-lisp-find-files user-init-dir "\\.el$"))
                (if (file-newer-than-file-p file autoload-file)
                    (throw 'newer file)))))
        (let ((generated-autoload-file autoload-file)
              (el-root-subdirs (find-lisp-find-files-internal
                                user-init-dir
                                'find-lisp-file-predicate-is-directory
                                'find-lisp-default-directory-predicate)))
          (apply 'update-directory-autoloads (cons user-init-dir el-root-subdirs))
          (rwd-recompile-init)
          (load autoload-file) ; helps rwd-recompile-init dependencies

          ))
    (message "loading autoloads")
    (load autoload-file)
    (message "done loading autoloads")))
