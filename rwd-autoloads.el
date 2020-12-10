(eval-when-compile
  (require 'rwd-load))

(require 'autoload)                     ; = ;;;###autoload

(defun rwd-recompile-init ()
  (interactive)
  (report-time 'rwd-recompile-init
    (byte-recompile-directory (expand-file-name user-init-dir) 0)))

(setq autoload-file (concat user-init-dir "loaddefs.el"))

(require 'find-lisp)

(defun rwd-autoloads-out-of-date-p ()
  (report-time 'rwd-autoloads-out-of-date-p
    (or (not (file-exists-p autoload-file))
        (catch 'newer
          (dolist (file (find-lisp-find-files user-init-dir "\\.el$"))
            (unless (equal file autoload-file)
              (let ((elc-file (byte-compile-dest-file file)))
                (when (or (file-newer-than-file-p file autoload-file)
                          (not (file-exists-p elc-file))
                          (file-newer-than-file-p file elc-file))
                  (message "NEWER! %s" file)
                  (throw 'newer file)))))))))

(defun rwd-generate-autoloads ()
  (report-time 'rwd-generate-autoloads
    (let ((generated-autoload-file autoload-file)
          (el-root-subdirs (find-lisp-find-files-internal
                            user-init-dir
                            'find-lisp-file-predicate-is-directory
                            'find-lisp-default-directory-predicate)))
      (apply 'update-directory-autoloads (cons user-init-dir el-root-subdirs))
      (rwd-recompile-init)
      (set-file-times autoload-file))))

(defun rwd-autoloads ()
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive)
  (report-time 'rwd-autoloads
    (when (rwd-autoloads-out-of-date-p)
      (rwd-generate-autoloads))
    (rwd-load autoload-file)))

(provide 'rwd-autoloads)
