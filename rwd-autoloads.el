(eval-when-compile
  (require 'rwd-load)
  (require 'find-lisp))

(require 'autoload)                     ; = ;;;###autoload

(defun rwd-recompile-init ()
  (interactive)
  (message "recompiling %s" user-init-dir)
  (byte-recompile-directory (expand-file-name user-init-dir) 0))

(setq generated-autoload-file (concat user-init-dir "loaddefs.el"))


(defun rwd-autoloads-out-of-date-p (autoload-file)
  (report-time 'rwd-autoloads-out-of-date-p
    (or (not (file-exists-p autoload-file))
        (catch 'newer
          (dolist (file (find-lisp-find-files user-init-dir "\\.el$"))
            (when (file-newer-than-file-p file autoload-file)
              (message "NEWER! %s" file)
              (throw 'newer file)))))))

(defun rwd-generate-autoloads (autoload-file)
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
    (when (rwd-autoloads-out-of-date-p generated-autoload-file)
      (rwd-generate-autoloads generated-autoload-file))
    (rwd-load generated-autoload-file)))

(provide 'rwd-autoloads)
