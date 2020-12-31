(eval-when-compile
  (require 'bytecomp)
  (require 'autoload)                     ; = ;;;###autoload
  (require 'rwd-load))

(defun rwd-recompile-init ()
  (interactive)
  (report-time 'rwd-recompile-init
    (byte-recompile-directory (expand-file-name user-init-dir) 0)))

(setq autoload-file (concat user-init-dir "loaddefs.el"))

(defun rwd-autoloads-out-of-date-p ()
  (report-time 'rwd-autoloads-out-of-date-p
    (or (not (file-exists-p autoload-file))
        (catch 'newer
          (dolist (file (directory-files-recursively user-init-dir "\\.el$"))
            (unless (equal file autoload-file)
              (let ((elc-file (byte-compile-dest-file file)))
                (when (or (file-newer-than-file-p file autoload-file)
                          (not (file-exists-p elc-file))
                          (file-newer-than-file-p file elc-file))
                  (message "NEWER! %s" file)
                  (throw 'newer file)))))))))

(defun rwd-generate-autoloads ()
  (interactive)
  (report-time 'rwd-generate-autoloads
    (let ((generated-autoload-file autoload-file)
          (el-root-subdirs (seq-filter
                            #'file-directory-p
                            (directory-files-recursively user-init-dir "." t))))
      (apply 'update-directory-autoloads (cons user-init-dir el-root-subdirs))
      (rwd-recompile-init)
      (set-file-times autoload-file))))

(defun rwd-autoloads (&optional force)
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive "p")
  (report-time 'rwd-autoloads
    (when (or force (rwd-autoloads-out-of-date-p))
      (rwd-generate-autoloads)
      (rwd-recompile-init))
    (rwd-load autoload-file)))

(defun rwd-optimize ()
  (interactive)
  (rwd-autoloads t))

(provide 'rwd-autoloads)
