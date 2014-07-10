(defconst *dired-dirs*
  (mapcar
   (lambda (s) (cons s (file-truename (concat "~/Links/" s))))
   (directory-files "~/Links/" nil "^[A-Z]+$")))

(defconst *dired-aliases*
  (mapcar (lambda (e) (car e)) *dired-dirs*))

;;;###autoload
(defun rwd-dired-open-alias (&optional alias)
  (interactive)
  (unless alias
    (setq alias
          (completing-read "Alias: "
                           *dired-aliases*
                           nil t)))
  (if (and (stringp alias) (> (length alias) 0 ))
      (let ((pair (assoc alias *dired-dirs*)))
        (if pair
            (dired (cdr pair))
          (error "Invalid alias %s" alias)))
    (error "Invalid alias %s" alias)))
