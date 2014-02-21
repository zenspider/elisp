;;;###autoload
(defun rwd-find-file-up (file-name &optional dir)
 (let ((f (expand-file-name file-name (or dir default-directory)))
       (parent (file-truename (expand-file-name ".." dir))))
   (cond ((string= dir parent) nil)
         ((file-exists-p f) f)
         (t (rwd-find-file-up file-name parent)))))

;;;###autoload
(defun rwd-tag-file-list ()
  (let ((dir (rwd-find-file-up "TAGS")))
    (if dir (list dir) nil)))
