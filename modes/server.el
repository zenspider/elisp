(defun rwd/server-visit-files (orig-fn files &rest args)
  (let ((new-files (mapcar (-lambda ((path pos))
                             (if pos (cons path pos)
                               (rwd/parse-path-with-pos path)))
                           files)))
    (apply orig-fn new-files args)))

(advice-add 'server-visit-files :around #'rwd/server-visit-files)
;; (advice-remove 'server-visit-files #'rwd/server-visit-files)
