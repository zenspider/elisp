(eval-when-compile
  (require 'dash))

(defun rwd/find-file/numbers (orig-fun filename &rest args)
  (-let* (((path . pos) (rwd/parse-path-with-pos filename)))
    (apply orig-fun path args)
    (when pos
      (-let (((line . col) pos))
        (and line (goto-line line))
        (and col  (move-to-column (max col 0)))))))

(advice-add 'find-file :around #'rwd/find-file/numbers)
;; (advice-remove 'find-file #'rwd/find-file/numbers)
