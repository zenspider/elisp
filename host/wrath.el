(defun rwd/after-init/lappy ()
  (when (eq 1 (length command-line-args))
    (rwd-lappy)))

(when window-system
 (add-hook 'after-init-hook 'rwd/after-init/lappy t))
