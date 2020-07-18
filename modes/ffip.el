(defun rwd/ffip-create-shell-command (orig-fn &rest args)
  (concat (apply orig-fn args) "| sort"))

(advice-add 'ffip-create-shell-command :around 'rwd/ffip-create-shell-command)

(setq ffip-use-rust-fd t)
