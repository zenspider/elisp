(defun rwd/window-number-select (orig-fn number &rest args)
  (apply orig-fn (min (length (window-list nil nil)) number) args))

(advice-add 'window-number-select :around 'rwd/window-number-select)
;; (advice-remove 'window-number-select 'rwd/window-number-select)
