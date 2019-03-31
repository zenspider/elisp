(remove-hook 'erc-text-matched-hook 'erc-hide-fools)
(easy-menu-add-item  nil '("tools") ["IRC" erc-select t])

(defun rwd/erc-track-switch-buffer+perspective (&rest args)
  (cond (erc-modified-channels-alist
         (persp-switch "i"))))

(advice-add 'erc-track-switch-buffer
            :before #'rwd/erc-track-switch-buffer+perspective)
