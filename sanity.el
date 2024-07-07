;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These functions and bindings keep ryan sane.

(keymap-global-set "C-x C-b" 'bs-show)
(keymap-global-set "M-s"     'fixup-whitespace)

(when window-system
  (keymap-global-unset "C-z")
  (setq ns-alternate-modifier    'none
        ns-command-modifier      'meta
        ns-pop-up-frames         nil
        ns-use-native-fullscreen nil))

;;;###autoload
(defun rwd-previous-line-6 ()
  (interactive)
  (previous-line 6))

;;;###autoload
(defun rwd-forward-line-6 ()
  (interactive)
  (forward-line 6))

;;;###autoload
(defun rwd-scroll-up ()
  (interactive)
  (scroll-down 1))

;;;###autoload
(defun rwd-scroll-down ()
  (interactive)
  (scroll-up 1))

;; compatibility:
(keymap-global-set "M-g"      'goto-line)
(keymap-global-set "C-<up>"   'rwd-previous-line-6)
(keymap-global-set "C-<down>" 'rwd-forward-line-6)
(keymap-global-set "M-<up>"   'rwd-scroll-up)
(keymap-global-set "M-<down>" 'rwd-scroll-down)

(provide 'sanity)
