;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These functions and bindings keep ryan sane.

(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "M-s")     'fixup-whitespace)
(when window-system
  (global-unset-key "\C-z")
  (setq ns-alternate-modifier (quote none))
  (setq ns-command-modifier (quote meta))
  (setq ns-pop-up-frames nil)
  (setq ns-use-native-fullscreen nil))

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
(global-set-key (kbd "M-g")      'goto-line)
(global-set-key (kbd "<C-up>")   'rwd-previous-line-6)
(global-set-key (kbd "<C-down>") 'rwd-forward-line-6)
(global-set-key (kbd "<M-up>")   'rwd-scroll-up)
(global-set-key (kbd "<M-down>") 'rwd-scroll-down)
