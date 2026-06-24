;; -*- lexical-binding: t; -*-

(eval-and-compile
  (require 'which-key))

;;;###autoload
(defun rwd/which-key-show-buffer (act-popup-dim)
  (when (and which-key-preserve-window-configuration
             which-key--saved-window-configuration)
    (set-window-configuration which-key--saved-window-configuration)
    (setq which-key--saved-window-configuration nil))

    (which-key--debug-message "Displaying: %s" act-popup-dim)
  (display-buffer which-key--buffer))

;;;###autoload
(defun rwd/which-key-max-dimensions (_ignored)
  (which-key--side-window-max-dimensions))

;; DEBUGGING:
;; (setq which-key--debug-buffer-name "*which-key-debug*")
;; (setq which-key--debug-buffer-name nil)

;; USING:
;; (setq which-key-preserve-window-configuration t)
;; (setq which-key-popup-type 'custom)
;; (setq which-key-custom-show-popup-function #'rwd/which-key-show-buffer)
;; (setq which-key-custom-hide-popup-function #'which-key--hide-buffer-side-window)
;; (setq which-key-custom-popup-max-dimensions-function #'rwd/which-key-max-dimensions)
;; (setq which-key-max-display-columns 2)

;; RESET:
;; (setq which-key-popup-type 'side-window)
