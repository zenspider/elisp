;; https://github.com/mnp/dispwatch
(defun rwd/display-change (disp)
  (interactive)
  (let ((disp (dispwatch--get-display)))
    ;; TODO: adapt and fold these in
    ;; (setq split-height-threshold (* 2 60)
    ;;       split-width-threshold (* 2 80))
   (cond ((equal disp "1440x900")
          (rwd-set-mac-font "Menlo" 10)
          (rwd-split-smart))
         ;; ((equal disp "FUCK")
         ;;  (rwd-set-mac-font "Menlo" 16)
         ;;  (rwd-split-smart))
         (t (message "Unknown display dimensions: %s" disp)
            (find-file (expand-file-name "~/Bin/elisp/rwd-windows.el"))))))

(require 'dispwatch)
(add-hook 'dispwatch-display-change-hooks #'rwd/display-change)
(dispwatch-enable)
