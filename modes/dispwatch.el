(defvar rwd/displays
  `(("1440x900"  ,(* 1 80) ,(* 2 80) "Menlo" 14)
    ("1792x1120" ,(* 1 80) ,(* 2 80) "Menlo" 16)
    ("4000x1440" ,(* 2 60) ,(* 2 80) "Menlo" 16))
  "Contains a list of display entries for emacs' dispwatch")

(defun rwd/display-setup (h w f s)
  (setq split-height-threshold h
        split-width-threshold w)
  (rwd-set-mac-font f s)
  (message "rwd/display-setup %s %s %s %s" h w f s))

(defun rwd/display-change (disp)
  (interactive)
  (message "rwd/display-change %s" disp)
  (let ((args (assoc disp rwd/displays)))
    (unless args
      (find-variable 'rwd/displays)
      (error "Please extend rwd/displays with %S" disp))
    (apply 'rwd/display-setup (cdr args))
    (rwd-fix-fullscreen)
    (sleep-for 0.25)
    (rwd-split-smart)))

;; https://github.com/mnp/dispwatch
(require 'dispwatch)
(add-hook 'dispwatch-display-change-hooks 'rwd/display-change)
(dispwatch-enable)

(defalias 'rwd/display-reset 'dispwatch-reset)
