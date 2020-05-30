;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       1         2         3         4         5         6         7         8
;;345678901234567890123456789012345678901234567890123456789012345678901234567890


(defvar rwd/displays nil
  "Contains a list of display entries for emacs' dispwatch")

(setq rwd/displays
  `(("default" ,(* 1 80) ,(* 2 80) "Fira Code" 14) ; basics

    ("1440x900"  ,(* 1 80) ,(* 2 80) "Fira Code" 14) ; normal 13" lappy
    ("1792x1120" ,(* 2 80) ,(* 2 80) "Fira Code" 16) ; mpb 16" at regular
    ("2048x1280" ,(* 2 60) ,(* 2 80) "Fira Code" 16) ; mbp 16" at densest

    ;; pairing MPB 16" w/ HP 27Z @ 60hz
    ("1280x720"  400 160 "Fira Code"  9) ; largest  text
    ("1920x1080" 400 160 "Fira Code" 12) ; larger   text
    ("2560x1440" 400 160 "Fira Code" 15) ; medium   text
    ("3200x1800" 400 160 "Fira Code" 18) ; smaller  text
    ("3840x2160" 400 160 "Fira Code" 24) ; smallest text (*)
    ("4000x1440" ,(* 2 60) ,(* 2 80) "Fira Code" 16) ; chef POS (dell?) monitor
    ))

;; (rwd/display-reset)

(defun rwd/display-setup (h w f s)
  (setq split-height-threshold h
        split-width-threshold w)
  (rwd-set-mac-font f s)
  (message "rwd/display-setup %s %s %s %s" h w f s))

(defun rwd/display-change (_disp)       ; ignore this incoming var
  (interactive)
  (when window-system
    (let* ((attr (frame-monitor-attributes))
           (geom (assoc 'geometry attr))
           (w (nth 3 geom))
           (h (nth 4 geom))
           (disp (format "%sx%s" w h))  ; calculated from actual geometry
           (args (assoc disp rwd/displays))
           (default (assoc "default" rwd/displays)))
      (message "rwd/display-change %s" disp)
      (unless args
        (find-variable 'rwd/displays)
        (insert (format "%S\n" (list disp 'w 'h "Font" 16)))
        (message "Please extend rwd/displays with %S" disp)
        (setq args default))
      (apply 'rwd/display-setup (cdr args))
      (rwd-fix-fullscreen)
      (sleep-for 0.25)
      (rwd-split-smart))))

;; https://github.com/mnp/dispwatch
(require 'dispwatch)

(when window-system
  ;; (remove-hook 'dispwatch-display-change-hooks 'rwd/display-change)
  (add-hook 'dispwatch-display-change-hooks 'rwd/display-change)
  (dispwatch-mode 1))

(defun rwd/display-reset ()
  "Reset and force dispwatch to trigger again."
  (interactive)
  (setq dispwatch-current-display nil))
