;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       1         2         3         4         5         6         7         8
;;345678901234567890123456789012345678901234567890123456789012345678901234567890

(defvar rwd/displays nil
  "Contains a list of display entries for emacs' dispwatch")

(setq rwd/displays
  `(("default"   ,(* 1 80) ,(* 2 80) "Fira Code" 14) ; fallback

    ("1440x900"  ,(* 1 80) ,(* 2 80) "Fira Code" 14) ; normal 13" lappy

    ;; MBP 16"
    ("1152x720"  ,(* 2 80) ,(* 2 80) "Fira Code" 10) ; largest  text
    ("1344x840"  ,(* 2 80) ,(* 2 80) "Fira Code" 13) ; larger   text
    ("1536x960"  ,(* 2 80) ,(* 2 80) "Fira Code" 15) ; medium   text
    ("1792x1120" ,(* 2 80) ,(* 2 80) "Fira Code" 16) ; smaller  text (*)
    ("2048x1280" ,(* 2 60) ,(* 2 80) "Fira Code" 18) ; smallest text

    ;; pairing MPB 16" w/ HP 27Z @ 60hz
    ("1280x720"  ,(* 2 60) ,(* 2 80) "Fira Code"  9) ; largest  text
    ("1920x1080" ,(* 2 60) ,(* 2 80) "Fira Code" 12) ; larger   text
    ("2560x1440" ,(* 2 60) ,(* 2 80) "Fira Code" 16) ; medium   text
    ("3200x1800" ,(* 2 60) ,(* 2 80) "Fira Code" 18) ; smaller  text
    ("3840x2160" ,(* 2 60) ,(* 2 80) "Fira Code" 24) ; smallest text (*)
    ("4000x1440" ,(* 2 60) ,(* 2 80) "Fira Code" 16) ; chef POS (dell?) monitor

    ;; 13" MBP paired with HP 27Z @ 60hz usbc
    ;;1920x1080                                      ; largest  text (*)
    ;;2560x1440                                      ; larger   text
    ("3008x1692" ,(* 2 60) ,(* 2 80) "Fira Code" 18) ; medium   text
    ("3360x1890" ,(* 2 60) ,(* 2 80) "Fira Code" 20) ; smaller  text
    ;;3840x2160                                      ; smallest text
    ))

;; (rwd/display-reset)
;; (rwd-split-smart)
;; (rwd/current-display)

(defun rwd/display-setup (h w f s)
  (setq split-height-threshold h
        split-width-threshold w)
  (rwd-set-mac-font f s)
  (message "rwd/display-setup %s %s %s %s" h w f s))

(defun rwd/current-display ()
  (let* ((attr (frame-monitor-attributes))
         (geom (assoc 'geometry attr))
         (w (nth 3 geom))
         (h (nth 4 geom))
         (disp (format "%sx%s" w h))  ; calculated from actual geometry
         )
    disp))

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
