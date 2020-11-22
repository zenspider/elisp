;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       1         2         3         4         5         6         7         8
;;345678901234567890123456789012345678901234567890123456789012345678901234567890

(defvar rwd/displays nil
  "Contains a list of display entries for emacs' dispwatch")

(setq rwd/displays
  `(("default"   14)

    ;; 13" MBP
    ("1440x900"  14) ; *

    ;; 13" MBP M1
    ;; 1024x640 ?!?!
    ("1280x800"  12)
    ;; 1440x900        *
    ("1680x1050" 16)

    ;; MBP 16"
    ("1152x720"  10)
    ("1344x840"  13)
    ("1536x960"  15)
    ("1792x1120" 16) ; *
    ("2048x1280" 18)

    ;; pairing MPB 16" w/ HP 27Z @ 60hz
    ("1280x720"   9)
    ("1920x1080" 12)
    ("2560x1440" 16)
    ("3200x1800" 18)
    ("3840x2160" 24) ; *

    ;; 13" MBP paired with HP 27Z @ 60hz usbc
    ;;1920x1080        *
    ;;2560x1440
    ("3008x1692" 18)
    ("3360x1890" 20)
    ;;3840x2160

    ;; 13" MBP M1 paired with HP 27Z @ 60hz usbc

    ("1504x846"  15)
    ;;1920x1080
    ;;2560x1440
    ;;3008x1692        *
    ;;3840x2160
    ))

;; (rwd/display-reset)
;; (rwd-split-smart)
;; (rwd/current-display)

(defcustom rwd/default-font "Fira Code"
  "Default font to use in all windows.")

(defcustom rwd/default-split-height (* 2 60) "Used by rwd/display-setup")
(defcustom rwd/default-split-width  (* 2 80) "Used by rwd/display-setup")

(defun rwd/display-setup (s)
  (setq split-height-threshold rwd/default-split-height
        split-width-threshold  rwd/default-split-width)
  (rwd-set-mac-font rwd/default-font s)
  (message "rwd/display-setup %s %s %s %s" h w rwd/default-font s))

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
        (insert (format "%S\n" (list disp 'w 'h 16)))
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

;; (rwd/display-reset)
