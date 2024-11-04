;; -*- truncate-lines: t -*-

;;                                                                                                 1                                                                                                   2
;;       1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7         8         9         0
;;34567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012

(defvar rwd/displays nil
  "Contains a list of display entries for emacs' dispwatch")

(setq rwd/displays
  `(("default"   14)

    ;; 13" MBP M1
    ;; 1024x640 ?!?!
    ("1280x800"  12)
    ("1440x900"  14) ; *
    ("1680x1050" 16)

    ;; 13" MBA M2
    ("1470x956"  16)
    ("1710x1112" 17) ; *
    ("2048x1332" 19)

    ;; 16" MBP
    ("1152x720"  10)
    ("1344x840"  13)
    ("1536x960"  15)
    ("1792x1120" 16) ; *
    ("2048x1280" 18)
    ("2056x1329" 19) ; 16" M2 MBP raw

    ;; 16" MPB paired with HP 27Z @ 60hz
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
    ;; 13" MBP M2 paired with HP 27Z @ 60hz usbc (same)

    ("1504x846"  15)
    ;;1920x1080
    ;;2560x1440
    ;;3008x1692        *
    ;;3840x2160
    ))

;; (rwd/display-reset)
;; (rwd-split-smart)
;; (rwd/current-display)

(defcustom rwd/default-font "FiraCode Nerd Font"
  "Default font to use in all windows."
  :type 'string
  :group 'rwd/gui)

(defcustom rwd/default-split-height (* 2 60) "Used by rwd/display-setup"
  :type 'integer
  :group 'rwd/dispwatch)
(defcustom rwd/default-split-width  (* 2 80) "Used by rwd/display-setup"
  :type 'integer
  :group 'rwd/dispwatch)

(defun rwd/display-setup (s)
  (setq split-height-threshold rwd/default-split-height
        split-width-threshold  rwd/default-split-width)
  (rwd-set-mac-font rwd/default-font s))

(defun rwd/current-display ()
  (let* ((attr (frame-monitor-attributes))
         (geom (assoc 'geometry attr))
         (w (nth 3 geom))
         (h (nth 4 geom))
         (disp (format "%sx%s" w h))  ; calculated from actual geometry
         )
    disp))

(defvar rwd/dispwatch/initialized '())

(defun rwd/dispwatch-display-change-hook (geom)
  (when window-system
    (let* ((w       (car geom))
           (h       (cdr geom))
           (current (rwd/current-display))
           (disp    (format "%sx%s" w h))
           (args    (assoc disp      rwd/displays))
           (default (assoc "default" rwd/displays)))
      (unless (and rwd/dispwatch/initialized (equal current disp))
        (if rwd/dispwatch/initialized
            (message "dispwatch %S -> %S" current disp)
          (setq rwd/dispwatch/initialized t)
          (message "initializing dispwatch to %S" disp))
        (unless args
          (find-variable 'rwd/displays)
          (insert (format "%S\n" (list disp 'w 'h 16)))
          (message "Please extend rwd/displays with %S" disp)
          (setq args default))
        (apply 'rwd/display-setup (cdr args))
        (rwd-split-smart)))))

(when window-system
  (add-to-list 'load-path (expand-file-name "~/Work/git/mnp/dispwatch"))
  ;; https://github.com/mnp/dispwatch
  (rwd-require 'dispwatch))

(defun rwd/display-reset ()
  "Reset and force dispwatch to trigger again."
  (interactive)
  (message "rwd/display-reset")
  (setq rwd/dispwatch/initialized '())
  (setq dispwatch-current-display nil))

;; (rwd/display-setup 36)
;; (rwd-ns-fullscreen)
;; (rwd/display-reset)

(provide 'modes/dispwatch)
