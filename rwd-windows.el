;; -*- truncate-lines: t -*-

;;                                                                                                 1                                                                                                   2
;;       1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7         8         9         0
;;34567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012

;;;###autoload
(progn
  (windmove-default-keybindings)
  (winner-mode 1)
  (require 'window-number)
  (window-number-meta-mode 1))

;; https://www.reddit.com/r/emacs/comments/2k71ql/efficiently_using_a_large_monitor/
;; (defun working-split (window-count)
;;   "Make vertical splits for working window setup.
;;
;; If optional argument WINDOW_COUNT is omitted or nil, default to
;; max splits of at least 90 chars wide
;; "
;;   (interactive "P")
;;   (let ((window-count (if window-count window-count (/ (frame-width) 79)))
;;         (show-buffers (if (projectile-project-p)
;;                           (projectile-project-buffers)
;;                         (remove-if-not 'buffer-live-p
;;                                        (remove-if 'minibufferp (rwd/persp/current-buffers))))))
;;     (delete-other-windows)
;;     ;; split window appropriate count - make 2nd window current
;;     (dotimes (i (- window-count 1))
;;       (split-window-horizontally)
;;       (if (= i 0) (other-window 1)))
;;     (balance-windows)
;;     ;; set window buffer from show-buffers list
;;     (mapcar* 'set-window-buffer (window-list) show-buffers)))

(setq split-height-threshold (* 2 60)
      split-width-threshold (* 2 80))

;; https://emacs.stackexchange.com/questions/20492/how-can-i-get-a-sensible-split-window-policy
(defun my-split-window-sensibly (&optional window)
    "replacement `split-window-sensibly' function which prefers vertical splits"
    (interactive)
    (let ((window (or window (selected-window))))
        (or (and (window-splittable-p window t)
                 (with-selected-window window
                     (split-window-right)))
            (and (window-splittable-p window)
                 (with-selected-window window
                     (split-window-below))))))

(setq split-window-preferred-function #'my-split-window-sensibly)

;; stolen and cleaned from: https://emacs.stackexchange.com/questions/28390/quickly-adjusting-text-to-dpi-changes
(defun rwd/dpi ()
  (let* ((attr    (frame-monitor-attributes))
         (mm-size (assoc 'mm-size attr))
         (mm-x    (nth 1 mm-size))
         (geo     (assoc 'geometry attr))
         (geo-x   (nth 3 geo))
         (in/mm   25.4))
    (* (/ (float geo-x) mm-x) in/mm)))

(defun rwd/window-columns ()
  (/ (frame-width) 80.0))

(rwd/dpi)                               ; 128.33684210526314

(defun rwd/monitor-font-size ()
  (let ((dpi (rwd/dpi)))
    (cond ((< dpi 110) 12)
          ((< dpi 130) 14)
          ((< dpi 160) 16)
          (t 14))))

(rwd-set-mac-font "Menlo" (rwd/monitor-font-size))

(window-width)


;; ;; Maintain at least 80 columns, even when the window is resized
;;
;; (setq maximum-font-height 200)
;; (setq minimum-font-height 140)
;; (setq previous-frame-width 0)
;; (setq minimum-frame-width (* 2 80))
;;
;; ;; 290 =  83 chars wide 26 lines tall
;; ;; 250 =  94 chars wide 30 lines tall
;; ;; 220 = 109 chars wide
;; ;; 210 = 109 chars wide
;; ;; 200 = 118 chars wide
;; ;; 190 = 129 chars wide
;; ;; 180 = 129 chars wide
;; ;; 170 = 142 chars wide
;; ;; 160 = 142 chars wide
;; ;; 150 = 157 chars wide
;; ;; 140 = 177 chars wide 56 lines tall
;;
;; (defun window-width-to-font-size (window-width)
;;   ;; (window-body-width)
;;   ;; (window-body-height)
;;
;;   (frame-width)
;;   (frame-pixel-width)
;;
;;   (/ (frame-text-width)
;;      (* 80 (frame-char-width)))
;;
;;   (frame-pixel-height)
;;
;;   ;; Insert a calculation to turn window width into 79 chars.
;;   )
;;
;; (add-to-list
;;  'window-size-change-functions
;;  (lambda (frame)
;;    (dolist (window (window-list frame))
;;
;;      (set-face-attribute 'default frame :height trial-size)
;;      (set-face-attribute 'default nil :width (window-width-to-font-size (window-body-width window))))))
;;
;;
;; ;; https://www.emacswiki.org/emacs/FrameResize
;; (defun rwd/auto-resize-fonts (frame)
;;   (if (/= previous-frame-width (frame-width frame))
;;       (let ((trial-size maximum-font-height))
;;         (set-face-attribute 'default frame :height trial-size)
;;         (while (and (> trial-size minimum-font-height)
;;                     (< (frame-width) minimum-frame-width))
;;           (setq trial-size (- trial-size 10))
;;           (set-face-attribute 'default frame :height trial-size))
;;         (setq previous-frame-width (frame-width frame)))))
;;
;; ;; (add-hook 'window-size-change-functions 'rwd/auto-resize-fonts)
;; ;; (remove-hook 'window-size-change-functions 'rwd/auto-resize-fonts)
;;
;; ;; (let ((window (or window (selected-window))))
;; ;;   (or (and (window-splittable-p window)
;; ;;            ;; Split window vertically.
;; ;;            (with-selected-window window
;; ;;              (split-window-below)))
;; ;;       (and (window-splittable-p window t)
;; ;;            ;; Split window horizontally.
;; ;;            (with-selected-window window
;; ;;              (split-window-right)))
;; ;;       (and (eq window (frame-root-window (window-frame window)))
;; ;;            (not (window-minibuffer-p window))
;; ;;            ;; If WINDOW is the only window on its frame and is not the
;; ;;            ;; minibuffer window, try to split it vertically disregarding
;; ;;            ;; the value of `split-height-threshold'.
;; ;;            (let ((split-height-threshold 0))
;; ;;              (when (window-splittable-p window)
;; ;;                (with-selected-window window
;; ;;                  (split-window-below)))))))
;;
;; ;; http://arnab-deka.com/posts/2012/09/emacs-change-fonts-dynamically-based-on-screen-resolution/
;; (defun fontify-frame (frame)
;;   (interactive)
;;   (if window-system
;;       (progn
;;         (if (> (x-display-pixel-width) 2000)
;;             (set-frame-parameter frame 'font "Inconsolata 19") ;; Cinema Display
;;          (set-frame-parameter frame 'font "Inconsolata 16")))))
;;
;; ;; Fontify current frame
;; (fontify-frame nil)
;;
;; ;; Fontify any future frames
;; (push 'fontify-frame after-make-frame-functions)
