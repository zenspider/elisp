;; -*- truncate-lines: t -*-

;;                                                                                                 1                                                                                                   2
;;       1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7         8         9         0
;;34567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012

(defun interactive-split-current-window ()
  "Interactively split the current window, either horizontally or
vertically. The rule of thumb is that this function favors a
horizontal split, unless it would result in windows narrower than
the current fill-column."
  (interactive)
  (if (> (window-width) (* 2 fill-column))
      (split-window-horizontally)
    (split-window-vertically)))

(defun rwd/window-columns ()
  (/ (frame-width) 80))

;; TODO: keep poking at using split-width-threshold and friends on the
;; large monitor and decide if I need to override
;; split-window-sensibly to favor vertical splits. This is a start:

;; https://emacs.stackexchange.com/questions/20492/how-can-i-get-a-sensible-split-window-policy
(defun rwd/split-window-sensibly (&optional window)
    "replacement `split-window-sensibly' function which prefers vertical splits"
    (interactive)
    (let ((window (or window (selected-window))))
        (or (and (window-splittable-p window t)
                 (with-selected-window window
                     (split-window-right)))
            (and (window-splittable-p window)
                 (with-selected-window window
                     (split-window-below))))))
