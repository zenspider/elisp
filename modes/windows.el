(when-idle rwd-idle-time
  (windmove-default-keybindings)
  (winner-mode 1))

(defun nth* (n xs)
  "Select the Nth item from XS, zero based. Go backwards from the
right if N is negative."
  (if (< n 0)
      (nth* (1- (abs n)) (reverse xs))
    (nth n xs)))

(defun rwd/top-edge-windows ()
  (let* ((windows (list (frame-first-window)))
         (next    (window-in-direction 'right (car windows) nil 1)))
    (while next
      (setq windows (cons next windows))
      (setq next    (window-in-direction 'right next)))
    (nreverse windows)))

(defun rwd/display-buffer-in-column (buffer actions)
  "Try to display buffer in the Nth column (window on the top
edge) from the left, starting at 0. Negative values count from
the right. The Nth column is determined by the 'column assoc in
the ACTIONS alist."
  (let* ((column (alist-get 'column actions))
         (windows (rwd/top-edge-windows))
         (window  (and column (nth* column windows))))
    (when window
      (window--display-buffer buffer window 'reuse actions))))

(defun rwd/nth-column-for-matching-buffers (nth regexp)
  (add-to-list 'display-buffer-alist `(,regexp
                                       (rwd/display-buffer-in-column)
                                       (column . ,nth))
               'append))

(setq display-buffer-alist nil)
(rwd/nth-column-for-matching-buffers  0 "^shell-")
(rwd/nth-column-for-matching-buffers -1 "\\*info\\|\\*help")
(rwd/nth-column-for-matching-buffers -1 "^magit:")
(rwd/nth-column-for-matching-buffers  0 "^magit-\\(diff\\|revision\\):")
