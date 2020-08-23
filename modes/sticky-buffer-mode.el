;; https://lists.gnu.org/archive/html/help-gnu-emacs/2007-05/msg00975.html

(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil "!" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))
