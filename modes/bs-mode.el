(require 'bs)

;; adapted from:
;; https://github.com/gcv/dotfiles/blob/master/emacs/init.el

(defun rwd/buffer-major-mode (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    major-mode))

(when window-system
 (defun rwd/persp/same-mode (current-buf)
   (let* ((orig-mode    (rwd/buffer-major-mode bs--buffer-coming-from))
          (current-mode (rwd/buffer-major-mode current-buf)))
     (and (persp-is-current-buffer current-buf)
          (equal orig-mode current-mode))))

 (add-to-list 'bs-configurations '("persp"
                                   nil nil
                                   "^\\*" persp-buffer-filter
                                   bs-sort-buffer-interns-are-last))

 (add-to-list 'bs-configurations '("current-mode"
                                   nil rwd/persp/same-mode
                                   "." nil
                                   nil)))
