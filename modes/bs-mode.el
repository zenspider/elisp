(require 'bs)

;; adapted from:
;; https://github.com/gcv/dotfiles/blob/master/emacs/init.el

(defun rwd/persp/current-buffers ()
  (persp-buffers (persp-curr)))

(defun rwd/buffer-major-mode (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    major-mode))

(defun rwd/bs-config/persp (buf)
  (with-current-buffer buf
    (memq buf (rwd/persp/current-buffers))))

(defun rwd/bs-config/not-persp (buf)
  (not (rwd/bs-config/persp buf)))

(defun rwd/persp/same-mode (current-buf)
  (let* ((orig-mode    (rwd/buffer-major-mode bs--buffer-coming-from))
         (current-mode (rwd/buffer-major-mode current-buf)))
    (and (rwd/bs-config/persp current-buf)
         (equal orig-mode current-mode))))

(add-to-list 'bs-configurations '("persp"
                                  nil nil
                                  "^\\*" rwd/bs-config/not-persp
                                  bs-sort-buffer-interns-are-last))

(add-to-list 'bs-configurations '("current-mode"
                                  nil rwd/persp/same-mode
                                  "." nil
                                  nil))
