(require 'bs)

;; adapted from:
;; https://github.com/gcv/dotfiles/blob/master/emacs/init.el

(defun rwd/bs-config/persp (buf)
  (with-current-buffer buf
    (not (member buf (persp-buffers (persp-curr))))))

;; TODO: switch to customize when I get this right
(add-to-list 'bs-configurations
             '("persp"
               nil nil
               "^\\*"
               rwd/bs-config/persp))
