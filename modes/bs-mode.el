(require 'bs)

;; adapted from:
;; https://github.com/gcv/dotfiles/blob/master/emacs/init.el

(defun rwd/persp/current-buffers ()
  (persp-buffers (persp-curr)))

(defun rwd/bs-config/persp (buf)
  (with-current-buffer buf
    (not (memq buf (rwd/persp/current-buffers)))))

;; TODO: switch to customize when I get this right
(add-to-list 'bs-configurations '("persp" nil nil
                                  "^\\*"
                                  rwd/bs-config/persp
                                  bs-sort-buffer-interns-are-last))
