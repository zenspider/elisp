(require 'perspective "../persp/perspective.el")

(defun persp-scratch-buffer ()          ; TODO: push up
  (let* ((curr-name (persp-name (persp-curr)))
         (initial-persp (equal curr-name persp-initial-frame-name)))
    (concat "*scratch*"
            (unless initial-persp
              (format " (%s)" (persp-name (persp-curr)))))))

(define-key perspective-map (kbd "d") 'persp-kill)
(define-key perspective-map (kbd "RET") 'persp-switch-last)

(unless window-system                   ; restore C-z z and C-z C-z to suspend
  (define-key perspective-map (kbd "z")   'suspend-frame)
  (define-key perspective-map (kbd "C-z") 'suspend-frame))

(persp-mode)
