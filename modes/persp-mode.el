
(setq persp-path (expand-file-name "~/Work/git/nex3/perspective-el"))

(when (file-directory-p persp-path)
  (add-to-list 'load-path persp-path))

(require 'perspective)

(define-key perspective-map (kbd "d") 'persp-kill)
(define-key perspective-map (kbd "RET") 'persp-switch-last)

(unless window-system                   ; restore C-z z and C-z C-z to suspend
  (define-key perspective-map (kbd "z")   'suspend-frame)
  (define-key perspective-map (kbd "C-z") 'suspend-frame))

(persp-mode)
