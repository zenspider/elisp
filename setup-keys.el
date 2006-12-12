;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys & Menus:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\C-\M-n" 'insert-buffer-name)
(global-set-key "\C-\M-x" 'bury-buffer)
(global-set-key "\C-c\C-r" 'recompile)
(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key "\C-x\C-p" 'find-file-at-point)
(global-set-key "\C-x\C-t" 'toggle-buffer)
(global-set-key "\M-s" 'fixup-whitespace)
(global-set-key (kbd "<f10>") 'end-kbd-macro)
(global-set-key (kbd "<f11>") 'call-last-kbd-macro)
(global-set-key (kbd "<f12>") 'query-replace-regexp)
(global-set-key (kbd "<f5>")  'kill-buffer-and-close-window) ;; close
(global-set-key (kbd "<f6>")  'clean-whitespace)             ;; clean (setup-aliases.el)
(global-set-key (kbd "<f6>")  'windoze-sucks)
(global-set-key (kbd "<f7>")  'swap-buffers) ; located in setup-aliases.el
(global-set-key (kbd "<f8>")  'toggle-split) ; located in setup-aliases.el
(global-set-key (kbd "<f9>")  'start-kbd-macro)
(global-set-key [?\C-c ?\C-\;] 'comment-or-uncomment-region)

;; (defun prev-window ()
;;   (interactive)
;;   (other-window -1))
;; (global-set-key "\C-xO" 'prev-window)

(define-key global-map [C-up]           'previous-line-6)
(define-key global-map [C-down]         'forward-line-6)

(unless (featurep 'xemacs)
  (progn
    (global-set-key "\M-g" 'goto-line)
    (require 'dired)
    (define-key dired-mode-map "k" 'dired-kill-subdir)
  ))
