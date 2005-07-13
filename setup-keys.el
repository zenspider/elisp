;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys & Menus:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (global-unset-key "\C-x\C-c") ;; Annoying behavior; save-buffers-kill-emacs
; (global-unset-key 'insert) ; Annoying behavior

; (global-set-key 'f1  'undo)                         ;; undo
; (global-set-key 'f2  'x-kill-primary-selection)     ;; cut
; (global-set-key 'f3  'x-copy-primary-selection)     ;; copy
; (global-set-key 'f4  'x-yank-clipboard-selection)   ;; paste

(global-set-key (kbd "<f5>")  'kill-buffer-and-close-window) ;; close
(global-set-key (kbd "<f6>")  'windoze-sucks)
(global-set-key (kbd "<f6>")  'clean-whitespace)             ;; clean (setup-aliases.el)
(global-set-key (kbd "<f7>")  'swap-buffers) ; located in setup-aliases.el
(global-set-key (kbd "<f8>")  'toggle-split) ; located in setup-aliases.el
(global-set-key (kbd "<f9>")  'start-kbd-macro)
(global-set-key (kbd "<f10>") 'end-kbd-macro)
(global-set-key (kbd "<f11>") 'call-last-kbd-macro)
(global-set-key (kbd "<f12>") 'query-replace-regexp)

; (global-set-key (kbd "C-x !") 'insert-shell-command-interactive) ; located in setup-aliases.el

(global-set-key "\C-\M-x" 'bury-buffer)
(global-set-key "\C-\M-n" 'insert-buffer-name)
(global-set-key "\M-s" 'fixup-whitespace)
(global-set-key "\C-x\C-p" 'find-file-at-point)
(global-set-key "\C-c\C-r" 'recompile)

(defun forward-block-of-lines () 
  (interactive "_") 
  (forward-line block-movement-size)) 

(define-key global-map [C-up]           'previous-line-6)
(define-key global-map [C-down]         'forward-line-6)

(unless (featurep 'xemacs)
  (progn
    (require 'dired)
    (global-set-key "\M-g" 'goto-line)
    (define-key dired-mode-map "k" 'dired-kill-subdir)
  ))
