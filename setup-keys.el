;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys & Menus:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (global-unset-key "\C-x\C-c") ;; Annoying behavior; save-buffers-kill-emacs
(global-unset-key 'insert) ; Annoying behavior

; (global-set-key 'f1  'undo)                         ;; undo
; (global-set-key 'f2  'x-kill-primary-selection)     ;; cut
; (global-set-key 'f3  'x-copy-primary-selection)     ;; copy
; (global-set-key 'f4  'x-yank-clipboard-selection)   ;; paste
(global-set-key 'f5  'kill-buffer-and-close-window) ;; close
(global-set-key 'f6  'windoze-sucks)
(global-set-key 'f6  'clean-whitespace)             ;; clean (setup-aliases.el)
(global-set-key 'f7  'swap-buffers) ; located in setup-aliases.el
(global-set-key 'f8  'toggle-split) ; located in setup-aliases.el
(global-set-key 'f9  'start-kbd-macro)
(global-set-key 'f10 'end-kbd-macro)
(global-set-key 'f11 'call-last-kbd-macro)
(global-set-key 'f12 'query-replace-regexp)

(global-set-key (kbd "C-x !") 'insert-shell-command-interactive) ; located in setup-aliases.el

(global-set-key "\C-\M-b" 'bury-buffer)
(global-set-key "\C-\M-n" 'insert-buffer-name)
(global-set-key "\M-s" 'fixup-whitespace)

