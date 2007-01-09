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
(global-set-key (kbd "<f7>")  'swap-buffers) ; located in setup-aliases.el
(global-set-key (kbd "<f8>")  'toggle-split) ; located in setup-aliases.el
(global-set-key (kbd "<f12>") 'query-replace-regexp)

(define-key global-map [C-up]           'previous-line-6)
(define-key global-map [C-down]         'forward-line-6)

(define-key isearch-mode-map (kbd "C-o") ; occur easily inside isearch
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

; compatibility
(if running-emacs
  (progn
    (global-set-key "\M-g" 'goto-line)
    (require 'dired)
    (define-key dired-mode-map "k" 'dired-kill-subdir)))
