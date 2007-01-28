;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys & Menus:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f12>")   'query-replace-regexp)
(global-set-key (kbd "<f7>")    'swap-buffers)
(global-set-key (kbd "<f8>")    'toggle-split)
(global-set-key (kbd "C-M-x")   'bury-buffer)
(global-set-key (kbd "C-c C-r") 'recompile)
(global-set-key (kbd "C-c e")   'my-eval-and-replace)
(global-set-key (kbd "C-c f")   'my-selective-display)
(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-x C-t") 'toggle-buffer)
(global-set-key (kbd "M-s")     'fixup-whitespace)

; this is so awesome - occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

; iconify bugs the crap out of me:
(global-unset-key (kbd "C-z"))
(add-hook 'comint-mode-hook
          (lambda ()
            (define-key shell-mode-map (kbd "C-z") 'comint-stop-subjob)))

; compatibility:
(if running-emacs
    (progn
      (global-set-key (kbd "M-g")      'goto-line)
      (global-set-key (kbd "<C-up>")   'previous-line-6)
      (global-set-key (kbd "<C-down>") 'forward-line-6)
      (add-hook 'dired-load-hook
                '(lambda ()
                   (define-key dired-mode-map "k" 'dired-kill-subdir)))))

; This allows me to enforce that bury-buffer is bound to C-M-x
; regardless of mode (YAY!)
(require 'override-keymaps)
(override-keymaps)
