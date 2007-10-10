;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys & Menus:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f12>")   'query-replace-regexp)
(global-set-key (kbd "<f7>")    'toggle-split)
(global-set-key (kbd "<f8>")    'swap-buffers)
(global-set-key (kbd "C-M-x")   'bury-buffer)
(global-set-key (kbd "C-c #")   'kmacro-insert-counter)
(global-set-key (kbd "C-c 1")   'my-reset-macro-counter)
(global-set-key (kbd "C-c C-r") 'recompile)
(global-set-key (kbd "C-c e")   'erase-buffer)
(global-set-key (kbd "C-c f")   'my-selective-display)
(global-set-key (kbd "C-c o")   'occur-buffer)
(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-x C-t") 'toggle-buffer)
(global-set-key (kbd "M-C-y")   'kill-ring-search)
(global-set-key (kbd "M-s")     'fixup-whitespace)
(global-set-key (kbd "M-?")     'etags-select-find-tag-at-point)

;; FIX: blech!
;; (mapcar (lambda (mode)
;;           (define-key mode (kbd "C-c C-a") 'autotest-switch))
;;         '(diff-mode-map
;; 	  diff-minor-mode-map
;; 	  grep-mode-map
;; 	  help-mode-map))

;; TODO: put this in elisp buffers only
; HACK (global-set-key (kbd "C-c e")   'my-eval-and-replace)

; this is so awesome - occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;; iconify bugs the crap out of me:
(when window-system (global-unset-key "\C-z"))
(def-hook comint-mode
  (define-key shell-mode-map (kbd "C-z") 'comint-stop-subjob))

(def-hook dired-mode
  (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode))

; compatibility:
(if running-emacs
    (progn
      (global-set-key (kbd "M-g")      'goto-line)
      (global-set-key (kbd "<C-up>")   'previous-line-6)
      (global-set-key (kbd "<C-down>") 'forward-line-6)
      (def-hook dired-load
        (define-key dired-mode-map "k" 'dired-kill-subdir))))

; This allows me to enforce that bury-buffer is bound to C-M-x
; regardless of mode (YAY!)
(require 'override-keymaps)
(override-keymaps)
