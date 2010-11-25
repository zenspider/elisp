(inf-ruby-keys)

(define-key ruby-mode-map (kbd "C-c C-a") 'autotest-switch)
(define-key ruby-mode-map (kbd "C-c C-p") 'pastebin)
(define-key ruby-mode-map (kbd "C-c C-r") 'rcov-buffer)
(define-key ruby-mode-map (kbd "C-c C-b") 'ruby-run-buffer-clean)
(define-key ruby-mode-map (kbd "C-c C-t") 'ri-show-term-composite-at-point)

;; TODO: fix this to only be the which-func-modes stuff. preferably elsewhere
(require 'which-func)
(add-to-list 'which-func-modes 'ruby-mode)
(which-func-mode 1)

(imenu-add-menubar-index)
(flyspell-prog-mode)
(blank-mode))
