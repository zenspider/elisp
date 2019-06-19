;; (inf-ruby-setup-keybindings)

(define-key ruby-mode-map (kbd "C-c C-a") 'autotest-switch)
(define-key ruby-mode-map (kbd "C-c C-p") 'pastebin)
(define-key ruby-mode-map (kbd "C-c C-r") 'rcov-buffer)
(define-key ruby-mode-map (kbd "C-c C-b") 'ruby-run-buffer-clean)
(define-key ruby-mode-map (kbd "C-c C-t") 'ri-show-term-composite-at-point)

(imenu-add-menubar-index)
(flyspell-prog-mode)
