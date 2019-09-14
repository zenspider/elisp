(add-hook 'enh-ruby-mode-hook
          '(lambda ()
             (flycheck-mode 1)
             (define-key enh-ruby-mode-map (kbd "C-c e")
               'rwd-flycheck-toggle-list-errors)))
