(add-to-list 'display-buffer-alist `(,(rx bos "*Flycheck errors*" eos)
                                     (display-buffer-reuse-window
                                      display-buffer-below-selected)
                                     (window-height   . 0.33)))
(flycheck-package-setup)
(flycheck-color-mode-line-mode)
