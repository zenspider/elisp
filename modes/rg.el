(autoload 'rg-menu "rg-menu" "doco" t)
(global-set-key (kbd "C-c r") #'rg-menu)

(with-eval-after-load 'rg-menu
  (require 'rg)
  (rg-define-search rg-search-for-word
    :flags ("--word-regexp")
    :menu ("Search" "w" "Word")))
