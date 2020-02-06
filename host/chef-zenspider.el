(defvar rwd/erm/flycheck t "should we use flycheck mode?")

(defun zenspider/erm/flycheck ()
  (when rwd/erm/flycheck
    (flycheck-mode 1))
  (define-key enh-ruby-mode-map (kbd "C-c e")
    'rwd-flycheck-toggle-list-errors))

(add-hook 'enh-ruby-mode-hook 'zenspider/erm/flycheck)
;; (remove-hook 'enh-ruby-mode-hook 'zenspider/erm/flycheck)

(defun zenspider/magit/skip-tags ()
  (add-to-list 'magit-refs-filter-alist '("^[v0-9]")))

(add-hook 'magit-mode-hook 'zenspider/magit/skip-tags)
;; (remove-hook 'magit-mode-hook 'zenspider/magit/skip-tags)
