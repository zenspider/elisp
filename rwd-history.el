(require 'timid)

(timid-mode t)
(setq timid-enable-globally t)

;; removes duplicates - from http://www.emacswiki.org/emacs/timid.el
(defun my-add-file-hook ()
  "Add the name of the file just opened or written to
     `file-name-history'"
  (and buffer-file-name
       (progn
         (setq file-name-history
               (delete (expand-file-name buffer-file-name)
                       (delete buffer-file-name file-name-history)))
         (push (expand-file-name buffer-file-name) file-name-history)))
  nil)

(add-hook 'find-file-hooks  'my-add-file-hook)
(add-hook 'write-file-hooks 'my-add-file-hook)

(dolist (cmd '(isearch-forward
               isearch-forward-regexp
               query-replace
               replace-regexp))
  (put cmd 'timid-completion 'disabled))

;; prime the pump:
;; (setq file-name-history (remove-duplicates (sort (rwd-find-project-files) 'string<)))
