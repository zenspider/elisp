;; removes duplicates - from http://www.emacswiki.org/emacs/timid.el
(defun my-add-file-hook ()
  "Add the name of the file just opened or written to
     `file-name-history'"
  (and buffer-file-name
       (progn (setq file-name-history
                    (delete buffer-file-name file-name-history))
              (push buffer-file-name file-name-history)))
  nil)

(add-hook 'find-file-hooks  'my-add-file-hook)
(add-hook 'write-file-hooks 'my-add-file-hook)
