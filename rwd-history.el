;; TODO: move most of these defuns out to rwd-aliases (and rename)

;;;###autoload
(progn
  (require 'timid)
  (timid-mode t)
  (setq timid-enable-globally t))

;;;###autoload
(defun canonical-file-path (path)
  (file-truename (expand-file-name path)))

;;;###autoload
(defun canonical-file-paths (paths)
  (mapcar 'canonical-file-path paths))

;; removes duplicates - from http://www.emacswiki.org/emacs/timid.el - modified
;;;###autoload
(defun my-add-file-hook ()
  "Add the name of the file just opened or written to
     `file-name-history'"
  (and buffer-file-name
       (let ((expanded-path (canonical-file-path buffer-file-name)))
         (setq file-name-history (delete expanded-path file-name-history))
         (push expanded-path file-name-history)))
  nil)

;;;###autoload
(defun canonicalize-file-name-history ()
  (interactive)
  (message "canonicalizing file-name-history")
  (let ((newlist '())
        (old-size (length file-name-history)))
    (dolist (path (canonical-file-paths file-name-history))
      (and (file-exists-p path) (add-to-list 'newlist path)))
    (setq file-name-history newlist)
    (message "file-name-history went from %d to %d"
             old-size (length file-name-history))))

;;;###autoload
(progn 
  (add-hook 'find-file-hooks  'my-add-file-hook)
  (add-hook 'write-file-hooks 'my-add-file-hook)
  (add-hook 'savehist-save-hook  'canonicalize-file-name-history))

;;;###autoload
(dolist (cmd '(isearch-forward
               isearch-forward-regexp
               query-replace
               replace-regexp))
  (put cmd 'timid-completion 'disabled))

;;;###autoload
(defun repopulate-file-name-history ()
  (interactive)
  (setq newlist '())
  (dolist (path (canonical-file-paths
                 (remove-duplicates (sort (rwd-find-project-files) 'string<))))
    (add-to-list 'file-name-history path))
  (canonicalize-file-name-history))
