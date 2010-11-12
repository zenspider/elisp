;; TODO: move most of these defuns out to rwd-aliases (and rename)

;;;###autoload
(progn
  (require 'tramp)
  (require 'timid)
  (timid-mode t)
  (setq timid-enable-globally t)
  (dolist (cmd '(goto-line
                 isearch-forward
                 isearch-forward-regexp
                 isearch-query-replace
                 isearch-query-replace-regexp
                 query-replace
                 query-replace-regexp
                 replace-regexp))
    (put cmd 'timid-completion 'disabled)))

;;;###autoload
(defun canonical-file-path (path)
  (file-truename (expand-file-name path)))

;;;###autoload
(defun canonical-file-paths (paths)
  (mapcar 'canonical-file-path paths))

;; removes duplicates - from http://www.emacswiki.org/emacs/timid.el - modified
;;;###autoload
(defun rwd-add-file-hook ()
  "Add the name of the file just opened or written to
     `file-name-history'"
  (and buffer-file-name
       (let ((expanded-path (canonical-file-path buffer-file-name)))
         (setq file-name-history (delete expanded-path file-name-history))
         (push expanded-path file-name-history)))
  nil)

;;;###autoload
(defconst rwd-tramp-method-regexp
  (concat "^/" (regexp-opt (mapcar 'car tramp-methods) t) ":"))

;;;###autoload
(defun canonicalize-file-name-history ()
  "Set `file-name-history' to the first `history-length' - 100
elements of `file-name-history' after sorting by mtime,
canonicalizing, removing duplicates and filtering out all TRAMP
paths, directories, backups, and non-existent files."
  (interactive)
  (setq file-name-history
        (head (sort (remove-duplicates
                     (mapcar 'canonical-file-path
                             (remove-if
                              (lambda (path) 
                                (or
                                 (string-match "^/.+::" path)
                                 (string-match rwd-tramp-method-regexp path)
                                 (file-directory-p path)
                                 (string-match "~$" path)
                                 (not (file-exists-p path))))
                              file-name-history)) :test #'equal)
                    'sort-files-by-date)
         (- history-length 100))))

;;;###autoload
(progn 
  (add-hook 'find-file-hooks     'rwd-add-file-hook)
  (add-hook 'write-file-hooks    'rwd-add-file-hook)
  (add-hook 'savehist-save-hook  'canonicalize-file-name-history))

;;;###autoload
(defun repopulate-file-name-history ()
  (interactive)
  (setq file-name-history (rwd-find-project-files))
  (canonicalize-file-name-history))
