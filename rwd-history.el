;; TODO: move most of these defuns out to rwd-aliases (and rename)

(eval-when-compile
  (require 'dash))

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

(defun rwd/filter-file-list (files)
  "Canonicalize FILES, removing duplicates and
filtering out all TRAMP paths, directories, backups, and
non-existent files."
  (require 'tramp)
  (let ((rwd-tramp-method-regexp
         (concat "^/" (regexp-opt (mapcar 'car tramp-methods) t) ":")))
    (-uniq                              ; TODO: removed sorting
     (mapcar 'canonical-file-path
             (-reject (lambda (path)
                        (or
                         (string-match "^/.+::" path)
                         (string-match rwd-tramp-method-regexp path)
                         (file-directory-p path)
                         (string-match "~$" path)
                         (not (file-exists-p path))))
                      files)))))

;;;###autoload
(defun canonicalize-file-name-history ()
  "Set `file-name-history' to the first `history-length' - 100
elements of `file-name-history' after canonicalizing, removing
duplicates and filtering out all TRAMP paths, directories,
backups, and non-existent files."
  (interactive)
  (setq file-name-history (head (rwd/filter-file-list file-name-history)
                                (- history-length 100))))

;;;###autoload
(progn
  (add-hook 'find-file-hooks     'rwd-add-file-hook)
  (add-hook 'write-file-hooks    'rwd-add-file-hook)
  (add-hook 'savehist-save-hook  'canonicalize-file-name-history))

;;;###autoload
(defun repopulate-file-name-history ()
  (interactive)
  (setq file-name-history
        (reverse (sort (rwd/filter-file-list (rwd-find-project-files))
                       'sort-files-by-mtime)))
  (canonicalize-file-name-history))
