;;;###autoload
(eval-after-load "dabbrev" '(defalias 'dabbrev-expand 'hippie-expand))

;;;###autoload
(defun he-tag-beg ()
  (let ((p
         (save-excursion 
           (backward-word 1)
           (point))))
    p))

;;;###autoload
(defun rwd-try-expand-tag (old)
  (require 'cc-mode)
  (require 'etags)

  (unless old
    (he-init-string (he-tag-beg) (point))
    (setq he-expand-list (sort
                          (all-completions he-search-string 'rwd-tags-complete-tag) 'string-lessp)))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))

;;;###autoload
(defun rwd-tags-complete-tag (string predicate what)
  (save-excursion
    (if tags-completion-table
        ;; If we need to ask for the tag table, allow that.
        (if (eq what t)
            (all-completions string (tags-completion-table) predicate)
          (try-completion string (tags-completion-table) predicate)))))

;; stolen from HippieExpand on emacswiki:
;; Don't add extra paren when expanding line with paredit
;;;###autoload
(defadvice he-substitute-string (after he-paredit-fix)
  "remove extra paren when expanding line in paredit"
  (if (and paredit-mode (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

