;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Misc:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ; enable/disable commands:
(put 'erase-buffer 'disabled nil) ;; nukes stupid warning
(put 'overwrite-mode 'disabled t) ; annoying

(if window-system
    (add-hook 'after-init-hook 'rwd-resize-small t))

;; (setq ispell-program-name "aspell")
;; (setq ispell-dictionary-alist
;;       '((nil
;;          "[A-Za-z]" "[^A-Za-z]" "[']" nil
;;          ("-B" "-d" "english" "--dict-dir"
;;           "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
;;          nil iso-8859-1)))

;; (eval-after-load "dabbrev" '(defalias 'dabbrev-expand 'hippie-expand))

;; (defun he-tag-beg ()
;;   (let ((p
;;          (save-excursion 
;;            (backward-word 1)
;;            (point))))
;;     p))

;; (defun try-expand-tag (old)
;;   (unless  old
;;     (he-init-string (he-tag-beg) (point))
;;     (setq he-expand-list (sort
;;                           (all-completions he-search-string 'tags-complete-tag) 'string-lessp)))
;;   (while (and he-expand-list
;;               (he-string-member (car he-expand-list) he-tried-table))
;;     (setq he-expand-list (cdr he-expand-list)))
;;   (if (null he-expand-list)
;;       (progn
;;         (when old (he-reset-string))
;;         ())
;;     (he-substitute-string (car he-expand-list))
;;     (setq he-expand-list (cdr he-expand-list))
;;     t))
