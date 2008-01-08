;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; enable/disable commands:
(put 'erase-buffer 'disabled nil) ;; nukes stupid warning
(put 'overwrite-mode 'disabled t) ; annoying
;; (put 'downcase-region  'disabled nil)
;; (put 'upcase-region    'disabled nil)

;; in order of preference:
;; (my-set-mac-font "bitstream vera sans mono" 12)
;; (my-set-mac-font "monaco" 12)
;; (my-set-mac-font "andale mono" 12)
;; (my-set-mac-font "profont" 12)
;; (my-set-mac-font "profont" 9)
;; (my-set-mac-font "courier" 12)
;; (my-set-mac-font "courier new" 12)

(add-hook 'after-init-hook 'small t)

(setq ispell-program-name "aspell")
(setq ispell-dictionary-alist
      '((nil
         "[A-Za-z]" "[^A-Za-z]" "[']" nil
         ("-B" "-d" "english" "--dict-dir"
          "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
         nil iso-8859-1)))

(eval-after-load "dabbrev" '(defalias 'dabbrev-expand 'hippie-expand))

; (require 'cc-mode)

(defun he-tag-beg ()
  (let ((p
         (save-excursion 
           (backward-word 1)
           (point))))
    p))

(defun try-expand-tag (old)
  (unless  old
    (he-init-string (he-tag-beg) (point))
    (setq he-expand-list (sort
                          (all-completions he-search-string 'tags-complete-tag) 'string-lessp)))
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
