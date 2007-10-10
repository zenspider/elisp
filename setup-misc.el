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
