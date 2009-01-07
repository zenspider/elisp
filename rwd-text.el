;;;###autoload
(def-hook text-mode ; can't use hook-after-load because it never "loads"
  (turn-on-auto-fill)
  (define-key text-mode-map (kbd "M-s") 'fixup-whitespace)
  (flyspell-mode))

;; from http://www.emacswiki.org/emacs/CocoAspell
;;;###autoload
(setq ispell-program-name "aspell"
      ispell-dictionary "english"
      ispell-dictionary-alist
      (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
                       ("-B" "-d" "english" "--dict-dir"
                        "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
                       nil iso-8859-1)))
        `((nil ,@default)
          ("english" ,@default))))
