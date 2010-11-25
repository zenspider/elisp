;; Startup optimizations
;; http://www.elliotglaysher.org/emacs/

;;;###autoload
(setq gc-cons-threshold (max 3000000 gc-cons-threshold))

;;;###autoload
(setq default-frame-alist
      '((wait-for-wm . nil)
        (top         . 0)
        (width       . 80)
        (height      . 48)
        (font        . "Bitstream Vera Sans Mono-12")))


;; enable/disable commands:
;;;###autoload
(put 'erase-buffer 'disabled nil) ; nukes stupid warning

;;;###autoload
(set-register ?e '(file . "~/Bin/elisp/emacs.el"))
