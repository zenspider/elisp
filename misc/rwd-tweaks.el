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
(progn
  (set-register ?e '(file . "~/Bin/elisp/emacs.el"))
  (set-register ?p '(file . "~/Bin/elisp/rwd-packages.el")))

;;;###autoload
(progn
  ;; (require 'popwin)
  (autoload 'popwin:original-display-buffer "popwin")
  (setq display-buffer-function 'popwin:original-display-buffer))

;;;###autoload
(eval-after-load 'vc
  '(defun vc-deduce-backend ()
     (cond ((derived-mode-p 'vc-dir-mode)   vc-dir-backend)
           ((derived-mode-p 'log-view-mode) log-view-vc-backend)
           ((derived-mode-p 'diff-mode)     diff-vc-backend)
           ((derived-mode-p 'dired-mode)
            (vc-responsible-backend default-directory))
           ((derived-mode-p 'shell-mode)
            (vc-responsible-backend default-directory))
           (vc-mode (vc-backend buffer-file-name)))))
