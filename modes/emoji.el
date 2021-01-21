;; ✅ <- if you see a checkmark here, then emoji is working,
;; otherwise, try something like this:

;; (when window-system
;;   ;; from https://so.nwalsh.com/2020/02/29/dot-emacs
;;   (defun rwd/set-emoji-font (&optional frame)
;;     "Adjust the font settings of FRAME so Emacs can display emoji properly."
;;     (let ((font-name (case system-type
;;                        ((darwin) "Apple Color Emoji"))))
;;       (when font-name
;;         (set-fontset-font t 'symbol (font-spec :family font-name) frame 'prepend))))
;;   ;; For when Emacs is started in GUI mode:
;;   (rwd/set-emoji-font nil)
;;   (add-hook 'after-make-frame-functions 'rwd/set-emoji-font))
