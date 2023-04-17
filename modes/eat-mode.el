;; ANNOYANCE: M-up/M-down don't send as arrows -- this is hard muscle memory
;; TODO: map M-p to (eat-self-input 1 (kbd "C-p"))
;; (define-key eat-mode-map           [M-p] (lambda () (eat-self-input 1 [C-p])))
;; (define-key eat-semi-char-mode-map (kbd "M-p") (lambda () (eat-self-input 1 (kbd "C-p"))))
;; (define-key term-raw-map (kbd "M-p") 'term-send-up)
;; (define-key term-raw-map (kbd "M-n") 'term-send-down)

;;;; eat-semi-char-mode
;; C-c M-d -> eat-char-mode
;; C-c C-e -> eat-emacs-mode
;; C-c C-j -> eat-semi-char-mode

;;;; eat-emacs-mode
;; C-c M-d -> eat-char-mode
;; C-c C-j -> eat-semi-char-mode

;;;; eat-char-mode
;; C-M-m   -> eat-semi-char-mode
;; M-RET   -> eat-semi-char-mode


;; (with-eval-after-load 'eat
;;   (message "EAT LOADED!")
;;   (setq eat-semi-char-non-bound-keys
;;         (append (list
;;                  (kbd "M-0")
;;                  (kbd "M-1") (kbd "M-2") (kbd "M-3")
;;                  (kbd "M-4") (kbd "M-5") (kbd "M-6")
;;                  (kbd "M-7") (kbd "M-8") (kbd "M-9")
;;                  )
;;                 eat-semi-char-non-bound-keys))
;;   (eat-update-semi-char-mode-map))

(require 'eat)
(message "EAT LOADED!")
(setq eat-semi-char-non-bound-keys      ; TODO: wrap in with-eval-after-load https://codeberg.org/akib/emacs-eat/issues/56
      (append (list
               (kbd "M-0")
               (kbd "M-1") (kbd "M-2") (kbd "M-3")
               (kbd "M-4") (kbd "M-5") (kbd "M-6")
               (kbd "M-7") (kbd "M-8") (kbd "M-9")
               )
              eat-semi-char-non-bound-keys))
(eat-update-semi-char-mode-map)

(defalias 'rwd-shell 'rwd-shell-old "The old rwd-shell command")

;; TODO: fold this back into rwd-aliases when it feels right

;;;###autoload
(defun rwd-shell ()
  "Create a shell (using eat) buffer that is properly named (shell-<N>)"
  (interactive)
  (let ((buf (rwd-unique-buffer "shell")))
    (if (rwd-currently-only-scratch)
        (progn
          (when (and (eq 1 (length (window-list)))
                     (> (window-width) 82))
            (rwd-split-smart))
          (rwd-eat-buffer (switch-to-buffer buf)))
      (rwd-eat-buffer (switch-to-buffer-other-window buf)))))

(defun rwd-eat-buffer (buffer)
  (with-current-buffer buffer
    (unless (eq major-mode #'eat-mode)
      (eat-mode))
    (pop-to-buffer-same-window buffer)
    (unless (and eat--terminal
                 (eat-term-parameter eat--terminal 'eat--process))
      (eat-exec buffer (buffer-name) "/usr/bin/env" nil
                (list "sh" "-c" shell-file-name)))
    buffer))
