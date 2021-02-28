;; -*- lexical-binding: t; -*-

(when-idle rwd-idle-time
  (windmove-default-keybindings)
  (winner-mode 1)
  (smartrep-define-key winner-mode-map "C-c"
                       '(("<left>"  . winner-undo))))

(defun nth* (n xs)
  "Select the Nth item from XS, zero based. Go backwards from the
right if N is negative."
    (if (< n 0)
        (let* ((xs (vconcat xs)))
          (aref xs (+ (length xs) n)))
      (nth n xs)))

(defun rwd/top-edge-windows ()
  (let* ((windows (list (frame-first-window)))
         (next    (window-in-direction 'right (car windows) nil 1)))
    (while next
      (setq windows (cons next windows))
      (setq next    (window-in-direction 'right next)))
    (nreverse windows)))

(defun rwd/display-buffer-in-column (buffer actions)
  "Try to display buffer in the Nth column (window on the top
edge) from the left, starting at 0. Negative values count from
the right. The Nth column is determined by the 'column assoc in
the ACTIONS alist."
  (let* ((column (alist-get 'column actions))
         (windows (rwd/top-edge-windows))
         (window  (and column (nth* column windows))))
    (when window
      (window--display-buffer buffer window 'reuse actions))))

(defun rwd/nth-column-for-matching-buffers (nth regexp)
  (add-to-list 'display-buffer-alist `(,regexp
                                       (display-buffer-reuse-window rwd/display-buffer-in-column)
                                       (column . ,nth))
               'append))

(setq display-buffer-alist nil)

;; (rwd/nth-column-for-matching-buffers  0 "^shell-")
(rwd/nth-column-for-matching-buffers  0 "^magit-diff:")
(rwd/nth-column-for-matching-buffers  0 "^magit-revision:")
(rwd/nth-column-for-matching-buffers  0 "\\*vc-diff\\*")
(rwd/nth-column-for-matching-buffers  0 "^magit-\\(diff\\|revision\\):")

(rwd/nth-column-for-matching-buffers -1 "^COMMIT_EDITMSG")
(rwd/nth-column-for-matching-buffers -1 "PULLREQ_EDITMSG")
(rwd/nth-column-for-matching-buffers -1 "Racket REPL")
(rwd/nth-column-for-matching-buffers -1 "^.P4 diff")
(rwd/nth-column-for-matching-buffers -1 "\\*info\\|\\*help")
(rwd/nth-column-for-matching-buffers -1 "^magit:")

;; ; truncate-lines: t
;;                                                                                                 1                                                                                                   2
;;       1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7         8         9         0
;;34567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; usual 2 column code & execution

;; +-----------------+-----------------+
;; |                 |                 |
;; |                 |                 |
;; |      shell      |     code        |
;; |                 |                 |
;; |                 |                 |
;; |                 |                 |
;; |                 |                 |
;; |                 |                 |
;; |                 |                 |
;; |                 |                 |
;; |                 |                 |
;; |                 |                 |
;; |                 |                 |
;; +-----------------+-----------------+

;; 2 column code & test / autotest

;; +-----------------+-----------------+
;; |                 |                 |
;; |                 |                 |
;; |      code       |     tests       |
;; |                 |     magit       |
;; |                 |     diff        |
;; |                 |     help        |
;; |                 |                 |
;; |                 |                 |
;; |                 |-----------------+
;; |                 |    (popup)      |
;; |                 |     help        |
;; |                 |     repl        |
;; |                 |     etc         |
;; +-----------------+-----------------+

;; usual 3 column ... not sure how solid on this yet

;; +-----------------+-----------------+-----------------+
;; |                 |                 |                 |
;; |                 |                 |                 |
;; |                 |                 |     tests       |
;; |                 |                 |     magit       |
;; |      shell      |      code       |     diff        |
;; |                 |                 |     help        |
;; |                 |                 |                 |
;; |                 |                 |                 |
;; |                 |                 +-----------------+
;; |                 |                 | (popup)         |
;; |                 |                 |  help           |
;; |                 |                 |  repl           |
;; |                 |                 |  etc            |
;; +-----------------+-----------------+-----------------+


;; "*Calendar*"
;; "*Completions*"
;; "*Help*"
;; "*Messages*"
;; "*Shell Command Output*"
;; "*eshell*"
;; "*info*"
;; "*undo-tree*"
;; "\\*Async Shell.*\\*"
;; "\\*[Wo]*Man.*\\*"
;; "\\*poporg.*\\*"
;; "\\`\\*helm.*?\\*\\'"
;; "\\`\\*magit.*?\\*\\'"
;;
;; compilation-mode
;; magit-log-mode
;; magit-status-mode
;; occur-mode
;;
;; (setq rwd/purpose/3-col
;;       '(
;;         (comint-mode              . left)
;;         (eshell-mode              . left)
;;         (shell-mode               . left)
;;         (term-mode                . left)
;;         (emacs-lisp-mode          . middle)
;;         (enh-ruby-mode            . middle)
;;         (prog-mode                . middle)
;;         (text-mode                . middle)
;;         (Info-mode                . right)
;;         (apropos-mode             . right)
;;         (compilation-mode         . right)
;;         (dired-mode               . right)
;;         (grep-mode                . right)
;;         (helpful-mode             . right)
;;         (magit-diff-mode          . right)
;;         (magit-log-mode           . right)
;;         (magit-status-mode        . right)
;;         (occur-mode               . right)
;;         ))

;; (setq rwd/purpose/2-col
;;       '(
;;         (comint-mode              . left)
;;         (eshell-mode              . left)
;;         (term-mode                . left)
;;         (prog-mode                . left)
;;         (text-mode                . left)
;;
;;         (Info-mode                . right)
;;         (compilation-mode         . right)
;;         (dired-mode               . right)
;;         (magit-mode               . right)
;;         (special-mode             . right)
;;         ))

(defun rwd/for-mode (mode)
  (lambda (buffer actions)
    (with-current-buffer buffer (derived-mode-p mode))))

(defun rwd/nth-column-for-mode (nth mode)
  (add-to-list 'display-buffer-alist `(,(rwd/for-mode mode)
                                       (rwd/display-buffer-in-column)
                                       (column . ,nth))
               'append))

;; (rwd/for-mode 'comint-mode)
;; (apply (rwd/for-mode 'comint-mode) '("shell-1" 42))

;; (rwd/nth-column-for-matching-buffers  0 'comint-mode)

;; (add-to-list 'display-buffer-alist
;;              '((lambda (buffer actions)
;;                  (with-current-buffer buffer
;;                    (derived-mode-p 'comint-mode)))
;;                (rwd/display-buffer-in-column)
;;                (column . 0)))
