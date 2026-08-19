;; -*- lexical-binding: t; -*-

;; (add-to-list 'load-path "~/Work/git/dakra/ghostel/lisp")

(eval-when-compile
  (require 'ghostel))

(with-eval-after-load 'ghostel
  (defun ghostel-send-C-u ()
    "Send C-u, geneally clearing from point to the prompt."
    (interactive)
    (ghostel--on-user-input)
    (ghostel-send-key "u" "ctrl"))

  (keymap-set ghostel-mode-map "C-c e" #'ghostel-clear-scrollback)
  (keymap-set ghostel-mode-map "C-c C-u" #'ghostel-send-C-u)
  (keymap-set ghostel-mode-map "C-c u"   #'ghostel-send-C-u)
  (keymap-set ghostel-mode-map "C-x C-p" #'ghostel-find-file-at-point)

  (defalias 'ghostel-write-file #'comint-write-output))
