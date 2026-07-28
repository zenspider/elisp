;; -*- lexical-binding: t; -*-

;; (add-to-list 'load-path "~/Work/git/dakra/ghostel/lisp")

(eval-when-compile
  (require 'ghostel))

(with-eval-after-load 'ghostel-mode
  (keymap-set ghostel-mode-map "C-c e" #'ghostel-clear-scrollback)
  (defalias 'ghostel-write-file #'comint-write-output))
