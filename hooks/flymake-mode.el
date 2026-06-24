;; -*- lexical-binding: t; -*-

(eval-and-compile
  (require 'flymake-proc))

(defun flymake-html-init ()
  (let* ((temp-file (flymake-proc-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "/usr/local/bin/tidy" (list local-file))))

(add-to-list 'flymake-proc-allowed-file-name-masks
             '(("\\.html\\|\\.erb" flymake-html-init)))

(add-to-list 'flymake-err-line-patterns
             '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
               nil 1 2 4))
