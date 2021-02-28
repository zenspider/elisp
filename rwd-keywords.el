;; ============================================================
;; Progress Keyword Highlighting:

;;;###autoload
(defface my-red-face
  '((t (:foreground "white" :background "red")))
  "A red face for warnings that are bad."
 :group 'my-faces)

;;;###autoload
(defface my-yellow-face
  '((t (:background "yellow")))
  "A yellow face for warnings that are not quite that bad."
 :group 'my-faces)

;;;###autoload
(setq rwd-programming-modes             ; TODO: move somewhere global
      '(c-mode
        enh-ruby-mode racc-mode ruby-mode sh-mode
        lisp-mode racket-mode scheme-mode emacs-lisp-mode
        markdown-mode yaml-mode))

;;;###autoload
(progn
  (setq yellow-tokens
        (delete ?\s "\\<\\(F IX\\|D OC\\|R ETIRE\\|T ODO\\|W ARN\\)\\>.*"))
  (setq red-tokens
        (delete ?\s "\\<\\(H ACK\\|R EFACTOR\\)\\>.*"))

  (mapc (lambda (mode)
          (font-lock-add-keywords
           mode
           (list (list yellow-tokens 0 ''my-yellow-face 'prepend)
                 (list red-tokens    0 ''my-red-face    'prepend))))
        rwd-programming-modes))
