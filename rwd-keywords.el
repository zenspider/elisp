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
      '(c-mode diff-mode emacs-lisp-mode enh-ruby-mode haskell-mode lisp-mode
               racc-mode racket-mode ruby-mode rust-mode scheme-mode sml-mode
               text-mode))

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
