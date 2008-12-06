;;; oddmuse

;;;###autoload
(progn
  (autoload 'oddmuse-edit "oddmuse" "doco" t)
  (autoload 'emacswiki-post "oddmuse" "doco" t))

;;;###autoload
(defadvice oddmuse-post (before define-question compile activate)
  (unless (string-match "question" oddmuse-post)
    (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post))))
