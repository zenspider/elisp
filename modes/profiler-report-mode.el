(with-eval-after-load 'profiler-report
  (define-key profiler-report-mode-map (kbd "I")
    (lambda () (interactive) (profiler-report-expand-entry t))))
