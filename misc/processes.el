;;;###autoload
(eval-after-load 'subr
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function kill-buffer-query-functions)))
