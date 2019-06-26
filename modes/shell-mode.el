(defun comint-scroll-to-bottom-on-output ()
  (interactive)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t))

(defun rwd/comint/send-input ()
  (interactive)
  (comint-send-input)
  (sleep-for 0.05)
  (comint-show-output))

(define-key comint-mode-map (kbd "<C-return>") 'rwd/comint/send-input)

(defun rwd/comint-show-full-color ()
  (interactive)
  (font-lock-mode -1)
  (require 'xterm-color)
  (make-local-variable 'comint-output-filter-functions)
  (make-local-variable 'comint-preoutput-filter-functions)
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)
  (remove-from-list comint-output-filter-functions 'ansi-color-process-output)
  (remove-from-list comint-output-filter-functions 'comint-postoutput-scroll-to-bottom))

(defun rwd/comint-info-subjob ()
  "Info the current subjob.
This command also kills the pending input
between the process mark and point.

WARNING: if there is no current subjob, you can end up suspending
the top-level process running in the buffer.  If you accidentally do
this, use \\[comint-continue-subjob] to resume the process.  (This
is not a problem with most shells, since they ignore this signal.)"
  (interactive)
  (comint-skip-input)
  (process-send-string (current-buffer) ""))
