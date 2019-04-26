(require 'xterm-color)
(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))
(add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)

(setq shell-dirstack-query "dirs -l")

(define-key shell-mode-map (kbd "C-c C-d") nil)  ; keeps biting me
(define-key shell-mode-map (kbd "C-z C-z")    'comint-stop-subjob)
(define-key shell-mode-map (kbd "M-<return>") 'shell-resync-dirs)
(define-key comint-mode-map [C-up]            'rwd-previous-line-6)
(define-key comint-mode-map [C-down]          'rwd-forward-line-6)
(define-key comint-mode-map (kbd "C-c e")     'rwd-shell-clear)

(defun comint-info-subjob ()
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

(define-key comint-mode-map "\C-c\C-t" 'comint-info-subjob)
