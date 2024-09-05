(eval-when-compile
  (require 'comint))

(when-idle rwd-idle-time
  (coterm-mode +1))

(add-hook 'comint-output-filter-functions #'comint-osc-process-output)

;; hacked to use string-equal-ignore-case
;; $HOSTNAME=TR9TJ70595, but url-host=tr9tj70595
(defun ansi-osc-directory-tracker (_ text)
  "Update `default-directory' from OSC 7 escape sequences.

This function is intended to be included as an element of the
list that is the value of `ansi-osc-handlers'.  You should arrange
for your shell to print the appropriate escape sequence at each prompt,
such as with the following command:

    printf \"\\e]7;file://%s%s\\e\\\\\" \"$HOSTNAME\" \"$PWD\"

This functionality serves as an alternative to `dirtrack-mode'
and `shell-dirtrack-mode'."
  (let ((url (url-generic-parse-url text)))
    (when (and (string= (url-type url) "file")
               (or (null (url-host url))
                   (string-equal-ignore-case (url-host url) (system-name))))
      (ignore-errors
        (cd-absolute (url-unhex-string (url-filename url)))))))

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
