;; autotest.el - by Ryan Davis - ryan-ruby@zenspider.com
;;
;; Sets up an autotest buffer and provides conveniencte methods.
;;
;; History:
;; 1.0 beta 2 - 2007-04-03 - added autotest plugin / communication support
;; 1.0 beta 1 - 2007-03-06 - initial release

(require 'shell)

(defun autotest ()
  "Fire up an instance of autotest in its own buffer with shell bindings and compile-mode highlighting and linking."
  (interactive)
  (let ((buffer (shell "*autotest*")))

    (define-key shell-mode-map "\C-c\C-a" 'autotest-switch)

    (set (make-local-variable 'comint-output-filter-functions)
         '(comint-truncate-buffer comint-postoutput-scroll-to-bottom))
    (set (make-local-variable 'comint-buffer-maximum-size) 5000)
    (set (make-local-variable 'comint-scroll-show-maximum-output) t)
    (set (make-local-variable 'comint-scroll-to-bottom-on-output) t)

    (set (make-local-variable 'compilation-error-regexp-alist)
         '(
           ("^ +\\([^:]+\\):\\([0-9]+\\)" 1 2)
           ("\\[\\(.*\\):\\([0-9]+\\)\\]:$" 1 2)
           ("^ *\\(#{RAILS_ROOT}\\|[[+]\\)?\\([^:
]+\\):\\([0-9]+\\):in" 2 3)
           ))
    (compilation-shell-minor-mode)
    (comint-send-string buffer "autotest\n")))

(defun autotest-switch ()
  "Switch back and forth between autotest and the previous buffer"
  (interactive)
  (if (equal "*autotest*" (buffer-name))
      (switch-to-buffer nil)
    (switch-to-buffer "*autotest*")))

(if (require 'unit-test nil t)
    (progn
      (message "starting emacs server for autotest")
      (setq unit-test-colours (acons "gray" "#999999" unit-test-colours))
      (setq unit-test-colours (acons "dark-gray" "#666666" unit-test-colours))
      (setq unit-test-running-xpm (unit-test-dot "gray"))
      (server-start)
      (defun autotest-update (status)
        "Updates all buffer's modeline with the current test status."
        (interactive "S")
        (let ((autotest-map (make-sparse-keymap)))
          (define-key autotest-map [mode-line mouse-1] 'autotest-switch)
          (mapcar (lambda (buffer)
                    (with-current-buffer buffer
                      (if (eq status 'quit)
                          (show-test-none)
                        (progn
                          (show-test-status status)
                          (put-text-property
                           0 3
                           'keymap autotest-map
                           (car mode-line-buffer-identification))))))
                  (buffer-list)))
        status))
  (message "unit-test not found, not starting autotest/emacs integration"))

(provide 'autotest)
