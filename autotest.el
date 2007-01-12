;; autotest.el
;
; sets up an autotest buffer and provides conveniencte methods 

(require 'shell)

(defun autotest ()
  (interactive)
  (let ((buffer (shell "autotest")))
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
           ; ("^ *\\[?\\([^:\\n\\r]+\\):\\([0-9]+\\):in" 1 2)
           ))

    (compilation-shell-minor-mode)
    (comint-send-string buffer "autotest\n")))

(defun autotest-switch ()
  (interactive)
  (if (equal "autotest" (buffer-name))
      (switch-to-buffer nil)
    (switch-to-buffer "autotest")))

;; (defun rails-find-and-goto-error ()
;;   "Finds error in rails html log go on error line" 
;;   (interactive)
;;   (search-forward-regexp "RAILS_ROOT: \\([^<]*\\)")
;;   (let ((rails-root (concat (match-string 1) "/")))
;;     (search-forward "id=\"Application-Trace\"")
;;     (search-forward "RAILS_ROOT}")
;;     (search-forward-regexp "\\([^:]*\\):\\([0-9]+\\)")
;;     (let  ((file (match-string 1))
;;            (line (match-string 2)))
;;       ;; (kill-buffer (current-buffer))
;;       (message
;;        (format "Error found in file \"%s\" on line %s. "  file line))
;;       (find-file (concat rails-root file))
;;       (goto-line (string-to-int line)))))

(provide 'autotest)
