;;;###autoload
(defcustom flog/colors
  '((10.0 . "#ffffff")                    ; TODO: log?
    (12.5 . "#ffeeee")
    (15.0 . "#ffdddd")
    (17.5 . "#ffcccc")
    (20.0 . "#ffbbbb")
    (25.0 . "#ffaaaa")
    (30.0 . "#ff9999")
    (35.0 . "#ff8888")
    (40.0 . "#ff7777")
    (45.0 . "#ff6666")
    (50.0 . "#ff5555")
    (55.0 . "#ff4444")
    (60.0 . "#ff3333")
    (65.0 . "#ff2222")
    (70.0 . "#ff1111")
    (75.0 . "#ff0000"))
  "Max score (or t for default) to color mapping."
  :group 'flog)

;; (custom-reevaluate-setting 'flog/colors)
;; (describe-variable 'flog/colors)
;; (customize-variable 'flog/colors)

(defun shell-command-on-region-to-string (command)
  (let ((buffer (current-buffer))
        (resize-mini-windows nil)
        (max-mini-window-height 0))
   (with-temp-buffer
     (let ((temp-buf (current-buffer)))
       (with-current-buffer buffer
         (shell-command-on-region (point-min) (point-max) command temp-buf))
       (buffer-string)))))

(defun buffer-to-flog ()
  (let* ((beg 0)
         (m nil)
         (re (rx (group (+ digit) "." (+ digit)) ; score
                 ": "
                 (+? graph) "#" (+ graph)        ; name
                 (+ whitespace)
                 (group (+ (not (any ":"))))     ; path
                 ":"
                 (group (+ digit))               ; start-line
                 "-"
                 (group (+ digit))))             ; end-line
         (path (shell-quote-argument buffer-file-name))
         (flog (shell-command-on-region-to-string "flog -a -")))
    (while (string-match re flog beg)
      (setq m (cons (list (string-to-number (match-string-no-properties 3 flog))
                          (string-to-number (match-string-no-properties 4 flog))
                          (string-to-number (match-string-no-properties 1 flog)))
                    m)
            beg (match-end 0)))
    m))

(defun flog-color (n)
  (assoc-default n flog/colors (lambda (limit val)
                                 (or (booleanp limit)
                                     (< val limit)))))

;;;###autoload
(defun flog ()
  (interactive)

  (save-excursion
    (save-match-data
     (let ((flog (buffer-to-flog)))
       (remove-overlays)

       (dolist (result flog)
         (let* ((start (car result))
                (stop  (+ (cadr result) 2)) ; +2 to grab end line
                (score (caddr result))
                (ppl   (/ score (- stop start 1))) ; TODO: instead of score?
                (color (flog-color score))
                (ov (make-overlay (line-beginning-position start)
                                  (line-end-position stop))))
           (goto-char (point-min))

           (overlay-put ov 'help-echo (format "flog = %0.2f" score))
           (overlay-put ov 'face (cons 'background-color color))))))))

;;;###autoload
(defun flog/details ()
  (interactive)

  (let* ((path (shell-quote-argument buffer-file-name))
         (cmd (concat "flog -d -a " path)))
    (shell-command-on-region (point-min) (point-max) "flog -d -a -")))

;;;###autoload
(defun flog/clear ()
  (interactive)
  (remove-overlays))

;; (with-current-buffer "zombies.rb"
;;   (flog))
;;
;; (cl-prettyprint
;;  (with-current-buffer "zombies.rb"
;;    (mapcar (lambda (result)
;;              (let* ((start (car result))
;;                     (stop  (cadr result))
;;                     (score (caddr result))
;;                     (ppl   (/ score (- stop start 1)))
;;                     (color (flog-color score)))
;;                (list start stop score ppl color)))
;;            (buffer-to-flog))))

(provide 'flog)
