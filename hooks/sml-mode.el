(defun sml--read-run-cmd ()             ; override to standard defaults
  '("sml" "" ""))                       ; TODO: fall back to prompt if option

(defun rwd-sml-send-buffer-new ()
  (interactive)

  (let ((wtf-buffer (current-buffer)))
   (save-excursion
     (when (and (buffer-live-p sml-prog-proc--buffer)
                (get-buffer-process sml-prog-proc--buffer))
       (with-current-buffer sml-prog-proc--buffer
         (delete-process (get-buffer-process sml-prog-proc--buffer))
         (erase-buffer)
         (kill-buffer sml-prog-proc--buffer)))

     (sml-prog-proc-send-buffer nil))
   (pop-to-buffer wtf-buffer)))         ; TODO: why do I need pop-to-buffer ?

(define-key sml-mode-map (kbd "C-c C-c C-b") 'rwd-sml-send-buffer-new)
