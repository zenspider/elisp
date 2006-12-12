;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;; Takes a multi-line paragraph and makes it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun current-frame ()
  (car (car (last (cdr (current-frame-configuration))))))

(defun arrange-frame (w h &optional nosplit)
  "Rearrange the current frame to a custom width and height and split unless prefix."
  (let ((frame (current-frame)))
    (progn
      (delete-other-windows)
      (set-frame-position frame 5 25)
      (set-frame-size frame w h)
      (if (not nosplit)
          (split-window-horizontally)))))

(defun small (&optional split)
  "Create a small window suitable for coding on anything"
  (interactive "P")
  (arrange-frame 80 45 (not split)))

(defun medium (&optional nosplit)
  "Create a large window suitable for coding on a macbook"
  (interactive "P")
  (arrange-frame 170 45 nosplit))

(defun huge (&optional nosplit)
  "Create a really large window suitable for coding on a 20 inch cinema display"
  (interactive "P")
  (arrange-frame 199 60 nosplit))

(defun my-recompile-init ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/Bin/elisp") 0))

(if (featurep 'xemacs)
    (defadvice yank (after indent-region activate)
      (if (member major-mode '(emacs-lisp-mode
                               c-mode
                               c++-mode
                               tcl-mode
                               sql-mode
                               perl-mode
                               cperl-mode
                               java-mode
                               jde-mode
                               ruby-mode
                               LaTeX-mode
                               TeX-mode))
          (indent-region (region-beginning) (region-end) nil))))

(defun myshell ()
  "Create a shell buffer that is properly named (shell-<N>)"
  (interactive)
  (shell)		  ; use these two instead of (shell "shell")
  (rename-buffer "shell") ; as it plays with multipl windows better
  (rename-uniquely))

(defun hard-tabs ()
  "Set tabbing in current buffer so only hard tabs are inserted into file, but 4 col. tabstops are still observed."
  (interactive)
  (progn
    (setq tab-width 4)
    (setq indent-tabs-mode t)
    (save-excursion
      (tabify (mark) (point)))))

(defun mytabs()
  "Set tabbing back to the way I like it."
  (interactive)
  (progn
    (setq tab-width 4 indent-tabs-mode nil)
    (save-excursion
      (mark-whole-buffer)
      (untabify (mark) (point)))))

(defun mytabs2()
  "Set tabbing back to the way I like it."
  (interactive)
  (progn
    (setq tab-width 2 indent-tabs-mode nil)
    (save-excursion
      (mark-whole-buffer)
      (untabify (mark) (point)))))

(defun toggle-split ()
  "Toggle vertical/horizontal window split."
  (interactive)
  (if (one-window-p)
      (error "Frame doesn't have two windows")
    (let* ((cw (selected-window))
           (nw (next-window cw))
           (wf (window-frame cw))
           ;;(cb (buffer-name (window-buffer cw)))
           (nb (buffer-name (window-buffer nw)))
           (sv (if (eq (window-width cw) (frame-width wf))
                   t nil)))
      (delete-window nw)
      (split-window cw nil sv)
      (switch-to-buffer-other-window nb)
      )))

(defun swap-buffers ()
  "Swap the current 2 buffers in their windows"
  (interactive)
  (if (one-window-p)
      (error "Frame doesn't have two windows")
    (let* ((cw (selected-window))
           (nw (next-window cw))
           (cb (buffer-name (window-buffer cw)))
           (nb (buffer-name (window-buffer nw))))
      (switch-to-buffer nb)
      (select-window nw)
      (switch-to-buffer cb))))

(defun insert-buffer-name()
  "Insert the value of buffer-name"
  (interactive)
  (progn
    (insert (buffer-name))))

(defun reload-safari ()
  (interactive)
  (shell-command "printf 'tell application \"System Events\"\nclick button \"Stop\" of first window of process \"Safari\"\nend tell' | osascript" nil nil))

(defun clean-whitespace ()
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
	(progn
	  (delete-trailing-whitespace)
	  (goto-char (point-min))
	  (while (re-search-forward "^[ \t]+" nil t)
	    (delete-region (match-beginning 0) (match-end 0)))
	  (goto-char (point-min))
	  (while (re-search-forward " +" nil t)
	    (delete-region (+ 1 (match-beginning 0)) (match-end 0)))
	  (goto-char (point-min))
	  (while (re-search-forward "\n\n+" nil t)
	    (delete-region (+ 2 (match-beginning 0)) (match-end 0)))
	  (goto-char (point-min))
	  (while (looking-at "\n")
	    (delete-char 1))
	  (goto-char (- (point-max) 1))
	  (while (looking-at "\n")
	    (delete-char 1)))))))

(defun forward-line-6 ()
  (interactive)
  (forward-line 6))

(defun previous-line-6 ()
  (interactive)
  (previous-line 6))
