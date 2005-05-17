;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defalias 'macro-to-end (read-kbd-macro "C-u 0 C-x e"))
; (defalias 'shell-on-region (read-kbd-macro "C-u M-|"))
;(defalias 'kill-buffer-and-close-window
;  (read-kbd-macro "C-x k RET C-x 0"))

; (load "simple")

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun my-fix-window ()
  (interactive)

  (let ((frame (car (car (cdr (current-frame-configuration))))))
    (progn
      (delete-other-windows)
      (set-frame-position frame 5 25)
      (set-frame-size frame 80 45))))

(defun my-big-window ()
  "Create a large window suitable for coding on a 20 inch cinema display"
  (interactive)

  (let ((frame (car (car (cdr (current-frame-configuration))))))
    (progn
      (delete-other-windows)
      (set-frame-position frame 5 25)
      (set-frame-size frame 164 60)
      (split-window-horizontally))))

(defun my-recompile-init ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/Bin/elisp") 0 t))

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
  (shell)
  (rename-buffer "shell")
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

; (defalias 'cvs-remove-diff (read-kbd-macro
; "<down> 4*C-k NUL C-s ============ RET <right> <down> C-x C-x C-w"))

; (defalias 'p4-fill-diff (read-kbd-macro
; "TAB + SPC 2*<left> C-SPC C-e M-q C-k"))

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

(defun insert-shell-command (cmd)
  "Execute and insert results of a command in current buffer."
  (shell-command-on-region (point nil) (point nil) cmd nil t))

(defun insert-shell-command-interactive (s)
  "Execute and insert results of a command in current buffer."
  (interactive "sShell Command: ")
  (insert-shell-command s))

(defun windoze-sucks ()
  (interactive)
  (set-buffer-file-coding-system 'unix))

(defun reload-safari ()
  (interactive)
  (shell-command "printf 'tell application \"System Events\"\nclick button \"Stop\" of first window of process \"Safari\"\nend tell' | osascript" nil nil))

(defun clean-whitespace ()
  (interactive)
  (progn
    (beginning-of-buffer)
    (replace-regexp "[\ \t]+$" "" nil)
    (beginning-of-buffer)
    (replace-regexp "\n\n+" "\n\n" nil)))

(defun insert-isodate ()
  "Inserts an ISO-8601 compliant date string into the current buffer"
  (interactive)
  (insert-shell-command "date +%Y-%m-%d")
  (delete-backward-char 1))


(defun zentest ()
  "Runs zentest on the current buffer"
  (interactive)
  (shell-command (concat "./ZenTest.rb " buffer-file-name)))