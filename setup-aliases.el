;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'macro-to-end (read-kbd-macro "C-u 0 C-x e"))
(defalias 'shell-on-region (read-kbd-macro "C-u M-|"))
(defalias 'kill-buffer-and-close-window
  (read-kbd-macro "C-x k RET C-x 0"))

(defun hard-tabs ()
  "Set tabbing in current buffer so only hard tabs are inserted into file, but 4 col. tabstops are still observed."
  (interactive)
  (progn
    (setq tab-width 4)
    (setq indent-tabs-mode t)
    (save-excursion
      (tabify (mark) (point)))))

(defun my-tabs()
  "Set tabbing back to the way I like it."
  (interactive)
  (progn
    (setq tab-width 4)
    (setq indent-tabs-mode nil)
    (save-excursion
      (mark-whole-buffer)
      (untabify (mark) (point)))))


(defalias 'clean-whitespace (read-kbd-macro
"<f12> [ SPC C-q TAB ]+ SPC <backspace> $ 2*RET ! ESC < <f12> C-q C-j C-q C-j + RET C-q C-j C-q C-j RET ! ESC <"))
  
(defalias 'cvs-remove-diff (read-kbd-macro
"<down> 4*C-k NUL C-s ============ RET <right> <down> C-x C-x C-w"))

;; Java Aliases

(defalias 'java-getter (read-kbd-macro
"TAB public SPC <C-right> <right> M-d C-y () SPC { C-d RET TAB return SPC C-y ; RET } RET 3*<up> 2*<C-right> <right> 2*C-d M-l C-a 4*<down>"))

(defalias 'java-setter (read-kbd-macro
"TAB public SPC void SPC M-d C-d <C-right> ( C-y SPC newValue) SPC { RET TAB = SPC newValue C-e RET } RET C-a 3*<up> 2*<C-right> M-d C-y C-a <down> TAB C-y SPC C-a <up> 2*<C-right> <right> 2*C-d M-l C-a 4*<down>"))

(defalias 'java-convert-assert (query-replace-regexp "QT.assert(\\([^,]+\\),[^\"]+\\(\"[^\"]+\"\\));" "assert(\\2, \\1);" nil))

(defalias 'java-munge-test (read-kbd-macro
"5*<down> import SPC suntest.javaspec.runtime.*; M-% suntest.javaspec.runtime. 2*RET ! M-<"))

(defun java-classname()
  "Return the name of the public class for that file. Calculates using the buffer name only."
  (interactive)
  (let* ((myFileName (buffer-name))
	 (x (- (length myFileName) 5))
	 (dot-java (substring myFileName x (length myFileName)))
	 (className (if (string-equal ".java" dot-java)
			(substring myFileName 0 x)
		      myFileName)))
    className))

(defun java-new-class()
  "Insert the body of a new class"
  (interactive)
  (let ((className (java-classname)))
    (progn
      (insert "public class " className " {

  public static void main(String [] args) {
    " className " obj = new " className "\(\);
    obj.run\(\);
  }

  public void run\(\) {
  }
}"))))

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
