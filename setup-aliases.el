;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'macro-to-end (read-kbd-macro "C-u 0 C-x e"))
(defalias 'shell-on-region (read-kbd-macro "C-u M-|"))
(defalias 'kill-buffer-and-close-window
  (read-kbd-macro "C-x k RET C-x 0"))

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
			   LaTeX-mode
			   TeX-mode))
      (indent-region (region-beginning) (region-end) nil)))

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

(defalias 'cvs-remove-diff (read-kbd-macro
"<down> 4*C-k NUL C-s ============ RET <right> <down> C-x C-x C-w"))

(defalias 'p4-fill-diff (read-kbd-macro
"TAB + SPC 2*<left> C-SPC C-e M-q C-k"))

;; Java Aliases

(defalias 'java-new-copy-constructor (read-kbd-macro
"2*RET 2*<up> TAB public SPC ins 3*<backspace> ESC x insert-buffer-name RET ESC <backspace> <backspace> ( ESC x insert-buffer-name RET ESC <backspace> <backspace> SPC o) S-SPC { RET TAB super(o); 2*RET TAB XXX; RET } C-a 2*<down> 6*<up> C-l"))

(defalias 'java-new-constructor (read-kbd-macro
"2*RET 2*<up> TAB public SPC ESC x insert- buffer- name RET ESC <backspace> <backspace> () S-SPC { RET TAB super(); RET } C-a 2*<up> C-l"))

(defalias 'java-new-clone (read-kbd-macro
"2*RET 2*<up> TAB public SPC Object SPC clone() S-SPC { RET TAB return SPC new SPC ESC x insert- bu TAB - TAB RET ESC <backspace> <backspace> (this); RET } C-a"))

(defalias 'java-setters (read-kbd-macro
"M-d TAB C-s ; RET <backspace> C-a TAB C-SPC C-s SPC RET <left> C-x C-x C-x r s 1 C-x C-x C-w C-d C-SPC C-e C-x r s 2 C-x C-x C-w public SPC void SPC C-x r i 2 C-e ( C-x r i 1 C-e SPC newValue) S-SPC { RET TAB this. C-x r i 2 C-e SPC = SPC newValue; RET } 2*RET TAB public SPC C-x r i 1 C-e SPC C-x r i 2 C-e () S-SPC { RET TAB return SPC this. C-x r i 2 C-e ; RET } 2*RET <backspace> <down>" nil))

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

(defalias 'windoze-sucks
  (read-kbd-macro "ESC < M-% C-q C-m 2*RET ! ESC <"))

(defalias 'clean-whitespace (read-kbd-macro
"<f12> [ SPC C-q TAB ]+ SPC <backspace> $ 2*RET ! ESC < <f12> C-q C-j C-q C-j + RET C-q C-j C-q C-j RET ! ESC <"))
  
