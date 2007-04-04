;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun arrange-frame (w h &optional nosplit)
  "Rearrange the current frame to a custom width and height and split unless prefix."
  (let ((frame (selected-frame)))
    (when (equal 'mac (framep frame))
      (delete-other-windows)
      (set-frame-position frame 5 25)
      (set-frame-size frame w h)
      (if (not nosplit)
          (split-window-horizontally)))))

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

(defun expand-parse (name l &optional str pos)
  (cond ((null l)
         (list name str (reverse pos)))
        ((equal 'n (car l))
         (expand-parse name (cdr l) (concat str "\n")
                       (cons (1+ (length str)) pos)))
        ((equal 'p (car l))
         (expand-parse name (cdr l) str (cons (1+ (length str)) pos)))
        (t (expand-parse name (cdr l) (concat str (car l)) pos))))

(defun forward-line-6 ()
  (interactive)
  (forward-line 6))

(defun grep-current-word ()
  "Grep for the current word"
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'grep)))

(defun huge (&optional nosplit)
  "Create a really large window suitable for coding on a 20 inch cinema display."
  (interactive "P")
  (my-set-mac-font "bitstream vera sans mono" 12)
  (arrange-frame 200 60 nosplit))

(defun hugeish (&optional nosplit)
  "Yet another screen layout. Suitable for 13in but denser than medium."
  (interactive "P")
  (my-set-mac-font "bitstream vera sans mono" 10)
  (arrange-frame 200 62 nosplit))

(defun insert-buffer-name()
  "Insert the value of buffer-name."
  (interactive)
  (progn
    (insert (buffer-name))))

(defun insert-modeline ()
  "Insert the current modeline into the file."
  (interactive)
  (let ((mode (symbol-name major-mode)))
    (insert "-*- ")
    (comment-region (line-beginning-position) (line-end-position))
    (insert (substring mode 0 (- (length mode) 5)))
    (insert " -*-")
    (insert "\n")))

(defun insert-path (path)
  (interactive "G")
  (insert (expand-file-name path)))

(defun insert-shebang ()
  "Insert a shebang line based on mode into the file."
  (interactive)
  (let* ((mode (symbol-name major-mode))
         (prg  (substring mode 0 (- (length mode) 5)))
         (path (shell-command-to-string (concat "which " prg))))
    (save-excursion
      (goto-char (point-min))
      (insert "#!")
      (insert path)
      (insert "\n"))))

(defun list-join (sep lst)
  (mapconcat (lambda (x) x) lst sep))

(defun locate-make-mdfind-command-line (search-string)
  (list "mdfind" (concat "kMDItemDisplayName=*" search-string "*")))

(defun medium (&optional nosplit)
  "Create a large window suitable for coding on a macbook."
  (interactive "P")
  (my-set-mac-font "bitstream vera sans mono" 12)
  (arrange-frame 170 45 nosplit))

(defun modify-tabs (length enabled)
  (progn
    (setq tab-width length indent-tabs-mode enabled)
    (save-excursion
      (mark-whole-buffer)
      (if enabled
          (tabify (mark) (point))
          (untabify (mark) (point))))))

(defun my-emacs-wiki ()
  (interactive)
  (shell-command (concat "open http://localhost/~ryan/emacs/static/" (buffer-name))))

(defun my-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun my-indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun my-generate-new-buffer-name (name)
  "Find a new buffer name not currently used in the form of <name>-<N> where N starts at 1"
  (let* ((count 1)
         (buffer-name (format "%s-%d" name count)))
    (while (get-buffer buffer-name)
      (set 'buffer-name (format "%s-%d" name count))
      (set 'count (+ count 1)))
    buffer-name))

(defun my-read-this ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (follow-mode))

(defun my-recompile-init ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/Bin/elisp") 0))

(defun my-record-current-window ()
  (delete-other-windows)
  (insert (pp `(arrange-frame ,(window-width) ,(+ 1 (window-height)) t))))

(defun my-reset-macro-counter (n)
  "Set kmacro-counter to \\[universal-argument] prefix's value or 1 by default"
  (interactive "p")
  (setq kmacro-counter (or n 1)))

(defun my-selective-display (column)
  "Rotate folding the buffer at no, 2, 4, and 6 columns."
  (interactive "P")
  (set-selective-display
   (if (< (or selective-display 0) 6)
       (or column (+ (or selective-display 0) 2))
     nil)))

(defun my-set-mac-font (name  size)
  (interactive
   (list (completing-read "font-name: "
                          (mapcar (lambda (p) (list (car p) (car p)))
                                  (x-font-family-list)) nil t)
         (read-number "size: " 12)))
  (set-face-attribute 'default nil
                      :family name
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height (* 10 size))
  (frame-parameter nil 'font))

;; to get the current font:
; (frame-parameter nil 'font)

(defun myshell ()
  "Create a shell buffer that is properly named (shell-<N>)"
  (interactive)
  (shell (my-generate-new-buffer-name "shell")))

(defun mytabs()
  "Set tabbing to spaces at 2 col tabstops."
  (interactive)
  (modify-tabs 2 nil))

(defun mytabs-hard ()
  "Set tabbing to real tabs but viewed at 4 col tabstops."
  (interactive)
  (modify-tabs 4 t))

(defun mytabs4()
  "Set tabbing to spaces at 4 col tabstops."
  (interactive)
  (modify-tabs 4 nil))

(defun peepcode ()
  "Create a small font window suitable for doing live demos in 800x600."
  (interactive)
  (arrange-frame 85 34 t)
  (my-set-mac-font "bitstream vera sans mono" 14))

(defun presentation ()
  "Create a giant font window suitable for doing live demos."
  (interactive)
  (arrange-frame 85 25 t)
  (my-set-mac-font "bitstream vera sans mono" 24))

(defun previous-line-6 ()
  (interactive)
  (previous-line 6))

(defun reload-safari ()
  (interactive)
  (shell-command "printf 'tell application \"System Events\"
click button \"Stop\" of first window of process \"Safari\"
end tell' | osascript" nil nil))

(defun server-stop ()
  "Stop the server"
  (interactive)
  (server-start t))

(defun small (&optional split)
  "Create a small window suitable for coding on anything."
  (interactive "P")
  (my-set-mac-font "bitstream vera sans mono" 12)
  (arrange-frame 80 45 (not split)))

(defun spotlight ()
  (interactive)
  (let ((locate-command "mdfind")
        (locate-make-command-line 'locate-make-mdfind-command-line))
    (call-interactively 'locate nil)))

(defun spotlight-full ()
  (interactive)
  (let ((locate-command "mdfind"))
    (call-interactively 'locate nil)))

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

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


(defun occur-buffer ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "occur -n -p")))
