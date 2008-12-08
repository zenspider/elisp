;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Aliases: (use sort-paragraphs on this section)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defadvice find-file-at-point (around goto-line compile activate)
  (let ((line (and (looking-at ".*:\\([0-9]+\\)")
                   (string-to-number (match-string 1)))))
    ad-do-it
    (and line (goto-line line))))

;;;###autoload
(defmacro def-hook (mode &rest body)
  `(add-hook
    ',(intern (concat (symbol-name mode) "-hook"))
    (defun ,(intern (concat "my-" (symbol-name mode) "-hook")) ()
      ,@body)))
(put 'def-hook 'lisp-indent-function 1)

;;;###autoload
(defmacro hook-after-load (mode &rest body)
  `(eval-after-load ,(symbol-name mode)
     '(def-hook ,(intern (concat (symbol-name mode) "-mode"))
              ,@body)))
(put 'hook-after-load 'lisp-indent-function 1)

;;;###autoload
(defun rwd-arrange-frame (w h &optional nosplit)
  "Rearrange the current frame to a custom width and height and split unless prefix."
  (let ((frame (selected-frame)))
    ;; (when (or (equal 'mac (framep frame)) (equal 'ns (framep frame)))
    (when (memq (framep frame) '(mac ns))
      (delete-other-windows)
      (set-frame-position frame 5 25)
      (set-frame-size frame w h)
      (if (not nosplit)
          (split-window-horizontally)))))

;; (defun chmod ()
;;   (interactive)
;;   (shell-command (format "chmod %s %s"
;;                          (read-from-minibuffer "Mode string?: " "u+x")
;;                          (buffer-file-name))))

;; (defun clean-whitespace ()
;;   (interactive)
;;   (save-excursion
;;     (save-restriction
;;       (save-match-data
;;     (progn
;;       (delete-trailing-whitespace)
;;       (goto-char (point-min))
;;       (while (re-search-forward "^[ \t]+" nil t)
;;         (delete-region (match-beginning 0) (match-end 0)))
;;       (goto-char (point-min))
;;       (while (re-search-forward " +" nil t)
;;         (delete-region (+ 1 (match-beginning 0)) (match-end 0)))
;;       (goto-char (point-min))
;;       (while (re-search-forward "\n\n+" nil t)
;;         (delete-region (+ 2 (match-beginning 0)) (match-end 0)))
;;       (goto-char (point-min))
;;       (while (looking-at "\n")
;;         (delete-char 1))
;;       (goto-char (- (point-max) 1))
;;       (while (looking-at "\n")
;;         (delete-char 1)))))))

;; (defun escape-newlines (start end)
;;   (interactive "r")
;;   (munge-newlines start end "\n" "\\n"))

;; (defun grep-current-word ()
;;   "Grep for the current word"
;;   (interactive)
;;   (let ((current-prefix-arg t))
;;     (call-interactively 'grep)))

;; (defun insert-buffer-name()
;;   "Insert the value of buffer-name."
;;   (interactive)
;;   (progn
;;     (insert (buffer-name))))

;; (defun insert-date ()
;;   "Insert today's date."
;;   (interactive)
;;   (insert (shell-command-to-string "today")))

;; (defun insert-modeline ()
;;   "Insert the current modeline into the file."
;;   (interactive)
;;   (let ((mode (symbol-name major-mode)))
;;     (insert "-*- ")
;;     (comment-region (line-beginning-position) (line-end-position))
;;     (insert (substring mode 0 (- (length mode) 5)))
;;     (insert " -*-")
;;     (insert "\n")))

;; (defun insert-path (path)
;;   (interactive "G")
;;   (insert (expand-file-name path)))

;; (defun insert-shebang ()
;;   "Insert a shebang line based on mode into the file."
;;   (interactive)
;;   (let* ((mode (symbol-name major-mode))
;;          (prg  (substring mode 0 (- (length mode) 5)))
;;          (path (shell-command-to-string (concat "which " prg))))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (insert "#!")
;;       (insert path)
;;       (insert "\n"))))

;;;###autoload
(defun rwd-lappy ()
  (interactive)
  (rwd-resize-13)
  (rwd-shell)
  (rwd-swap-buffers))

(defalias 'lappy 'rwd-lappy)

;; (defun list-join (sep lst)
;;   (mapconcat (lambda (x) x) lst sep))

;; (defun locate-make-mdfind-command-line (search-string)
;;   (list "mdfind" (concat "kMDItemDisplayName=*" search-string "*")))

;; (defun modify-tabs (length enabled)
;;   (progn
;;     (setq tab-width length indent-tabs-mode enabled)
;;     (save-excursion
;;       (mark-whole-buffer)
;;       (if enabled
;;           (tabify (mark) (point))
;;           (untabify (mark) (point))))))

;; (defun munge-newlines (start end from to)
;;   (save-excursion
;;     (save-match-data
;;       (goto-char start)
;;       (let ((case-fold-search nil))
;;         (while (re-search-forward from end t)
;;           (replace-match to t t))))))

;;;###autoload
(defun rwd-emacs-wiki ()
  (interactive)
  (dired (concat "~/Sites/emacs/static/")))

;; (defun my-eval-and-replace ()
;;   "Replace the preceding sexp with its value."
;;   (interactive)
;;   (backward-kill-sexp)
;;   (condition-case nil
;;       (prin1 (eval (read (current-kill 0)))
;;              (current-buffer))
;;     (error (message "Invalid expression")
;;            (insert (current-kill 0)))))

;; (defun my-get-mac-font ()
;;   (list (face-attribute 'default :family)
;;         (/ (face-attribute 'default :height) 10)))

;; (defun my-indent-whole-buffer ()
;;   "indent whole buffer"
;;   (interactive)
;;   (delete-trailing-whitespace)
;;   (indent-region (point-min) (point-max) nil)
;;   (untabify (point-min) (point-max)))

;; (defun my-insert-line-count ()
;;   (interactive)
;;   (let ((msg (count-lines-region (mark) (point))))
;;     (move-end-of-line '())
;;     (insert " ")
;;     (set-mark (point))
;;     (insert msg)
;;     (comment-region (mark) (point))))

;;;###autoload
(defun rwd-quickref ()
  (interactive)
  (find-file-other-window (expand-file-name "~/Work/p4/zss/www/zenspider.com/data/Languages/Ruby/QuickRef")))

;; (defun my-read-this ()
;;   (interactive)
;;   (delete-other-windows)
;;   (split-window-horizontally)
;;   (follow-mode))

;;;###autoload
(defun my-recompile-init ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/Bin/elisp") 0))

;; (defun my-record-current-window ()
;;   (interactive)
;;   (delete-other-windows)
;;   (let ((font (my-get-mac-font)))
;;     (insert (pp `(defun xxx (&optional nosplit)
;;                    (interactive "P")
;;                    (my-set-mac-font ,(car font) ,(cadr font))
;;                    (rwd-arrange-frame ,(window-width) ,(+ 1 (window-height)) nosplit))))))

;; (defun my-reset-macro-counter (n)
;;   "Set kmacro-counter to \\[universal-argument] prefix's value or 1 by default"
;;   (interactive "p")
;;   (setq kmacro-counter (or n 1)))

;; (defun my-ruby-massage ()
;;   (interactive)
;;   (save-excursion
;;     (replace-string "#<" "["  nil (point-min) (point-max))
;;     (replace-string ">"  "]"  nil (point-min) (point-max))
;;     (replace-string "=]" "=>" nil (point-min) (point-max))))

;; (defun my-ruby-sexp (start end)
;;   (interactive "r")
;;   (save-excursion
;;     (save-match-data
;;       (replace-regexp "]" ")" nil start end)
;;       (replace-regexp "\\[" "s(" nil start end))))

;; (defun my-selective-display (column)
;;   "Rotate folding the buffer at no, 2, 4, and 6 columns."
;;   (interactive "P")
;;   (set-selective-display
;;    (if (< (or selective-display 0) 6)
;;        (or column (+ (or selective-display 0) 2))
;;      nil)))

;;;###autoload
(defun rwd-set-mac-font (name size)
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

;;;###autoload
(defun rwd-shell ()
  "Create a shell buffer that is properly named (shell-<N>)"
  (interactive)
  (let* ((name "shell")
         (count 1)
         (buffer-name (format "%s-%d" name count)))
    (while (get-buffer buffer-name)
      (set 'buffer-name (format "%s-%d" name count))
      (set 'count (+ count 1)))
    (shell buffer-name)))

;;;###autoload
(defalias 'myshell 'rwd-shell)

;;;###autoload
(defun rwd-tabs (&optional width)
  "Set tabbing to spaces at 2 col tabstops."
  (interactive "p")
  (setq tab-width (or width 2) indent-tabs-mode nil)
  (save-excursion
    (mark-whole-buffer)
    (untabify (point-min) (point-max))))

;;;###autoload
(defun rwd-occur-buffer ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "occur -n -p")))

;;;###autoload
(defun rwd-resize-13 (&optional nosplit)
  (interactive "P")
  (rwd-set-mac-font "apple-bitstream vera sans mono" 12)
  (rwd-arrange-frame 170 50 nosplit))

;;;###autoload
(defun rwd-resize-13-dense (&optional nosplit)
  "Yet another screen layout. Suitable for 13in but denser than medium."
  (interactive "P")
  (rwd-set-mac-font "bitstream vera sans mono" 10)
  (rwd-arrange-frame 200 62 nosplit))

;;;###autoload
(defun rwd-resize-20 (&optional nosplit)
  "Create a really large window suitable for coding on a 20 inch cinema display."
  (interactive "P")
  (rwd-set-mac-font "bitstream vera sans mono" 12)
  (rwd-arrange-frame 200 60 nosplit))

;;;###autoload
(defun rwd-resize-peepcode ()
  "Create a small font window suitable for doing live demos in 800x600."
  (interactive)
  (rwd-arrange-frame 80 30 t)
  (rwd-set-mac-font "bitstream vera sans mono" 15))

;;;###autoload
(defun rwd-resize-presentation ()
  "Create a giant font window suitable for doing live demos."
  (interactive)
  (rwd-arrange-frame 80 25 t)
  (rwd-set-mac-font "bitstream vera sans mono" 20))

;;;###autoload
(defun rwd-resize-small (&optional split)
  "Create a small window suitable for coding on anything."
  (interactive "P")
  (rwd-set-mac-font "bitstream vera sans mono" 12)
  (rwd-arrange-frame 80 45 (not split)))

;;;###autoload
(defun rwd-forward-line-6 ()
  (interactive)
  (forward-line 6))

;;;###autoload
(defun rwd-previous-line-6 ()
  (interactive)
  (previous-line 6))

;;;###autoload
(defun rwd-scroll-down ()
  (interactive)
  (scroll-up 1))

;;;###autoload
(defun rwd-scroll-up ()
  (interactive)
  (scroll-down 1))

;;;###autoload
(defun rwd-scroll-top ()
  (interactive)
  (recenter 0))

;;;###autoload
(defun server-stop ()
  "Stop the server"
  (interactive)
  (server-start t))

;;;###autoload
(defun rwd-swap-buffers ()
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

;;;###autoload
(defun rwd-toggle-split ()
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
      (switch-to-buffer-other-window nb))))

;; ;; TODO: prefix with my- or something so I use this
;; (defun un-camelcase-region (start end)
;;   (interactive "r")
;;   (save-excursion
;;     (save-match-data
;;       (goto-char start)
;;       (let ((case-fold-search nil))
;;         (while (re-search-forward "\\([a-z]\\)\\([A-Z]\\)" end t)
;;           (replace-match (concat (match-string 1)
;;                                  "_"
;;                                  (downcase (match-string 2))) t))))))

;; (defun unescape-newlines (start end)
;;   (interactive "r")
;;   (munge-newlines start end "\\\\n" "\n"))

;; (defun unfill-paragraph ()
;;   (interactive)
;;   (let ((fill-column (point-max)))
;;     (fill-paragraph nil)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; "Borrowed" defuns:

;; (defun sacha/decrease-font-size ()
;;   (interactive)
;;   (set-face-attribute 'default
;;                       nil
;;                       :height
;;                       (floor (* 0.9
;;                                   (face-attribute 'default :height)))))

;; (defun sacha/increase-font-size ()
;;   (interactive)
;;   (set-face-attribute 'default
;;                       nil
;;                       :height
;;                       (ceiling (* 1.10
;;                                   (face-attribute 'default :height)))))

;; (defun sudo-buffer ()
;;   "Revert buffer using tramp sudo.
;;     This will also reserve changes already made by a non-root user."
;;   (interactive)
;;   (let ((f (buffer-file-name)))
;;     (when f
;;       (let ((content (when (buffer-modified-p)
;;                        (widen)
;;                        (buffer-string))))
;;         (if (file-writable-p f)
;;             (revert-buffer)
;;           (kill-buffer (current-buffer))
;;           (find-file (concat "/sudo::" f))
;;           (when content
;;             (let ((buffer-read-only nil))
;;               (erase-buffer)
;;               (insert content))))))))

;; (face-attribute 'default :height)

;;; stolen from: http://www.emacswiki.org/cgi-bin/wiki/IndentRigidlyN
;;;###autoload
(defun indent-rigidly-n (n)
  "Indent the region, or otherwise the current line, by N spaces."
  (let* ((use-region (and transient-mark-mode mark-active))
         (rstart (if use-region (region-beginning) (point-at-bol)))
         (rend   (if use-region (region-end)       (point-at-eol)))
         (deactivate-mark "irrelevant")) ; avoid deactivating mark
    (indent-rigidly rstart rend n)))

;;;###autoload
(defun indent-rigidly-4 ()
  "Indent the region, or otherwise the current line, by 4 spaces."
  (interactive)
  (indent-rigidly-n 4))

;;;###autoload
(defun outdent-rigidly-4 ()
  "Indent the region, or otherwise the current line, by -4 spaces."
  (interactive)
  (indent-rigidly-n -4))
