;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Aliases: (use sort-paragraphs on this section)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defadvice find-file-at-point (around goto-line compile activate)
  (let ((line (and (looking-at ".*?:\\([0-9]+\\)")
                   (string-to-number (match-string 1)))))
    ad-do-it
    (and line (goto-line line))))

;;;###autoload
(defalias 'lappy 'rwd-lappy)

;;;###autoload
(defalias 'myshell 'rwd-shell)

;;;###autoload
(defmacro def-hook (mode &rest body)
  (message "** def-hook %s" mode)
  `(add-to-list ',(intern (concat (symbol-name mode) "-hook"))
    (defun ,(intern (concat "rwd-" (symbol-name mode) "-hook")) ()
      (message "**** running %s hook" ',mode)
      ,@body)))
(put 'def-hook 'lisp-indent-function 1)

;; ;;;###autoload
;; (defmacro hook-after-load (mode &rest body)
;;   (message "** hook-after-load %s" mode)
;;   ;; (if (fboundp mode) (error "mode already loaded: %s" mode))
;;   `(eval-after-load ',mode
;;      '(def-hook ,mode ,@body)))
;; (put 'hook-after-load 'lisp-indent-function 1)

;;;###autoload
(defmacro hook-after-load-new (mode hookname &rest body)
  (message "** hook-after-load %s" mode)
  ;; (if (fboundp mode) (error "mode already loaded: %s" mode))
  `(eval-after-load ',mode
     '(def-hook ,(or hookname mode) ,@body)))
(put 'hook-after-load-new 'lisp-indent-function 1)

;; ;;;###autoload
;; (defmacro hook-mode-after-load (mode &rest body)
;;   (let ((modename (intern (concat (symbol-name mode) "-mode"))))
;;     (message "** hook-mode-after-load %s" modename)
;;     `(eval-after-load ',modename
;;        '(progn
;;           (message "**** adding %s hook" ',modename)
;;           (def-hook ,modename ,@body)))))
;; (put 'hook-mode-after-load 'lisp-indent-function 1)

;;;###autoload
(defun list-join (sep lst)
  (mapconcat (lambda (x) x) lst sep))

;;;###autoload
(defun munge-newlines (start end from to)
  (save-excursion
    (save-match-data
      (goto-char start)
      (let ((case-fold-search nil))
        (while (re-search-forward from end t)
          (replace-match to t t))))))

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

;;;###autoload
(defun rwd-emacs-wiki ()
  (interactive)
  (dired (concat "~/Sites/emacs/static/")))

;;;###autoload
(defun rwd-find-project-files ()
  (split-string (shell-command-to-string (concat "find " (list-join " " rwd-project-dirs) " -name \\*.rb -o -name \\*.el -o -name \\*.y"))))

;;;###autoload
(defun rwd-forward-line-6 ()
  (interactive)
  (forward-line 6))

;;;###autoload
(defun rwd-insert-shebang ()
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

;;;###autoload
(defun rwd-lappy ()
  (interactive)
  (rwd-resize-13)
  (rwd-shell)
  (rwd-swap-buffers))

;;;###autoload
(defun rwd-newlines-escape (start end)
  (interactive "r")
  (munge-newlines start end "\n" "\\n"))

;;;###autoload
(defun rwd-newlines-unescape (start end)
  (interactive "r")
  (munge-newlines start end "\\\\n" "\n"))

;;;###autoload
(defun rwd-occur-buffer ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "occur -n -p")))

;;;###autoload
(defun rwd-previous-line-6 ()
  (interactive)
  (previous-line 6))

;;;###autoload
(defun rwd-quickref ()
  (interactive)
  (find-file-other-window (expand-file-name "~/Work/p4/zss/www/zenspider.com/data/Languages/Ruby/QuickRef")))

;;;###autoload
(defun rwd-read-this ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (follow-mode))

;;;###autoload
(defun rwd-resize-13 (&optional nosplit)
  (interactive "P")
  (rwd-set-mac-font "DejaVu Sans Mono" 12)
  (rwd-arrange-frame 170 48 nosplit))

;;;###autoload
(defun rwd-resize-13-dense (&optional nosplit)
  "Yet another screen layout. Suitable for 13in but denser than medium."
  (interactive "P")
  (rwd-set-mac-font "DejaVu Sans Mono" 10)
  (rwd-arrange-frame 200 52 nosplit))

;;;###autoload
(defun rwd-resize-20 (&optional nosplit)
  "Create a really large window suitable for coding on a 20 inch cinema display."
  (interactive "P")
  (rwd-set-mac-font "DejaVu Sans Mono" 12)
  (rwd-arrange-frame 200 60 nosplit))

;;;###autoload
(defun rwd-resize-peepcode ()
  "Create a small font window suitable for doing live demos in 800x600."
  (interactive)
  (rwd-arrange-frame 80 30 t)
  (rwd-set-mac-font "DejaVu Sans Mono" 15))

;;;###autoload
(defun rwd-resize-presentation ()
  "Create a giant font window suitable for doing live demos."
  (interactive)
  (rwd-arrange-frame 80 25 t)
  (rwd-set-mac-font "DejaVu Sans Mono" 20))

;;;###autoload
(defun rwd-resize-small (&optional split)
  "Create a small window suitable for coding on anything."
  (interactive "P")
  (rwd-set-mac-font "DejaVu Sans Mono" 12)
  (rwd-arrange-frame 80 48 (not split)))

;;;###autoload
(defun rwd-insert-arrange-frame ()
  (interactive)
  (insert (format "%s" (list 'rwd-arrange-frame (frame-width) (frame-height)))))

;;;###autoload
(defun rwd-scroll-down ()
  (interactive)
  (scroll-up 1))

;;;###autoload
(defun rwd-scroll-top ()
  (interactive)
  (recenter 0))

;;;###autoload
(defun rwd-scroll-up ()
  (interactive)
  (scroll-down 1))

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
(defun rwd-tabs (&optional width)
  "Set tabbing to spaces at 2 col tabstops."
  (interactive "p")
  (setq tab-width (or width 2) indent-tabs-mode nil)
  (save-excursion
    (mark-whole-buffer)
    (untabify (point-min) (point-max))))

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

;;;###autoload
(defun server-stop ()
  "Stop the server"
  (interactive)
  (server-start t))

;; (defun my-get-mac-font ()
;;   (list (face-attribute 'default :family)
;;         (/ (face-attribute 'default :height) 10)))

;; (defun my-indent-whole-buffer ()
;;   "indent whole buffer"
;;   (interactive)
;;   (delete-trailing-whitespace)
;;   (indent-region (point-min) (point-max) nil)
;;   (untabify (point-min) (point-max)))

;; (defun my-ruby-sexp (start end)
;;   (interactive "r")
;;   (save-excursion
;;     (save-match-data
;;       (replace-regexp "]" ")" nil start end)
;;       (replace-regexp "\\[" "s(" nil start end))))

;; (defun my-selective-display (column)
;;   "Rotate folding the buffer at no, 2, 4, 6, and 8 columns."
;;   (interactive "P")
;;   (set-selective-display
;;    (if (< (or selective-display 0) 8)
;;        (or column (+ (or selective-display 0) 2))
;;      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Borrowed" defuns:

(defun split-horizontally-not-vertically ()
  "If there's only one window (excluding any possibly active
     minibuffer), then split the current window horizontally."
  (interactive)
  (if (and
       (>= (frame-width) (* 2 (frame-height)))
       (= (length (window-list nil 'dont-include-minibuffer-even-if-active)) 1))
      (split-window-horizontally)))
(add-hook 'temp-buffer-setup-hook 'split-horizontally-not-vertically)

(defun rwd-sudo-buffer ()
  "Revert buffer using tramp sudo.
    This will also reserve changes already made by a non-root user."
  (interactive)
  (let ((f (buffer-file-name)))
    (when f
      (let ((content (when (buffer-modified-p)
                       (widen)
                       (buffer-string))))
        (if (file-writable-p f)
            (revert-buffer)
          (kill-buffer (current-buffer))
          (find-file (concat "/sudo::" f))
          (when content
            (let ((buffer-read-only nil))
              (erase-buffer)
              (insert content))))))))

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
