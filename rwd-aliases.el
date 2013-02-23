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
(defadvice imenu (before push-position compile activate)
  (ring-insert find-tag-marker-ring (point-marker)))

;;;###autoload
(progn
  (setq ns-is-fullscreen nil)

  (defadvice ns-toggle-fullscreen (before record-state compile activate)
    (setq ns-is-fullscreen (not ns-is-fullscreen))))

;;;###autoload
(defalias 'big 'rwd-resize-13)

;;;###autoload
(defalias 'huge 'rwd-resize-full)

;;;###autoload
(defalias 'small 'rwd-resize-small)

;;;###autoload
(progn
 (autoload 'grep-ed-start  "grep-ed" nil t)
 (defalias 'grep-edit      'grep-ed-start)
 (defalias 'grep-edit-save 'grep-ed-save-changes-and-exit))

;;;###autoload
(defalias 'repl 'ielm)

;;;###autoload
(defalias 'lappy 'rwd-lappy)

;;;###autoload
(defalias 'myshell 'rwd-shell)

;;;###autoload
(defun align-regexp-= ()
  (interactive)
  (message "align = from %s to %s" (region-beginning) (region-end))
  (save-excursion
    (save-match-data
      (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)="))))

;;;###autoload
(defun head (list n)
  "Return a copy of list with the first n elements"
  (butlast list (- (length list) n)))

;;;###autoload
(defun list-join (sep lst)
  (mapconcat identity lst sep))

;;;###autoload
(defun munge-newlines (start end from to)
  (save-excursion
    (save-match-data
      (goto-char start)
      (let ((case-fold-search nil))
        (while (re-search-forward from end t)
          (replace-match to t t))))))

;;;###autoload
(defun read-file-to-string (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(defun rwd-add-to-load-path (dir)
  "Adds a path to the load-path"
  (interactive "DDirectory: ")
  (add-to-list 'load-path (expand-file-name dir) t))

;;;###autoload
(defun rwd-arrange-frame (w h &optional nosplit)
  "Rearrange the current frame to a custom width and height and split unless prefix."
  (let ((frame (selected-frame)))
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
(defun rwd-forward-line-6 ()
  (interactive)
  (forward-line 6))

(eval-when-compile
  (require 'htmlize))

;;;###autoload
(defun rwd-htmlize-buffer-as-string ()
  (interactive)
  (require 'htmlize)
  (kill-new (htmlize-region-for-paste (point-min) (point-max))))

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
  (rwd-resize-full)
  (shell (rwd-unique-buffer "shell"))) ;; done manually to avoid swap oddities

;;;###autoload
(defun rwd-newlines-escape (start end)
  (interactive "r")
  (munge-newlines start end "\n" "\\n"))

;;;###autoload
(defun rwd-newlines-unescape (start end)
  (interactive "r")
  (munge-newlines start end "\\\\n" "\n"))

;;;###autoload
(defun rwd-ns-fullscreen ()
  (interactive)
  (or ns-is-fullscreen (ns-toggle-fullscreen)))

;;;###autoload
(defun rwd-occur (opt)
  (save-excursion
    (shell-command-on-region (point-min) (point-max)
                             (concat "occur " opt))))

;;;###autoload
(defun rwd-occur-buffer (&optional all)
  (interactive "P")
  (if all
      (rwd-occur "-p")
      (rwd-occur "-p -o")))

;;;###autoload
(defun rwd-occur-n-buffer (&optional all)
  (interactive "P")
  (if all
      (rwd-occur "-p -n")
      (rwd-occur "-p -n -o")))

;;;###autoload
(defun rwd-previous-line-6 ()
  (interactive)
  (forward-line -6))

;;;###autoload
(defun rwd-quickref ()
  (interactive)
  (find-file-other-window (expand-file-name "~/Work/p4/zss/www/zenspider.com/Languages/Ruby/QuickRef.html.md.erb")))

;;;###autoload
(defun rwd-read-this ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (follow-mode))

;;;###autoload
(defun rwd-renumber-list ()
  (interactive)
  (letf ((query-replace-defaults '("\\(.+:\\)" . "\\,(1+ \\#):")))
    (call-interactively 'replace-regexp)))

;;;###autoload
(defun rwd-resize-13 (&optional nosplit)
  (interactive "P")
  (rwd-set-font-size 12)
  (rwd-arrange-frame 163 48 nosplit))

;;;###autoload
(defun rwd-resize-13-dense (&optional nosplit)
  "Yet another screen layout. Suitable for 13in but denser than medium."
  (interactive "P")
  (rwd-set-font-size 10)
  (rwd-arrange-frame 200 52 nosplit))

;;;###autoload
(defun rwd-resize-20 (&optional nosplit)
  "Create a really large window suitable for coding on a 20 inch cinema display."
  (interactive "P")
  (rwd-set-font-size 12)
  (rwd-arrange-frame 200 60 nosplit))

;;;###autoload
(defun rwd-resize-full ()
  (interactive)
  (rwd-set-font-size 14)
  (delete-other-windows)
  (rwd-ns-fullscreen)
  (split-window-horizontally))

(defun rwd-resize-full-blind ()
  "Create a giant font window suitable for doing live demos."
  (interactive)
  (rwd-set-font-size 18)
  (delete-other-windows)
  (rwd-ns-fullscreen))

;;;###autoload
(defun rwd-resize-peepcode ()
  "Create a small font window suitable for doing live demos in 800x600."
  (interactive)
  (rwd-arrange-frame 80 30 t)
  (rwd-set-font-size 15))

;;;###autoload
(defun rwd-resize-presentation ()
  "Create a giant font window suitable for doing live demos."
  (interactive)
  (rwd-arrange-frame 92 34 t)
  (rwd-set-font-size 20))

;;;###autoload
(defun rwd-resize-small (&optional split)
  "Create a small window suitable for coding on anything."
  (interactive "P")
  (rwd-set-font-size 12)
  (rwd-arrange-frame 80 48 (not split)))

;;;###autoload
(defun rwd-split-thirds ()
  "Splits the current frame vertically into even thirds."
  (interactive)
  (split-window-vertically)
  (split-window-vertically)
  (balance-windows))

;;;###autoload
(defun rwd-rotate-windows ()
  "When a process or command brings up their buffer on top of the buffer you were working on and you want to move it into the next window so they're both side by side"
  (interactive)
  (switch-to-buffer (other-buffer))
  (other-window 1)
  (switch-to-buffer (other-buffer)))

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
(defun rwd-set-font-size (size)
  (interactive "nSize: ")
  (rwd-set-mac-font "DejaVu Sans Mono" size))

;;;###autoload
(defun rwd-set-mac-font (name size)
  (interactive
   (list (completing-read "font-name: "
                          (mapcar (lambda (p) (list p p))
                                  (font-family-list)) nil t)
         (read-number "size: " 12)))
  (set-face-attribute 'default nil
                      :family name
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height (* 10 size))
  (frame-parameter nil 'font))

(defun rwd-unique-buffer (name)
  (let* ((count 1)
         (buffer-name (format "%s-%d" name count)))
    (while (get-buffer buffer-name)
      (set 'count (+ count 1))
      (set 'buffer-name (format "%s-%d" name count)))
    buffer-name))

;;;###autoload
(defun rwd-shell ()
  "Create a shell buffer that is properly named (shell-<N>)"
  (interactive)
  (shell (switch-to-buffer-other-window (rwd-unique-buffer "shell"))))

;;;###autoload
(defun sort-files-by-date (a b)
  (let ((ta (nth 5 (file-attributes a)))
        (tb (nth 5 (file-attributes b))))
    (if (= (nth 0 ta) (nth 0 tb))
        (> (nth 1 ta) (nth 1 tb))
      (> (nth 0 ta) (nth 0 tb)))))

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
  ;; taken from http://www.emacswiki.org/emacs/dove-ext.el
  (interactive)
  (if (= 2 (length (window-list)))
      (let ((thisBuf (window-buffer))
            (nextBuf (progn (other-window 1) (buffer-name)))
            (split-type (if (= (window-width) (frame-width))
                            'split-window-horizontally
                          'split-window-vertically)))
        (progn
          (delete-other-windows)
	  (funcall split-type)
          (set-window-buffer nil thisBuf)
          (set-window-buffer (next-window) nextBuf)))
      (error "Frame doesn't have two windows")))

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

(defun rwd-select-thing-at-point (type)
  (require 'thingatpt)
  (let* ((bounds (bounds-of-thing-at-point type)))
    (when bounds
      (goto-char (cdr bounds))
      (set-mark (car bounds)))))

(defun rwd-select-sexp-at-point ()
  (interactive)
  (rwd-select-thing-at-point 'sexp))

(defun rwd-select-matching (s &optional end)
  (interactive "sSelect all matching: ")

  (require 'mark-multiple)

  (let ((re (regexp-quote s)))
    (mm/clear-all)

    (goto-char (point-min))

    (if (re-search-forward re nil t)
        (progn
          (mm/create-master (match-beginning 0) (match-end 0))
          (goto-char (or end (match-end 0)))

          (save-excursion
            (while (re-search-forward re nil t)
              (message "match! %S %S" (match-beginning 0) (match-end 0))

              (mm/add-mirror (match-beginning 0) (match-end 0)))))
      (message "No matches"))))

(defun rwd-select-all-mm-at-point ()
  (interactive)

  (require 'thingatpt)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (if bounds
        (let* ((start (car bounds))
               (end   (cdr bounds))
               (re    (regexp-quote (buffer-substring start end))))

          (rwd-select-matching re end))
      (call-interactively 'rwd-select-matching))))

(defun rwd-selective-display (column)
  "Rotate folding the buffer at no, 2, 4, 6, and 8 columns."
  (interactive "P")
  (set-selective-display
   (if (< (or selective-display 0) 8)
       (or column (+ (or selective-display 0) 2))
     nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Borrowed" defuns:

;;;###autoload
(defun rwd-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " 20480 t)
      (smerge-mode 1))))

;;;###autoload
(defun split-horizontally-not-vertically ()
  "If there's only one window (excluding any possibly active
     minibuffer), then split the current window horizontally."
  (interactive)
  (if (and
       (> (frame-width) 80)
       (>= (frame-width) (* 2 (frame-height)))
       (= (length (window-list nil 'dont-include-minibuffer-even-if-active)) 1))
      (if (not (active-minibuffer-window))
          (split-window-horizontally)
        (message "Don't know how yet"))))

;;;###autoload
(add-hook 'temp-buffer-setup-hook 'split-horizontally-not-vertically)

;;;###autoload
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

;;;###autoload
(defun rwd-clean ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))

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
(defun indent-rigidly-2 ()
  "Indent the region, or otherwise the current line, by 2 spaces."
  (interactive)
  (indent-rigidly-n 2))

;;;###autoload
(defun outdent-rigidly-2 ()
  "Indent the region, or otherwise the current line, by -2 spaces."
  (interactive)
  (indent-rigidly-n -2))

;; frame- or window-resizing function
;; from http://dse.livejournal.com/67732.html. Resizes either frame or window
;; to 80 columns. If the window can be sized to 80 columns wide, without
;; resizing the frame itself, it will resize the window. Otherwise, it will
;; resize the frame. You can use a prefix argument to specify a
;; different column width
(defun fix-frame-horizontal-size (width)
  "Set the frame's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (if window-system
      (set-frame-width (selected-frame) (or width 80))
    (error "Cannot resize frame horizontally: is a text terminal")))

(defun fix-window-horizontal-size (width)
  "Set the window's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (enlarge-window (- (or width 80) (window-width)) 'horizontal))

(defun fix-horizontal-size (width)
  "Set the window's or frame's width to 80 (or prefix arg WIDTH)."
  (interactive "P")
  (condition-case nil
      (fix-window-horizontal-size width)
    (error
     (condition-case nil
     (fix-frame-horizontal-size width)
       (error
    (error "Cannot resize window or frame horizontally"))))))

(global-set-key (kbd "C-x W") 'fix-horizontal-size) ;; FIX move

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;;###autoload
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;;###autoload
(defun rwd-weighin ()
  (interactive)
  (let ((weight  (read-number "Weight (lbs): "))
        (bodyfat (read-number "Bodyfat (%): ")))
    (find-file "~/Work/p4/zss/usr/ryand/weight.txt")
    (goto-char (point-max))
    (insert (format"%s %.1f %.1f\n" (shell-command-to-string "today") weight bodyfat))))

;;;###autoload
(defun github-open-url ()
  (interactive)
  (unless (vc-git-registered (buffer-file-name))
    (error "This is not a git file!"))
  (save-excursion
    (let* ((git-url    (shell-command-to-string "git config remote.origin.url"))
           (git-dir    (vc-git-root (buffer-file-name)))
           (rel-dir    (file-relative-name (buffer-file-name) git-dir))
           (branch     (remove-in-string
                         (shell-command-to-string "git symbolic-ref HEAD")
                        "refs/heads/\\|\n"))
           (url-base   (remove-in-string git-url "\\.git\n*\\|git:"))
           (start-line (line-number-at-pos (region-beginning)))
           (end-line   (line-number-at-pos (region-end)))
           (url        (format "http:%s/blob/%s/%s#L%d-%d"
                               url-base branch rel-dir start-line end-line)))
      (browse-url url))))

;;; http://www.emacswiki.org/emacs/SortWords
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.
    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.
    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun package-version (package)
  (let ((pkg-desc (assq package package-alist)))
    (if pkg-desc
        (package-version-join (package-desc-vers (cdr pkg-desc))))))

(defun rwd-uninstall-package (name)
  (and (package-installed-p name)
       (package-delete (symbol-name name) (package-version name))
       (package-initialize t)))

(defun rwd-html-to-markdown (beg end)
  (interactive "r")
  (shell-command-on-region beg end "/Users/ryan/Desktop/webby/octopress.blog/html2markdown.rb" t t nil t))

(defun rwd-faster-editing ()
  (interactive)
  (flyspell-mode -1)
  (auto-fill-mode -1))

(defun rwd-hide-region (start-re end-re)
  (let ((start (re-search-forward start-re)))
    (when (and start (re-search-forward end-re))
      (let ((ov (make-overlay start (match-beginning 0))))
        (overlay-put ov 'invisible 'rwd-hide-region)))))

(defun kill-matching-lines (regexp &optional rstart rend interactive)
  "Kill lines containing matches for REGEXP.

See `flush-lines' or `keep-lines' for behavior of this command.

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive
   (keep-lines-read-args "Kill lines containing match for regexp"))
  (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (kill-region (or rstart (line-beginning-position))
                     (or rend (point-max))))
      (kill-buffer)))
  (unless (and buffer-read-only kill-read-only-ok)
    ;; Delete lines or make the "Buffer is read-only" error.
    (flush-lines regexp rstart rend interactive)))

(defalias 'kill-lines 'kill-matching-lines)

(defun rwd-comint-scroll-to-bottom-on-output ()
  (interactive)
  (setq comint-scroll-to-bottom-on-output t))

(defun rwd-shell-clear ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)
    (goto-char (point-max))))


(defun rwd-comint-trucate-buffer ()
  (interactive)
  (add-to-list 'comint-output-filter-functions 'comint-truncate-buffer))

(defalias 'rwd-comint-trucate-buffer 'rwd-shell-truncate-buffer)

;;;###autoload
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))
