;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Aliases: (use sort-paragraphs on this section)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 's)
  (require 'dash))

(require 's)

(setq path-re
      (rx bol
          (group (1+ (not (any ":"))))
          (? ":"
             (? (group (1+ (any "0-9"))))
             (? ":"
                (group (1+ (any "0-9")))))))

;;;###autoload
(defun rwd/parse-path-with-pos (path)
  (-let* ((re path-re)
          ((path line col) (cdar (s-match-strings-all re path)))
          (line     (and line (string-to-number line)))
          (col      (or (and col (string-to-number col)) 0)))
    (if line
        (cons path (cons line col))
      (list path))))

;;;###autoload
(defun rwd/find-file-at-point/numbers (orig-fun &rest args)
  "If path at point is followed by :lineno, jump to that line."
  (-let* ((file-str (ffap-string-at-point))
          ((path . pos) (rwd/parse-path-with-pos file-str)))
    (apply orig-fun args)
    (when pos
      (-let (((line . col) pos))
        (and line (goto-line line))
        (and col  (move-to-column (max col 0)))))))

;;;###autoload
(advice-add 'find-file-at-point :around #'rwd/find-file-at-point/numbers)
;; (advice-remove 'find-file-at-point #'rwd/find-file-at-point/numbers)

(defun buffer-string-no-properties ()
  (buffer-substring-no-properties (point-min) (point-max)))

;;;###autoload
(defun rwd/imenu/push (_item)
  (xref-push-marker-stack))

;;;###autoload
(advice-add 'imenu :before #'rwd/imenu/push)

;;;###autoload
(unless (boundp 'ns-is-fullscreen)
  (setq ns-is-fullscreen nil)

  (defadvice ns-toggle-fullscreen (before record-state compile activate)
    (setq ns-is-fullscreen (not ns-is-fullscreen))))

;;;###autoload
(defalias 'elisp-mode 'emacs-lisp-mode "constantly screwing this one up...")

;;;###autoload
(defalias 'info-on-current-buffer 'Info-on-current-buffer
  "fuck you...")

;;;###autoload
(defalias 'big 'rwd-resize-13)

;;;###autoload
(defalias 'huge 'rwd-resize-full)

;;;###autoload
(defalias 'small 'rwd-resize-small)

;;;###autoload
(defalias 'rwd-conflict 'smerge-ediff)

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
(defun align-regexp-comment ()
  (interactive)
  (message "align = from %s to %s" (region-beginning) (region-end))
  (save-excursion
    (save-match-data
      (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)#"))))

;;;###autoload
(defun head (list &optional n)
  "Return a copy of list with the first n elements"
  (butlast list (- (length list) (or n 10))))

;;;###autoload
(defun identity (a) a)

;;;###autoload
(defun list-join (sep lst)
  (mapconcat 'identity lst sep))

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

;;;###autoload
(defmacro remove-from-list (var x)
  `(setq ,var (remove ,x ,var)))

;;;###autoload
(defun reverse-words (beg end)
  "Reverse the order of words in region."
  ;; stolen from https://www.emacswiki.org/emacs/ReverseWords
    (interactive "*r")
    (apply
     'insert
      (reverse
       (split-string
        (delete-and-extract-region beg end) "\\b"))))

(defun rwd-add-to-load-path (dir)
  "Adds a path to the load-path"
  (interactive "DDirectory: ")
  (add-to-list 'load-path (expand-file-name dir) t))

;;;###autoload
(defun rwd-arrange-frame (w h &optional nosplit)
  "Rearrange the current frame to a custom width and height and split unless prefix."
  (let ((frame (selected-frame)))
    (when running-osx
      (delete-other-windows)
      (set-frame-size frame w h)
      (set-frame-position (selected-frame) 0 23)
      (if (not nosplit)
          (split-window-horizontally)))))

(defun rwd-local-extend-env (&rest kvs)
  (make-local-variable 'process-environment)
  (setq process-environment (copy-sequence process-environment))
  (mapc (lambda (kv) (apply 'setenv kv)) kvs))

(defun ohmygems! (name)
  (let* ((home (concat "/Users/ryan/.gem/repos/" name))
         (bin  (concat home "/bin"))
         (path (concat bin ":" (getenv "PATH"))))
    (rwd-local-extend-env `("GEM_HOME" ,home)
                          `("PATH"     ,path))))
(put 'ohmygems! 'safe-local-eval-function t)

;;;###autoload
(defun rwd-emacs-wiki ()
  (interactive)
  (dired (concat "~/Sites/emacs/static/")))

;;;###autoload
(defun rwd-fixup-whitespace ()
  (interactive)
  (delete-horizontal-space)
  (and (looking-at "$")
       (delete-blank-lines)))

;;;###autoload
(defun rwd-forward-line-6 ()
  (interactive)
  (line-move 6))

(eval-when-compile
  (require 'htmlize nil t))

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
(defun rwd-join-lines ()
  "Join lines. Duh."
  (interactive)
  (join-line -1))

;;;###autoload
(defun rwd-lappy ()
  (interactive)
  (delete-other-windows)
  (rwd-resize-full)
  (other-window 1)                      ; to put it back on the left
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
(unless (fboundp 'package-desc-vers)
  (defsubst package-desc-vers (desc)
    "Extract version from a package description vector."
    (aref desc 0)))

(defun rwd-is-fullscreen ()
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

;;;###autoload
(defun rwd-ns-fullscreen ()
  (interactive)
  (unless (rwd-is-fullscreen)
    (toggle-frame-fullscreen)))

;;;###autoload
(defun rwd-ns-fullscreen-off ()
  (interactive)
  (if (rwd-is-fullscreen)
      (toggle-frame-fullscreen)))

;;;###autoload
(defun rwd-ns-fullscreen-toggle ()
  (interactive)
  (toggle-frame-fullscreen))

(defalias 'rwd-ns-toggle-fullscreen 'rwd-ns-fullscreen-toggle)

;;;###autoload
(defun rwd-ns-fullscreen-resize ()
  (interactive)
  (when (rwd-is-fullscreen)
    (toggle-frame-fullscreen)
    (toggle-frame-fullscreen)))

(defalias 'rwd-fix-fullscreen 'rwd-ns-fullscreen-resize)

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
      (rwd-occur "-p -x -n -o")))

;;;###autoload
(defun rwd-occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  ;; this might be a little confusing because this is emacs occur, not mine
  (interactive)
  (occur "[^[:ascii:]]"))

;;;###autoload
(defalias 'rwd-find-utf-8 'rwd-occur-non-ascii)

;;;###autoload
(defun rwd-previous-line-6 ()
  (interactive)
  (line-move-1 -6))

;;;###autoload
(defun rwd-quickref ()
  (interactive)
  (find-file-other-window (expand-file-name "~/Work/p4/zss/www/zenspider.com/ruby/quickref.html.md.erb")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windowing

(defmacro rwd-define-frame (name font-size &optional d-o-f splitfunc)
  "Create a new function defining a frame-resizing set of options"

  (let ((full-name  (intern (concat "rwd-resize-" name)))
        (dimensions (and (listp d-o-f) d-o-f))
        (full       (and (symbolp d-o-f) d-o-f)))
    `(defun ,full-name ()
       (interactive)
       (message "%s: %s %s %s" ',full-name ,font-size ',d-o-f ',splitfunc)
       (when (and ',full (featurep 'cocoa)) (sleep-for 0.01))
       ,@(if full `((rwd-ns-fullscreen)) `((rwd-ns-fullscreen-off)))
       (rwd-set-font-size ,font-size)
       ,@(when dimensions `((rwd-arrange-frame ,@dimensions t)))
       ,@(when splitfunc `((,(intern (concat "rwd-split-" (symbol-name splitfunc))))))
       )))

(rwd-define-frame "13"           12 (163 48))
(rwd-define-frame "13-dense"     10 (225 72))
(rwd-define-frame "13-half"      14 (87 54))
(rwd-define-frame "20"           12 (200 60))
(rwd-define-frame "default"      12 (80 48))
(rwd-define-frame "full"         14 full smart)
(rwd-define-frame "blind"        18 full)
(rwd-define-frame "dense"        9  full thirds-h)
(rwd-define-frame "peepcode"     15 (80 30))
(rwd-define-frame "presentation" 20 (92 34))
(rwd-define-frame "pres2"        24 (100 34))
(rwd-define-frame "small"        12 (80 48) v)

(defalias 'rwd-resize-reset 'rwd-resize-default)

(defun rwd-resize (font-size h w split full-screen)
  (unless full-screen (rwd-ns-fullscreen-off))
  (rwd-set-font-size font-size)
  (unless full-screen
    (rwd-arrange-frame h w (not split)))
  (unless split (delete-other-windows))
  (when full-screen (rwd-ns-fullscreen)))

;;;###autoload
(defun rwd-split-smart ()
  "Splits the current frame either in 2 or 3 depending on size"
  (interactive)

  (delete-other-windows)
  (dotimes (n (1- (/ (frame-width) 80)))
    (split-window-horizontally))
  (balance-windows))

;;;###autoload
(defun rwd-split-thirds-v ()
  "Splits the current frame vertically into even thirds."
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (split-window-vertically)
  (balance-windows))

;;;###autoload
(defun rwd-toggle-window (buffer display-fn)
  (let* ((window (and buffer
                      (get-buffer buffer)
                      (get-buffer-window (get-buffer buffer) nil))))
    (if window
        (delete-window window)
      (call-interactively display-fn))))

;;;###autoload
(defun rwd-split-thirds-h ()
  "Splits the current frame horizontally into even thirds."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

;;;###autoload
(defun rwd-rotate-windows (&optional reverse)
  "When a process or command brings up their buffer on top of the buffer you were working on and you want to move it into the next window so they're both side by side"
  (interactive "P")
  (switch-to-buffer (other-buffer))
  (message "reverse=%S" reverse)
  (other-window (if reverse -1 1))
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
  (rwd-set-mac-font "Fira Code" size))

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
  "Create a unique buffer name starting with the prefix in NAME.
Essentially, I didn't like the format of generate-new-buffer-name."
  (let* ((count 1)
         (buffer-name (format "%s-%d" name count)))
    (while (get-buffer buffer-name)
      (set 'count (+ count 1))
      (set 'buffer-name (format "%s-%d" name count)))
    buffer-name))

;;;###autoload
(defun rwd-utf8 ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t))

(defalias 'rwd-fuck-unicode 'rwd-utf8)
(defalias 'rwd-unicode      'rwd-utf8)
(defalias 'rwd-unix         'rwd-utf8)

;;;###autoload
(defun rwd-shell ()
  "Create a shell buffer that is properly named (shell-<N>)"
  (interactive)
  (let ((buf (rwd-unique-buffer "shell")))
    (if (rwd-currently-only-scratch)
        (progn
          (when (and (eq 1 (length (window-list)))
                     (> (window-width) 82))
            (rwd-split-smart))
          (shell (switch-to-buffer buf)))
      (shell (switch-to-buffer-other-window buf)))))

(require 'dash)

(defun rwd-currently-only-scratch ()
  ;; Modified to check the name so perspective-mode scratch buffers
  ;; count. Also checks any number of windows are all scratch, in case
  ;; you've already split the window.
  ;;
  (-all? (lambda (w) (string-prefix-p "*scratch*"
                                      (buffer-name (window-buffer w))))
         (window-list)))

;;;###autoload
(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-windows.el
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

;;;###autoload
(defun sort-files-by-mtime (a b)
  (let ((ta (file-attribute-modification-time (file-attributes a)))
        (tb (file-attribute-modification-time (file-attributes b))))
    (time-less-p ta tb)))

;;;###autoload
(defun sort-files-by-atime (a b)
  (let ((ta (file-attribute-access-time (file-attributes a)))
        (tb (file-attribute-access-time (file-attributes b))))
    (time-less-p ta tb)))

;;;###autoload
(defun sort-numbers (reverse beg end)
  "Sort numbers in the Region."
  ;; from https://emacs-china.github.io/blog/2017/08/19/sort-sexps/
  (interactive "*P\nr")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ((nextrecfun (lambda () (skip-syntax-forward "-.>")))
          (endrecfun  #'forward-sexp)
          (startkeyfun (lambda ()
                         (or (number-at-point)
                             (user-error "Sexp doesn't looks like a number")))))
      (sort-subr reverse nextrecfun endrecfun startkeyfun))))

;;;###autoload
(defun sort-sexps (reverse beg end)
  "Sort sexps in the Region."
  ;; from https://emacs-china.github.io/blog/2017/08/19/sort-sexps/
  (interactive "*P\nr")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ((nextrecfun (lambda () (skip-syntax-forward "-.>")))
          (endrecfun  #'forward-sexp))
      (sort-subr reverse nextrecfun endrecfun))))

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
    (untabify (point-min) (point-max))))

;;;###autoload
(defun rwd-flycheck-toggle-list-errors ()
  (interactive)
  (rwd-toggle-window flycheck-error-list-buffer
                     'flycheck-list-errors))

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
(defalias 'server-stop 'server-force-delete "Stop the server")

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
  (interactive)
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
(defun rwd-cheap-autotest ()
  "Set up the current shell buffer to be autotest-like."
  (interactive)

  ;; TODO: verify that we're in comint/shell buffer?

  (set (make-local-variable 'comint-output-filter-functions)
       '(comint-truncate-buffer comint-postoutput-scroll-to-bottom))
  (set (make-local-variable 'comint-buffer-maximum-size) 5000)
  (set (make-local-variable 'comint-scroll-show-maximum-output) t)
  (set (make-local-variable 'comint-move-point-for-output) t)

  (set (make-local-variable 'compilation-error-regexp-alist)
       '(
         ("^ +\\(#{RAILS_ROOT}/\\)?\\([^(:]+\\):\\([0-9]+\\)" 2 3)
         ("\\[\\(.*\\):\\([0-9]+\\)\\]:$" 1 2)
         ("^ *\\(?:from \\)?\\([[+]\\)?\\([^:
]+\\):\\([0-9]+\\):in" 2 3)
         ("^.* at \\([^:]*\\):\\([0-9]+\\)$" 1 2)
         )))

;;;###autoload
(defun rwd-clean ()
  "Untabifies, indents and deletes trailing whitespace from buffer."
  (interactive)

  (let ((whitespace-style '(empty
                            trailing
                            indentation::space
                            spece-before-tab::space
                            space-after-tab::space)))
    (whitespace-cleanup)
    (indent-region (point-min) (point-max))))

;;;###autoload
(defun rwd-ansi-colorize ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;;;###autoload
(defun rwd-ansi-colorize-strip ()
  (interactive)
  (ansi-color-filter-region (point-min) (point-max)))

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
        (package-version-join (package-desc-version (cdr pkg-desc))))))

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

(add-to-invisibility-spec '(rwd-hide-region . t))

(defun rwd-hide-region (start-re end-re)
  (let ((start (re-search-forward start-re)))
    (when (and start (re-search-forward end-re))
      (let ((ov (make-overlay start (match-beginning 0))))
        (overlay-put ov 'invisible 'rwd-hide-region)
        (overlay-put ov 'display "...")))))

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

(defvar rwd-window-config nil)

;;;###autoload
(defun rwd-window-save ()
  (interactive)
  (setq rwd-window-config
        (cons (current-window-configuration)
              rwd-window-config))
  (message "Window configuration pushed"))

;;;###autoload
(defun rwd-window-restore ()
  (interactive)
  (when rwd-window-config
    (set-window-configuration (car rwd-window-config))
    (unless (= (length rwd-window-config) 1)
      (setq rwd-window-config (cdr rwd-window-config))
      (message "Popped window configuration"))))

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

(defun rwd-workout-search (name)
  (save-excursion
    (re-search-backward (concat name ": +\\([0-9]+\\)") nil t)
    (cons name (match-string 1))))

(defun rwd-unfuck-bash-history ()
  (interactive)

  (find-file "~/.bash_history")
  (with-current-buffer ".bash_history"
    (delete-matching-lines "[^[:graph:][:space:]\n]\\|"
                           (point-min) (point-max))
    (save-buffer)
    (kill-buffer)))

(defun rwd-workout (days-offset)
  (interactive "P")
  (let ((time (seconds-to-time (+ (* (or days-offset 0) 86400)
                                  (time-to-seconds (current-time)))))
        (date (format-time-string "workout %Y-%m-%d" time)))
    (rwd/workout/2 date)))


(defvar workout-last-date nil)

(defun rwd/workout/2 (date)
  (interactive (list
                (read-from-minibuffer
                 "Date: "
                 (or workout-last-date
                     (format-time-string "%Y-%m-%d" (current-time))))))

  (setq workout-last-date date)

  (save-excursion
    (find-file "~/Work/p4/zss/usr/ryand/superslow.txt")
    (with-current-buffer "superslow.txt"
      (goto-char (point-max))

      (let* ((fmt "%-15s %3s# @ %3ss  -- \n")
             (exercises '("leg press"
                          "leg curl"
                          "chest press"
                          "pulldown"
                          "leg extension"
                          "overhead press"
                          "back extension"
                          "abdominals"
                          ;; "abduction"
                          "compound row"
                          ))
             (weights (reverse (mapcar
                                'rwd-workout-search
                                (reverse exercises)))))

        (goto-char (point-max))
        (apply 'insert
               (format "\nworkout %s\n\n" date)
               (-non-nil
                (mapcar (lambda (exercise)
                          (let* ((w-prompt (concat exercise " weight: "))
                                 (t-prompt (concat exercise   " time: "))
                                 (weight (read-from-minibuffer
                                          w-prompt
                                          (assoc-default exercise weights))))
                            (unless (string-equal weight "")
                              (let ((time (read-from-minibuffer t-prompt)))
                                (format fmt
                                        (concat exercise ":")
                                        weight
                                        time)))))
                        exercises))))
      (goto-char (point-max))))
  (with-current-buffer "superslow.txt"  ; outside save-excursion?!?
    (goto-char (point-max))))

(defun rwd-renumber-debug ()
  (interactive)
  (save-excursion
    (replace-regexp
     "debug [0-9]+"
     '(replace-eval-replacement concat "debug "
                                (replace-quote (1+ replace-count)))
     nil
     (point-min)
     (point-max))))

(unless (fboundp 'defvar-local)
  (defmacro rwd-defvar-local (var val &optional docstring)
    "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
    (declare (debug defvar) (doc-string 3))
    ;; Can't use backquote here, it's too early in the bootstrap.
    (list 'progn (list 'defvar var val docstring)
          (list 'make-variable-buffer-local (list 'quote var)))))

(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    "Set variable VAR to value VAL in current buffer."
    ;; Can't use backquote here, it's too early in the bootstrap.
    (list 'set (list 'make-local-variable (list 'quote var)) val)))

;;;###autoload
(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  ;; from http://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
      (switch-to-buffer buf)))

;;;###autoload
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."

  ;; from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if you
         ;; don't want it.
         (cond ((ignore-errors (org-edit-src-code)) (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t              (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode) (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;;;###autoload
(defun dired-ediff-marked-files ()
  ;; from http://www.emacswiki.org/emacs/DavidBoon#toc4
  "Run ediff on marked ediff files."
  (interactive)

  (let ((marked-files (dired-get-marked-files)))
    (when (= (length marked-files) 2)
      (ediff-files (nth 0 marked-files)
                   (nth 1 marked-files)))

    (when (= (length marked-files) 3)
      (ediff3 (nth 0 marked-files)
              (nth 1 marked-files)
              (nth 2 marked-files)))))

;;;###autoload
(defun ediff-2-windows-regions ()
  (interactive)

  (let* ((buffers (current-2-buffers))
         (curr-buf (car buffers))
         (next-buf (cdr buffers)))
    (let ((curr-narrowed (progn
                           (switch-to-buffer curr-buf)
                           (narrow-to-region-indirect (region-beginning) (region-end))))
          (next-narrowed (progn
                           (switch-to-buffer next-buf)
                           (narrow-to-region-indirect (region-beginning) (region-end)))))
      (ediff-buffers curr-narrowed next-narrowed))))

;;;###autoload
(defun ediff-2-windows ()
  (interactive)

  (let* ((buffers (current-2-buffers))
         (curr-buf (car buffers))
         (next-buf (cdr buffers)))
    (ediff-buffers next-buf curr-buf)))

;;;###autoload
(defun current-2-buffers ()
  (if (one-window-p)
      (error "Frame doesn't have two windows")
    (let* ((cw (selected-window))
           (nw (next-window cw))
           (cb (window-buffer cw))
           (nb (window-buffer nw)))
      (cons cb nb))))

;;;###autoload
(defun rwd-unfuck-modeline ()
  (interactive)
  (set-face-attribute 'mode-line nil
                      :background "grey75"
                      :foreground "black"))

;;;###autoload
(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string-no-properties 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun rwd/unfuck/text (start end)
  (interactive "r")
  (set-text-properties (or start (mark)) (or end (point)) nil))

(defun rwd/unfuck/yank (_arg)
  (rwd/unfuck/text (mark) (point)))

(advice-add 'yank :after #'rwd/unfuck/yank)
;; (advice-remove 'yank #'rwd/unfuck/text)

;; (setq urlreg "\\(?:http://\\)?www\\(?:[./#\+-]\\w*\\)+")
;; (re-seq urlreg (buffer-string))
