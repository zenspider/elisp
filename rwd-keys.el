;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Keys & Menus:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(progn
  (require 'map)

  (defun rwd-smart-keys (prefix map)
    "A simple function to bind a map of key/fn pairs to a prefix key."
    (let ((key (kbd (format "C-c %s" prefix)))
          (keymap (make-sparse-keymap)))

      (map-apply (lambda (k v) (define-key keymap (kbd k) (cadr v)))
                 map)

      (define-key global-map key keymap)
      keymap))

  (autoload 'inline-string-rectangle "inline-string-rectangle")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; global overrides:
  ;;
  ;; This is for keybindings that are not composed as "C-c
  ;; <letter>..." as such, they should be overriding something
  ;; standard in such a way as makes sense. "C-x C-b" using 'bs-show
  ;; to replace anemic 'list-buffers is a very good example of this.
  ;; Some of these keys aren't actually bound globally, but they might
  ;; be specific to modes (even though they should almost all be bound
  ;; into "C-c C-<letter>...", per standard.

  (global-set-key (kbd "<f7>")    'rwd-toggle-split)        ; undefined
  (global-set-key (kbd "<f8>")    'rwd-swap-buffers)        ; undefined
  (global-set-key (kbd "C-M-.")   'etags-select-find-tag)   ; was xref-find-apropos
  (global-set-key (kbd "C-M-;")   'unfill-paragraph)        ; undefined
  (global-set-key (kbd "C-M-SPC") 'er/expand-region)        ; was mark-sexp
  (global-set-key (kbd "C-M-x")   'bury-buffer)             ; unassigned?
  (global-set-key (kbd "C-M-y")   'kill-ring-search)        ; undefined
  (global-set-key (kbd "C-x C-b") 'bs-show)                 ; was list-buffers
  (global-set-key (kbd "C-x C-p") 'find-file-at-point)      ; was mark-page
  (global-set-key (kbd "C-x C-t") 'toggle-buffer)           ; was transpose-lines
  (global-set-key (kbd "C-x f")   'find-file)               ; was set-fill-column, typos
  (global-set-key (kbd "C-x r t") 'inline-string-rectangle) ; was string-rectangle
  (global-set-key (kbd "M-?")     'etags-select-find-tag-at-point) ; was xref-find-references
  (global-set-key (kbd "M-[")     'outdent-rigidly-2)       ; undefined
  (global-set-key (kbd "M-]")     'indent-rigidly-2)        ; undefined
  (global-set-key (kbd "M-j")     'rwd-join-lines)          ; was indent-new-comment-line

  ;; sanity/compatibility (mostly stuff from xemacs):
  (global-set-key (kbd "M-g")      'goto-line)
  (global-set-key (kbd "<C-up>")   'rwd-previous-line-6)
  (global-set-key (kbd "<C-down>") 'rwd-forward-line-6)
  (global-set-key (kbd "<M-up>")   'rwd-scroll-up)
  (global-set-key (kbd "<M-down>") 'rwd-scroll-down)

  ;; iconify bugs the crap out of me:
  ;; (when window-system (local-unset-key "\C-z"))
  ;; this is currently overridden by elscreen and my comint extensions.

  (global-unset-key (kbd "M-o"))        ; I will *never* use facemenu styles

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; violations that need to be changed over time

  (global-set-key (kbd "C-c C-s") 'rwd-select-all-mm-at-point)
  (global-set-key (kbd "C-c M-q") 'unfill-paragraph)
  (global-set-key (kbd "M-s")     'fixup-whitespace) ; useful search/highlight stuff
  (global-set-key (kbd "M-S")     'rwd-fixup-whitespace)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; mode map bindings
  ;;
  ;; Think about moving these into hooks if they're not for global
  ;; modes.

  (define-key read-expression-map [(tab)] 'hippie-expand)
  (define-key read-expression-map [(shift tab)] 'unexpand)
  (define-key emacs-lisp-mode-map (kbd "C-M-<return>") 'eval-defun)

  ;; this is so awesome - occur easily inside isearch
  (define-key isearch-mode-map (kbd "C-o")
    (lambda ()
      (interactive)
      (let ((case-fold-search isearch-case-fold-search))
        (occur (if isearch-regexp
                   isearch-string (regexp-quote isearch-string))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; normal user-level keybindings

  (global-set-key (kbd "C-c -")   'rwd-selective-display)
  (global-set-key (kbd "C-c O")   'rwd-occur-n-buffer)
  (global-set-key (kbd "C-c SPC") 'delete-trailing-whitespace)
  (global-set-key (kbd "C-c a")   'align-cols)
  (global-set-key (kbd "C-c b")   'rwd-rotate-windows)
  (global-set-key (kbd "C-c c")   'rwd-clean)
  (rwd-smart-keys 'd '((   "R" .  'ediff-regions-linewise)
                       (   "b" .  'ediff-buffers)
                       (   "d" .  'ediff-directories)
                       (   "f" .  'ediff-files)
                       (   "q" .  'ediff-quit)
                       ( "SPC" .  'delete-trailing-whitespace)
                       (   "r" .  'ediff-regions-wordwise)
                       (   "w" .  'delete-trailing-whitespace)))
  (global-set-key (kbd "C-c e")   'erase-buffer)
  (global-set-key (kbd "C-c g")   'magit-status)
  (global-set-key (kbd "C-c i")   'imenu)
  (global-set-key (kbd "C-c n")   'narrow-to-region-indirect)
  (global-set-key (kbd "C-x n")   'narrow-or-widen-dwim) ; TODO: switch to C-x n
  (global-set-key (kbd "C-c o")   'rwd-occur-buffer)
  (rwd-smart-keys 's '((   "c" .  'sort-columns)
                       (   "l" .  'sort-lines)
                       (   "n" .  'sort-numbers)
                       (   "p" .  'sort-paragraphs)
                       (   "s" .  'sort-sexps)
                       (   "S" .  'sort-symbols)
                       (   "w" .  'sort-words)))
  (global-set-key (kbd "C-x /")   'align-regexp)
  (global-set-key (kbd "C-x =")   'align-regexp-=)
  (global-set-key (kbd "C-x #")   'align-regexp-comment)

  ;; experimenting with hyperspace :D
  (global-set-key (kbd "H-SPC") 'point-to-register)
  (global-set-key (kbd "H-j")   'jump-to-register)
  (global-set-key (kbd "H-b")   'bury-buffer)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; unsorted

  ;; This allows me to enforce that bury-buffer is bound to C-M-x
  ;; regardless of mode (YAY!)
  (require 'override-keymaps)
  (override-keymaps))

;; (define-key erc-mode-map (kbd "C-c C-a") 'autotest-switch)

;; FIX: blech!
;; (mapcar (lambda (mode)
;;           (define-key mode (kbd "C-c C-a") 'autotest-switch))
;;         '(diff-mode-map
;;           diff-minor-mode-map
;;           grep-mode-map
;;           help-mode-map))

;;;###autoload
(global-set-key [remap isearch-forward]
                (lambda (p)
                  ;; http://endlessparentheses.com/quickly-search-for-occurrences-of-the-symbol-at-point.html
                  (interactive "P")
                  (let ((current-prefix-arg nil))
                    (call-interactively
                     (if p #'isearch-forward-symbol-at-point
                       #'isearch-forward)))))

;;;###autoload
(define-key isearch-mode-map (kbd "M-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (multi-occur-in-matching-buffers "."
       (if isearch-regexp
           isearch-string (regexp-quote isearch-string))))))

(define-key lisp-interaction-mode-map (kbd "C-j") 'newline-and-indent)

;; grep all same extension files from inside isearch
;;;###autoload
(define-key isearch-mode-map (kbd "C-M-o")
  (lambda ()
    (interactive)
    (grep-compute-defaults)
    (lgrep (if isearch-regexp isearch-string (regexp-quote isearch-string))
           (if (file-name-extension (buffer-file-name))
            (format "*.%s" (file-name-extension (buffer-file-name)))
            "*")
           default-directory)
    (isearch-abort)))

;;;###autoload
(eval-after-load 'p4
  '(define-key p4-prefix-map (kbd "A") 'p4-diff-all-opened))

;;;###autoload
(progn
  (when (require 'multiple-cursors nil t)
    (rwd-smart-keys 'm '(("<" . 'mc/mark-previous-like-this)
                         ("=" . 'mc/mark-all-like-this)
                         ("A" . 'mc/align)
                         ("a" . 'mc/mark-all-in-region)
                         ("e" . 'mc/edit-ends-of-lines)
                         ("i" . 'mc/insert-numbers)
                         ("h" . 'mc-hide-unmatched-lines-mode)
                         ("l" . 'mc/edit-lines)
                         ("m" . 'mc/mark-all-dwim)
                         ("n" . 'mc/mark-next-like-this)
                         ("r" . 'set-rectangular-region-anchor)
                         ("s" . 'mc/mark-all-symbols-like-this)))))
