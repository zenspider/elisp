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

  (defun gsk (key fn)
    (global-set-key (kbd key) fn))

  (defalias 'rsk 'rwd-smart-keys)

  (gsk "<f7>"    'rwd-toggle-split)               ; undefined
  (gsk "<f8>"    'rwd-swap-buffers)               ; undefined
  (gsk "C-M--"   'default-text-scale-decrease)    ; was negative-argument
  (gsk "C-M-."   'etags-select-find-tag)          ; was xref-find-apropos
  (gsk "C-M-;"   'unfill-paragraph)               ; unwrap a parapgraph!
  (gsk "C-M-="   'default-text-scale-increase)    ; bigger font everywhere
  (gsk "C-M-SPC" 'er/expand-region)               ; was mark-sexp
  (gsk "C-c B"   'bury-buffer)                    ; unassigned?
  (gsk "C-M-y"   'kill-ring-search)               ; undefined
  (gsk "C-h C"   'helpful-command)                ; better help for commands
  (gsk "C-h C-p" 'helpful-at-point)               ; better help at point
  (gsk "C-h F"   'helpful-function)               ; better help for functions
  (gsk "C-h K"   'helpful-kill-buffers)           ; better help kill windows
  (gsk "C-h f"   'helpful-callable)               ; better help for a thing
  (gsk "C-h k"   'helpful-key)                    ; better help for a key
  (gsk "C-h v"   'helpful-variable)               ; better help for a var
  (gsk "C-x C-b" 'bs-show)                        ; was list-buffers
  (gsk "C-x C-p" 'find-file-at-point)             ; was mark-page
  (gsk "C-x C-t" 'toggle-buffer)                  ; was transpose-lines
  (gsk "C-x f"   'find-file)                      ; was set-fill-column, typos
  (gsk "C-x r t" 'inline-string-rectangle)        ; was string-rectangle
  (gsk "M-?"     'etags-select-find-tag-at-point) ; was xref-find-references
  (gsk "M-["     'outdent-rigidly-2)              ; undefined
  (gsk "M-]"     'indent-rigidly-2)               ; undefined
  (gsk "M-j"     'rwd-join-lines)                 ; was indent-new-comment-line

  ;; experiment: cycle through buffers of the same mode
  (gsk "C-`"     'bs-cycle-next)            ; go to next buffer of same mode
  (gsk "C-~"     'bs-cycle-previous)        ; go to prev buffer of same mode
  ;; TODO: maybe my matias won't let me bind these? or mac os?
  (gsk "M-`"     'bs-cycle-next)            ; go to next buffer of same mode
  (gsk "M-~"     'bs-cycle-previous)        ; go to prev buffer of same mode
  (gsk "H-`"     'goto-last-change)         ; go to last change in buffer
  (gsk "H-~"     'goto-last-change-reverse) ; go to prev change

  ;; sanity/compatibility (mostly stuff from xemacs):
  (gsk "M-g"      'goto-line)
  (gsk "<C-up>"   'rwd-previous-line-6)
  (gsk "<C-down>" 'rwd-forward-line-6)
  (gsk "<M-up>"   'rwd-scroll-up)
  (gsk "<M-down>" 'rwd-scroll-down)

  ;; iconify bugs the crap out of me:
  (when window-system (global-unset-key "\C-z"))
  ;; this is currently overridden by elscreen and my comint extensions.

  (global-unset-key (kbd "M-o"))        ; I will *never* use facemenu styles

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; violations that need to be changed over time

  (gsk "C-c C-s" 'rwd-select-all-mm-at-point)
  (gsk "C-c M-q" 'unfill-paragraph)
  (gsk "M-s"     'fixup-whitespace) ; useful search/highlight stuff
  (gsk "M-S"     'rwd-fixup-whitespace)

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

  (gsk "C-c -"   'rwd-selective-display)
  (gsk "C-c O"   'rwd-occur-n-buffer)
  (gsk "C-c SPC" 'delete-trailing-whitespace)
  (gsk "C-c a"   'align-cols)
  (gsk "C-c b"   'rwd-rotate-windows)
  (gsk "C-c c"   'rwd-clean)
  (rsk 'd '((   "R" .  'ediff-regions-linewise)
            (   "b" .  'ediff-buffers)
            (   "d" .  'ediff-directories)
            (   "f" .  'ediff-files)
            (   "q" .  'ediff-quit)
            ( "SPC" .  'delete-trailing-whitespace)
            (   "r" .  'ediff-regions-wordwise)
            (   "w" .  'delete-trailing-whitespace)))
  (gsk "C-c e"   'erase-buffer)
  (gsk "C-c g"   'magit-status)
  (gsk "C-c G"   'magit-dispatch)
  (gsk "C-c f"   'magit-file-dispatch)
  (gsk "C-c i"   'imenu)
  (gsk "C-c n"   'narrow-to-region-indirect)
  (gsk "C-x n"   'narrow-or-widen-dwim)
  (gsk "C-c o"   'rwd-occur-buffer)
  (gsk "C-c p"   'ffip)
  (gsk "C-c S"   'string-edit-at-point)
  (rsk 's '((   "c" .  'sort-columns)
            (   "l" .  'sort-lines)
            (   "n" .  'sort-numbers)
            (   "p" .  'sort-paragraphs)
            (   "s" .  'sort-sexps)
            (   "S" .  'sort-symbols)
            (   "w" .  'sort-words)))
  (gsk "C-c /"   'align-regexp)
  (gsk "C-c ="   'align-regexp-=)
  (gsk "C-c #"   'align-regexp-comment)

  (gsk "C-c <up>"   'rwd-window-restore) ; push onto window stack
  (gsk "C-c <down>" 'rwd-window-save)    ; pop off window stack

  ;; experimenting with hyperspace :D
  (gsk "H-SPC" 'point-to-register)
  (gsk "H-j"   'jump-to-register)
  (gsk "H-b"   'bury-buffer)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; unsorted

  ;; ;; This allows me to enforce that bury-buffer is bound to C-M-x
  ;; ;; regardless of mode (YAY!)
  ;; (require 'override-keymaps)
  ;; (override-keymaps)
  )

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
(rsk 'm '(("<" . 'mc/mark-previous-like-this)
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
          ("s" . 'mc/mark-all-symbols-like-this)))
