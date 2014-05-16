;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Keys & Menus:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(progn
  (autoload 'inline-string-rectangle "inline-string-rectangle")

  (global-set-key (kbd "<f7>")    'rwd-toggle-split)
  (global-set-key (kbd "<f8>")    'rwd-swap-buffers) ;; FIX: this broke on 23
  (global-set-key (kbd "C-M-.")   'etags-select-find-tag)
  (global-set-key (kbd "C-M-;")   'unfill-paragraph)
  (global-set-key (kbd "C-M-x")   'bury-buffer)
  (global-set-key (kbd "C-c -")   'rwd-selective-display)
  (global-set-key (kbd "C-c C-d") 'delete-trailing-whitespace)
  (global-set-key (kbd "C-c C-s") 'rwd-select-all-mm-at-point)
  (global-set-key (kbd "C-c M-q") 'unfill-paragraph)
  (global-set-key (kbd "C-c O")   'rwd-occur-n-buffer)
  (global-set-key (kbd "C-c b")   'rwd-rotate-windows)
  (global-set-key (kbd "C-c a")   'align-cols)
  (global-set-key (kbd "C-c c")   'rwd-clean)
  (global-set-key (kbd "C-c d R") 'ediff-regions-linewise)
  (global-set-key (kbd "C-c d b") 'ediff-buffers)
  (global-set-key (kbd "C-c d d") 'ediff-directories)
  (global-set-key (kbd "C-c d f") 'ediff-files)
  (global-set-key (kbd "C-c d q") 'ediff-quit)
  (global-set-key (kbd "C-c d r") 'ediff-regions-wordwise)
  (global-set-key (kbd "C-c e")   'erase-buffer)
  (global-set-key (kbd "C-c g")   'magit-status)
  (global-set-key (kbd "C-c h")   'rwd-html-to-markdown)
  (global-set-key (kbd "C-c i")   'imenu)
  (global-set-key (kbd "C-c m")   'smerge-start-session)
  (global-set-key (kbd "C-c n")   'narrow-to-region-indirect)
  (global-set-key (kbd "C-c o")   'rwd-occur-buffer)
  (global-set-key (kbd "C-c s c") 'sort-columns)
  (global-set-key (kbd "C-c s l") 'sort-lines)
  (global-set-key (kbd "C-c s p") 'sort-paragraphs)
  (global-set-key (kbd "C-c s s") 'sort-symbols)
  (global-set-key (kbd "C-c s w") 'sort-words)
  (global-set-key (kbd "C-x /")   'align-regexp)
  (global-set-key (kbd "C-x =")   'align-regexp-=)
  (global-set-key (kbd "C-x #")   'align-regexp-comment)
  (global-set-key (kbd "C-x C-b") 'bs-show)
  (global-set-key (kbd "C-x C-p") 'find-file-at-point)
  (global-set-key (kbd "C-x C-t") 'toggle-buffer)
  (global-set-key (kbd "C-x D")   'dired-open-alias) ; very common typo
  (global-set-key (kbd "C-x f")   'find-file) ; very common typo
  (global-set-key (kbd "C-x r t") 'inline-string-rectangle)
  (global-set-key (kbd "M-?")     'etags-select-find-tag-at-point)
  (global-set-key (kbd "M-C-y")   'kill-ring-search)
  (global-set-key (kbd "C-M-SPC") 'er/expand-region)
  (global-set-key (kbd "M-[")     'outdent-rigidly-2)
  (global-set-key (kbd "M-]")     'indent-rigidly-2)
  (global-set-key (kbd "M-j")     'rwd-join-lines)
  (global-set-key (kbd "M-s")     'fixup-whitespace)
  (global-set-key (kbd "M-S")     'rwd-fixup-whitespace)

  (define-key read-expression-map [(tab)] 'hippie-expand)
  (define-key read-expression-map [(shift tab)] 'unexpand)

  ;; (require 'em-glob)
  ;; (setq tags-table-list
  ;;       (mapcar 'expand-file-name
  ;;               (eshell-extended-glob "~/Work/p4/zss/src/*/dev/TAGS")))

  ;; iconify bugs the crap out of me:
  (when window-system (local-unset-key "\C-z"))

  ;; compatibility:
  (global-set-key (kbd "M-g")      'goto-line)
  (global-set-key (kbd "<C-up>")   'rwd-previous-line-6)
  (global-set-key (kbd "<C-down>") 'rwd-forward-line-6)
  (global-set-key (kbd "<M-up>")   'rwd-scroll-up)
  (global-set-key (kbd "<M-down>") 'rwd-scroll-down)
  (global-set-key (kbd "C-M-l")    'rwd-scroll-top)

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

;; this is so awesome - occur easily inside isearch
;;;###autoload
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string (regexp-quote isearch-string))))))

;;;###autoload
(global-unset-key (kbd "M-o"))

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
    (global-set-key (kbd "C-c C-c e") 'mc/edit-ends-of-lines)
    (global-set-key (kbd "C-c C-c l") 'mc/edit-lines)
    (global-set-key (kbd "C-c C-c =") 'mc/mark-all-like-this)
    (global-set-key (kbd "C-c C-c a") 'mc/mark-all-in-region)
    (global-set-key (kbd "C-c C-c <") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-c n") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-c C-c r") 'set-rectangular-region-anchor)))
