;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Keys & Menus:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(progn
  (autoload 'inline-string-rectangle "inline-string-rectangle")

  (global-set-key (kbd "<f7>")    'rwd-toggle-split)
  (global-set-key (kbd "<f8>")    'rwd-swap-buffers) ;; FIX: this broke on 23
  (global-set-key (kbd "C-M-.")   'etags-select-find-tag)
  (global-set-key (kbd "C-M-x")   'bury-buffer)
  (global-set-key (kbd "C-c C-d") 'delete-trailing-whitespace)
  (global-set-key (kbd "C-c C-s") 'rwd-select-all-mm-at-point)
  (global-set-key (kbd "C-c O")   'rwd-occur-n-buffer)
  (global-set-key (kbd "C-c b")   'rwd-rotate-windows)
  (global-set-key (kbd "C-c c")   'rwd-clean)
  (global-set-key (kbd "C-c d b") 'ediff-buffers)
  (global-set-key (kbd "C-c d d") 'ediff-directories)
  (global-set-key (kbd "C-c d f") 'ediff-files)
  (global-set-key (kbd "C-c d q") 'ediff-quit)
  (global-set-key (kbd "C-c e")   'erase-buffer)
  (global-set-key (kbd "C-c g")   'magit-status)
  (global-set-key (kbd "C-c h")   'rwd-html-to-markdown)
  (global-set-key (kbd "C-M-;")    'unfill-paragraph)
  (global-set-key (kbd "C-c i")   'imenu)
  (global-set-key (kbd "C-c m")   'smerge-start-session)
  (global-set-key (kbd "C-c o")   'rwd-occur-buffer)
  (global-set-key (kbd "C-c s c") 'sort-columns)
  (global-set-key (kbd "C-c s l") 'sort-lines)
  (global-set-key (kbd "C-c s p") 'sort-paragraphs)
  (global-set-key (kbd "C-c s s") 'sort-symbols)
  (global-set-key (kbd "C-c s w") 'sort-words)
  (global-set-key (kbd "C-x /")   'align-regexp)
  (global-set-key (kbd "C-x =")   'align-regexp-=)
  (global-set-key (kbd "C-x C-b") 'bs-show)
  (global-set-key (kbd "C-x C-p") 'find-file-at-point)
  (global-set-key (kbd "C-x C-t") 'toggle-buffer)
  (global-set-key (kbd "C-x f")   'find-file) ; very common typo
  (global-set-key (kbd "C-x r t") 'inline-string-rectangle)
  (global-set-key (kbd "M-?")     'etags-select-find-tag-at-point)
  (global-set-key (kbd "M-C-y")   'kill-ring-search)
  (global-set-key (kbd "C-M-SPC") 'er/expand-region)
  (global-set-key (kbd "M-[")     'outdent-rigidly-2)
  (global-set-key (kbd "M-]")     'indent-rigidly-2)
  (global-set-key (kbd "M-s")     'fixup-whitespace)

;; Experiment with bookmark mode
  (global-set-key (kbd "M-SPC")   'bc-set)
  (global-set-key (kbd "M-j")     'bc-previous)
  (global-set-key (kbd "M-J")     'bc-next)
  (global-set-key (kbd "C-M-j")   'bc-local-previous)
  (global-set-key (kbd "C-M-J")   'bc-local-next)
  (global-set-key (kbd "C-c J b") 'bc-goto-current)
  (global-set-key (kbd "C-c J l") 'bc-list)

  (define-key read-expression-map [(tab)] 'hippie-expand)
  (define-key read-expression-map [(shift tab)] 'unexpand)

;; TODO:
;; (global-set-key (kbd "C-M-s")
;;   (lambda () (interactive) (fixup-whitespace) (delete-blank-lines)))

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

  (when running-osx
    (if (not (version< emacs-version "23"))
        (global-set-key (kbd "M-`") 'other-frame))

    ;(define-key local-map [ns-drag-file] 'ns-find-file)
    )

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
(local-unset-key (kbd "M-o"))

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

(require 'multiple-cursors)
(global-set-key (kbd "C-c C-a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c C-m") 'mc/edit-lines)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-<")     'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
