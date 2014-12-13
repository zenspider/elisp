;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Keys & Menus:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(progn
  (require 'smartrep)

  (autoload 'inline-string-rectangle "inline-string-rectangle")

  (global-set-key (kbd "<f7>")    'rwd-toggle-split)
  (global-set-key (kbd "<f8>")    'rwd-swap-buffers)
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
  (smartrep-define-key global-map "C-c d" '(("R" . 'ediff-regions-linewise)
                                            ("b" . 'ediff-buffers)
                                            ("d" . 'ediff-directories)
                                            ("f" . 'ediff-files)
                                            ("q" . 'ediff-quit)
                                            ("r" . 'ediff-regions-wordwise)))
  (global-set-key (kbd "C-c e")   'erase-buffer)
  (global-set-key (kbd "C-c g")   'magit-status)
  (global-set-key (kbd "C-c i")   'imenu)
  (global-set-key (kbd "C-c m")   'smerge-start-session)
  (global-set-key (kbd "C-c n")   'narrow-to-region-indirect)
  (global-set-key (kbd "C-c N")   'narrow-or-widen-dwim) ; TODO: switch to C-x n
  (global-set-key (kbd "C-c o")   'rwd-occur-buffer)
  (smartrep-define-key global-map "C-c s" '(("c" . 'sort-columns)
                                            ("l" . 'sort-lines)
                                            ("p" . 'sort-paragraphs)
                                            ("s" . 'sort-symbols)
                                            ("w" . 'sort-words)))
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

  ;; experimenting with hyperspace :D
  (global-set-key (kbd "H-SPC") 'point-to-register)
  (global-set-key (kbd "H-j")   'jump-to-register)
  (global-set-key (kbd "H-b")   'bury-buffer)

  (define-key read-expression-map [(tab)] 'hippie-expand)
  (define-key read-expression-map [(shift tab)] 'unexpand)

  ;; iconify bugs the crap out of me:
  ;; (when window-system (local-unset-key "\C-z"))
  ;; this is currently overridden by elscreen and my comint extensions.

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

;;;###autoload
(unless window-system
  (global-set-key (kbd "C-z C-z") 'suspend-frame)
  (global-set-key (kbd "C-Z") 'suspend-frame)) ; for shell-mode in terms
