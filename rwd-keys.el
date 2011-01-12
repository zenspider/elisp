;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Keys & Menus:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(progn
  (message "RAWR! loading rwd-keys.el")
  (global-set-key (kbd "<f7>")    'rwd-toggle-split)
  (global-set-key (kbd "<f8>")    'rwd-swap-buffers) ;; FIX: this broke on 23
  (global-set-key (kbd "C-M-.")   'etags-select-find-tag)
  (global-set-key (kbd "C-M-x")   'bury-buffer)
  (global-set-key (kbd "C-c C-d") 'delete-trailing-whitespace)
  (global-set-key (kbd "C-c C-s") 'sort-lines)
  (global-set-key (kbd "C-c c")   'rwd-clean)
  (global-set-key (kbd "C-c d b") 'ediff-buffers)
  (global-set-key (kbd "C-c d d") 'ediff-directories)
  (global-set-key (kbd "C-c d f") 'ediff-files)
  (global-set-key (kbd "C-c d q") 'ediff-quit)
  (global-set-key (kbd "C-c e")   'erase-buffer)
  (global-set-key (kbd "C-c i")   'imenu)
  (global-set-key (kbd "C-c b")   'rwd-rotate-windows)
  (global-set-key (kbd "C-c o")   'rwd-occur-buffer)
  (global-set-key (kbd "C-x /")   'align-regexp)
  (global-set-key (kbd "C-x C-b") 'bs-show)
  (global-set-key (kbd "C-x C-p") 'find-file-at-point)
  (global-set-key (kbd "C-x C-t") 'toggle-buffer)
  (global-set-key (kbd "M-?")     'etags-select-find-tag-at-point)
  (global-set-key (kbd "M-C-y")   'kill-ring-search)
  (global-set-key (kbd "M-[")     'outdent-rigidly-2)
  (global-set-key (kbd "M-]")     'indent-rigidly-2)
  (global-set-key (kbd "M-s")     'fixup-whitespace)
  (global-set-key (kbd "C-x f")   'find-file) ; very common typo
  (global-set-key (kbd "C-M-0") '(lambda () (interactive) (window-configuration-to-register ?0)))
  (global-set-key (kbd "C-0") '(lambda () (interactive) (jump-to-register ?0)))

;; TODO:
;; (global-set-key (kbd "C-M-s")
;;   (lambda () (interactive) (fixup-whitespace) (delete-blank-lines)))

  ;; (require 'em-glob)
  ;; (setq tags-table-list
  ;;       (mapcar 'expand-file-name
  ;;               (eshell-extended-glob "~/Work/p4/zss/src/*/dev/TAGS")))

  (define-key emacs-lisp-mode-map       (kbd "C-c e") 'my-eval-and-replace)
  (define-key lisp-interaction-mode-map (kbd "C-c e") 'my-eval-and-replace)

  ;; iconify bugs the crap out of me:
  (when window-system (global-unset-key "\C-z"))

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

    (define-key global-map [ns-drag-file] 'ns-find-file))

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

(define-key lisp-interaction-mode-map (kbd "C-j") 'newline-and-indent)

;; grep all same extension files from inside isearch
;;;###autoload
(define-key isearch-mode-map (kbd "C-M-o")
  (lambda ()
    (interactive)
    (grep-compute-defaults)
    (lgrep (if isearch-regexp isearch-string (regexp-quote isearch-string))
           (format "*.%s" (file-name-extension (buffer-file-name)))
           default-directory)
    (isearch-abort)))

;;;###autoload
(eval-after-load 'p4
  '(define-key p4-prefix-map (kbd "A") 'p4-diff-all-opened))
