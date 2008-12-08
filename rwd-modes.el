;; ;; ============================================================
;; ;; Requires:

;; (require 'project-local-variables)

;; (autoload 'which-function "which-func" "doco" t)
;; (autoload 'slime-setup "slime.el" "slime" t)
;; (autoload 'slime "slime.el" "slime" t)
;; (autoload 'etags-select-find-tag "etags-select" "doco" t)
;; (autoload 'etags-select-find-tag-at-point "etags-select" "doco" t)

;; (if running-xemacs
;;     (add-to-list 'Info-directory-list "/usr/share/info"))

;; (if running-emacs
;;     (progn
;;       (eval-when-compile
;;         (when (< emacs-major-version 20) (require 'cl)))
;;       (require 'vc-hooks)
;;       (setq apropos-do-all t)
;;       (eval-after-load 'comint
;;         '(add-hook 'comint-output-filter-functions
;;                    'comint-watch-for-password-prompt))))
;; (add-hook 'comint-mode-hook 'comint-add-scroll-to-bottom)

(hook-after-load info
  (add-to-list 'Info-directory-list "~/Sites/emacs/elisp/info"))

(setq my-usual-programming-modes
      '(ruby-mode lisp-mode scheme-mode emacs-lisp-mode))
;; (setq my-usual-programming-mode-hooks
;;       (mapcar (lambda (h) (intern (concat (symbol-name h) "-hook")))
;;               my-usual-programming-modes))

(dolist (spec '(("\\.bash.*$"                . ksh-mode)
                ("\\.js$"                    . ecmascript-mode)
                ("^\\(GNUm\\|M\\)akefile.*$" . makefile-mode)))
  (add-to-list 'auto-mode-alist spec))

;; ;; TODO:
;; ;; (defun setup-abbrevs-for-modes (modes)
;; ;;   (dolist (mode modes)
;; ;;     (setq ,(concat-symbols mode '-mode-abbrev-table) '())))

;; ;; (add-hook `(concat-symbols ',mode '-mode-hook)
;; ;;           (lambda ()
;; ;;             (require 'expand)
;; ;;             (expand-add-abbrevs
;; ;;              `(concat-symbols ',mode '-mode-abbrev-table)
;; ;;              `(concat-symbols ',mode '-expand-list))
;; ;;             (abbrev-mode)))))

;; ;; (setup-abbrevs-for-modes '(ruby scheme))

;; ;; (mapcar
;; ;;  (lambda (mode) (setq (concat-symbols mode '-mode-abbrev-table) '()))
;; ;;  '(ruby scheme))

;; (setq save-abbrevs nil)

;; ;; TODO:
;; ;;  (add-hook 'foo-mode-hook
;; ;;            (lambda ()
;; ;;               (set (make-local-variable imenu-generic-expression)
;; ;;                    '(("Comments" "^\\s-*#" 1)
;; ;;                      ...))))

;; ;; ============================================================
;; ;; Scheme / Lisp:

;; (setq scheme-program-name "mzscheme")
;; (setq inferior-lisp-program "/opt/local/bin/sbcl")

;; ;; (defconst scheme-expand-list
;; ;;   (mapcar (lambda (l) (expand-parse (car l) (car (cdr l))))
;; ;;           '(
;; ;;             ("def" ("(define " n
;; ;;                     "  (lambda (a)\n"
;; ;;                     "    (cond\n"
;; ;;                     "     ((null? a) 'fix)\n"
;; ;;                     "\n"
;; ;;                     "     (else 'fix))))\n")))))

;; ;; (setq scheme-mode-abbrev-table '())

;; ;; (add-hook 'scheme-mode-hook
;; ;;           '(lambda ()
;; ;;              (require 'expand)
;; ;;              (expand-add-abbrevs scheme-mode-abbrev-table scheme-expand-list)
;; ;;              (abbrev-mode)))

;; ;; ============================================================
;; ;; Misc Modes/Stuff:

;; (def-hook text-mode
;;   (turn-on-auto-fill)
;;   (define-key text-mode-map (kbd "M-s") 'fixup-whitespace))

;; (eval-after-load 'compile
;;   '(def-hook c-mode
;;      (local-set-key (kbd "C-c C-r") 'recompile)))

;; (setq ansi-color-for-comint-mode 'filter)

(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer." t)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(require 'window-number)
(window-number-meta-mode 1)

;; ;;; pastebin

;; (autoload 'pastebin-buffer "pastebin" "doco" t)
;; (autoload 'pastebin        "pastebin" "doco" t)
;; (hook-after-load ruby
;;   (add-to-list 'pastebin-type-assoc '(ruby-mode . "ruby")))
