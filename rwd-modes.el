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

;; HACK: fix! used to work

;; (eval-after-load 'info
;;   '(add-to-list 'Info-directory-list "~/Sites/emacs/elisp/info"))

;;;###autoload
(setq my-usual-programming-modes
      '(ruby-mode lisp-mode scheme-mode emacs-lisp-mode))

(dolist (spec '(("\\.bash.*$"                . ksh-mode)
                ("\\.js$"                    . ecmascript-mode)
                ("^\\(GNUm\\|M\\)akefile.*$" . makefile-mode)))
  (add-to-list 'auto-mode-alist spec))

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

;; (eval-after-load 'compile
;;   '(def-hook c-mode
;;      (local-set-key (kbd "C-c C-r") 'recompile)))

;; (setq ansi-color-for-comint-mode 'filter)

(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer." t)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; ;;; pastebin

;; (autoload 'pastebin-buffer "pastebin" "doco" t)
;; (autoload 'pastebin        "pastebin" "doco" t)
;; (hook-after-load ruby
;;   (add-to-list 'pastebin-type-assoc '(ruby-mode . "ruby")))
