;; ;; ============================================================
;; ;; Requires:

;; (require 'project-local-variables)

;; (when (require 'diminish nil 'noerror)
;;   (eval-after-load "company"
;;       '(diminish 'company-mode "Cmp"))
;;   (eval-after-load "abbrev"
;;     '(diminish 'abbrev-mode "Ab"))
;;   (eval-after-load "yasnippet"
;;     '(diminish 'yas/minor-mode "Y")))
;; 
;; (add-hook 'emacs-lisp-mode-hook 
;;   (lambda()
;;     (setq mode-name "el"))) 


;;;###autoload
(progn
  (autoload 'etags-select-find-tag "etags-select")
  (autoload 'etags-select-find-tag-at-point "etags-select")
  (autoload 'magit-status "magit" nil t)
  (autoload 'ssh "ssh" nil t))

(add-hook 'ssh-mode-hook 'ssh-directory-tracking-mode)

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
                ("\\.haml$"                  . haml-mode)
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

;;;###autoload
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " 20480 t)
      (smerge-mode 1))))

;;;###autoload
(add-hook 'find-file-hook 'sm-try-smerge t)
