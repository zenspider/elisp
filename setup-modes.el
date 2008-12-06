;; ;; ============================================================
;; ;; Requires:

(require 'p4)
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

;; (require 'p4-lowlevel)
;; (require 'vc-p4)

(hook-after-load info
  (add-to-list 'Info-directory-list "~/Sites/emacs/elisp/info"))

(setq my-usual-programming-modes
      '(ruby-mode lisp-mode scheme-mode emacs-lisp-mode))
(setq my-usual-programming-mode-hooks
      (mapcar (lambda (h) (intern (concat (symbol-name h) "-hook")))
              my-usual-programming-modes))

;; (dolist (spec '(("\\.bash.*$"                . ksh-mode)
;;                 ("\\.org$"                   . org-mode)
;;                 ("\\.js$"                    . ecmascript-mode)
;;                 ("\\.gem$"                   . tar-mode)
;;                 ("^\\(GNUm\\|M\\)akefile.*$" . makefile-mode)))
;;   (add-to-list 'auto-mode-alist spec))

;; ;; ============================================================
;; ;; Progress Keyword Highlighting:

(defface my-red-face
  '((t (:foreground "white" :background "red")))
  "A red face for warnings that are bad."
 :group 'my-faces)

(defface my-yellow-face
  '((t (:background "yellow")))
  "A yellow face for warnings that are not quite that bad."
 :group 'my-faces)

(setq yellow-tokens
      (delete ?\s "\\<\\(F IX\\|D OC\\|R ETIRE\\|T ODO\\|W ARN\\).*\\>"))
(setq red-tokens
      (delete ?\s "\\<\\(H ACK\\|R EFACTOR\\).*\\>"))

(mapcar (lambda (mode)
          (font-lock-add-keywords
           mode
           (list (list yellow-tokens 0 ''my-yellow-face 'prepend)
                 (list red-tokens    0 ''my-red-face    'prepend))))
        my-usual-programming-modes)

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
;; ;; Perforce:

;; (p4-set-my-clients '(ryand ryand-itsy ryand-greed ryand-amzn ryand-amzn2))
;; (p4-toggle-vc-mode-off)
;; (p4-set-client-config ".p4config")
;; (p4-set-client-name "ryand")
;; (p4-set-p4-executable "~/Bin/p4")

;; ;; (defun p4-normal () (interactive) (p4-set-p4-port "perforce:1666"))

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

;; ;;; filecache

;; (require 'filecache)

;; (defun my-file-cache ()
;;   (interactive)
;;   (file-cache-clear-cache)
;;   (file-cache-add-directory-list load-path)
;;   (message "Filling file cache")
;;   (file-cache-add-file "~/Work/svn/ruby/ruby_1_8/ruby.h")
;;   (file-cache-add-file "~/Work/svn/ruby/ruby_1_8/intern.h")
;;   (file-cache-add-directory-using-find "~/Bin/elisp")
;;   (file-cache-add-directory-using-find "~/Work/p4/zss/src/RubyInline/dev")
;;   (file-cache-add-directory-using-find "~/Work/p4/zss/src/ZenTest/dev")
;;   (file-cache-add-directory-using-find "~/Work/p4/zss/src/hoe/dev")
;;   (file-cache-add-directory-using-find "~/Work/p4/zss/src/miniunit/dev")
;;   (file-cache-add-directory-using-find "~/Work/p4/zss/src/flog/dev/lib")
;;   (file-cache-add-directory-using-find "~/Work/p4/zss/src/flay/dev/lib")
;;   (file-cache-add-directory-using-find "~/Work/p4/zss/src/ParseTree/dev")
;;   (file-cache-add-directory-using-find "~/Work/p4/zss/src/sexp_processor/dev/lib")
;;   (file-cache-add-directory-using-find "~/Work/p4/zss/src/ruby_parser/dev/lib")
;;   (file-cache-add-directory-using-find "~/Work/p4/zss/src/ruby_parser/dev/test")
;;   (file-cache-add-directory-using-find "~/Work/p4/zss/src/ruby2ruby/dev")
;;   (message "Done filling file cache"))

;; (defun my-file-cache-rubinius ()
;;   (interactive)
;;   (file-cache-clear-cache)
;;   (shell-command "find ~/Work/git/rubinius/{spec,shotgun,kernel,lib} -name \\*.rb -o -name \\*.txt" file-cache-buffer)
;;   (file-cache-add-from-file-cache-buffer)
;;   (kill-buffer file-cache-buffer))

;; (run-with-idle-timer 5 nil #'my-file-cache)

;; (mapcar (lambda (map) (define-key map [S-tab] 'file-cache-minibuffer-complete))
;;         (list minibuffer-local-completion-map
;;               minibuffer-local-map
;;               minibuffer-local-must-match-map))

;; (defun my-add-file-hook ()
;;   "Add the name of the file just opened or written to
;;      `file-name-history'"
;;   (and buffer-file-name
;;        (progn (setq file-name-history
;;                     (delete buffer-file-name file-name-history))
;;               (push buffer-file-name file-name-history)))
;;   nil)

;; (add-hook 'find-file-hooks  'my-add-file-hook)
;; (add-hook 'write-file-hooks 'my-add-file-hook)

;; ;;; pastebin

;; (autoload 'pastebin-buffer "pastebin" "doco" t)
;; (autoload 'pastebin        "pastebin" "doco" t)
;; (hook-after-load ruby
;;   (add-to-list 'pastebin-type-assoc '(ruby-mode . "ruby")))
