;; ============================================================
;; Requires:

(require 'compile)
(require 'p4)
(require 'autorevert)
(require 'ecmascript-mode)

; (require 'quack)
; (require 'w3m-load)

(autoload 'toggle-buffer "toggle" "doco" t)
(autoload 'toggle-style "toggle" "doco" t)
(autoload 'which-function "which-func" "doco" t)
(autoload 'slime-setup "slime.el" "slime" t)
(autoload 'slime "slime.el" "slime" t)
(autoload 'semantic-load-enable-code-helpers "semantic" "semantic lib" t)
(autoload 'etags-select-find-tag-at-point "etags-select" "doco" t)

(setq my-usual-programming-modes
      '(ruby-mode lisp-mode scheme-mode emacs-lisp-mode))

(dolist (spec '(("\\.bash.*$" . ksh-mode)
                ("\\.org$"    . org-mode)
                ("\\.js$"     . ecmascript-mode)
                ("\\.gem$"     . tar-mode)
                ("^\\(GNUm\\|M\\)akefile.*$" . makefile-mode)))
  (add-to-list 'auto-mode-alist spec))

; (require 'rinari)

;; ============================================================
;; Org Mode:

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-tag-alist '(("brainstorm" . ?b) ("code" . ?c) ("email" . ?e) ("logistics" . ?l) ("online" . ?o) ("research" . ?r) ("write" . ?w)))
(setq org-agenda-directory "~/Documents/org/")
(setq org-agenda-files
      (directory-files (expand-file-name org-agenda-directory) t "^.*\\.org$"))

;; ============================================================
;; Progress Keyword Highlighting:

(defface my-red-face
  '((t (:foreground "white" :background "red")))
  "A red face for warnings that are not quite that bad."
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

;; TODO:
;; (defun setup-abbrevs-for-modes (modes)
;;   (dolist (mode modes)
;;     (setq ,(concat-symbols mode '-mode-abbrev-table) '())))

;; (add-hook `(concat-symbols ',mode '-mode-hook)
;;           (lambda ()
;;             (require 'expand)
;;             (expand-add-abbrevs
;;              `(concat-symbols ',mode '-mode-abbrev-table)
;;              `(concat-symbols ',mode '-expand-list))
;;             (abbrev-mode)))))

;; (setup-abbrevs-for-modes '(ruby scheme))

;; (mapcar
;;  (lambda (mode) (setq (concat-symbols mode '-mode-abbrev-table) '()))
;;  '(ruby scheme))

(setq save-abbrevs nil)

;; TODO:
;;  (add-hook 'foo-mode-hook
;;            (lambda ()
;;               (set (make-local-variable imenu-generic-expression)
;;                    '(("Comments" "^\\s-*#" 1)
;;                      ...))))

;; ============================================================
;; XML:

;; TODO:
;; (add-to-list 'auto-mode-alist
;;              (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng"
;;                                                "xslt" "svg" "rss") t) "\\'")
;;                    'nxml-mode))

;; ============================================================
;; Perforce:

(p4-set-my-clients '(ryand ryand-itsy ryand-greed ryand-amzn ryand-amzn2))
(p4-toggle-vc-mode-off)
(p4-set-client-config ".p4config")
(p4-set-client-name "ryand")
(p4-set-p4-executable "~/Bin/p4")

(defun p4-normal () (interactive) (p4-set-p4-port "perforce:1666"))

;; ============================================================
;; Scheme / Lisp:

(setq scheme-program-name "mzscheme")
(setq inferior-lisp-program "/opt/local/bin/sbcl")

;; (defconst scheme-expand-list
;;   (mapcar (lambda (l) (expand-parse (car l) (car (cdr l))))
;;           '(
;;             ("def" ("(define " n
;;                     "  (lambda (a)\n"
;;                     "    (cond\n"
;;                     "     ((null? a) 'fix)\n"
;;                     "\n"
;;                     "     (else 'fix))))\n")))))

;; (setq scheme-mode-abbrev-table '())

;; (add-hook 'scheme-mode-hook
;;           '(lambda ()
;;              (require 'expand)
;;              (expand-add-abbrevs scheme-mode-abbrev-table scheme-expand-list)
;;              (abbrev-mode)))

;; ============================================================
;; Misc Modes/Stuff:

(if running-emacs
    (def-hook shell-mode
      (define-key shell-mode-map
        (kbd "<M-return>") 'shell-resync-dirs)))

(def-hook text-mode
  (turn-on-auto-fill)
  (define-key text-mode-map (kbd "M-s") 'fixup-whitespace))

(def-hook c-mode
  (local-set-key (kbd "C-c C-r") 'recompile))

(turn-on-auto-revert-mode)
(global-auto-revert-mode)

(defun my-load-cedet ()
  (interactive)
  (load-file (expand-file-name "~/Bin/elisp/third-party/cedet/common/cedet.el"))
  (require 'cedet)
  (require 'semantic)
  (semantic-load-enable-code-helpers))

;; FIX: put this on specific modes
;; (global-set-key (kbd "M-<return>") 'complete-tag)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; ============================================================
;; Whitespace Issues:

;; (standard-display-ascii ?\t "^I")

(require 'show-wspace)

(dolist (hook my-usual-programming-modes)
  (add-hook hook
            (lambda ()
              (show-ws-highlight-tabs)
              (show-ws-highlight-trailing-whitespace))))

(require 'filecache)

(defun my-file-cache ()
  (interactive)
  (file-cache-clear-cache)
  (file-cache-add-directory-list load-path)
  (file-cache-add-directory-using-find "~/Work/p4/zss/src/ParseTree/dev")
  (file-cache-add-directory-using-find "~/Work/p4/zss/src/ruby_parser/dev")
  (file-cache-add-directory-using-find "~/Work/p4/zss/src/ruby2ruby/dev"))

(my-file-cache)

(mapcar (lambda (map) (define-key map [S-tab] 'file-cache-minibuffer-complete))
        (list minibuffer-local-completion-map
              minibuffer-local-map
              minibuffer-local-must-match-map))
