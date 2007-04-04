;; ============================================================
;; Requires:

(require 'compile)
(require 'p4)
(require 'autorevert)
; (require 'quack)
; (require 'w3m-load)

;; (require 'ecb-autoloads)
;; (require 'mmm-mode)
;; (require 'mmm-auto)

(autoload 'toggle-buffer "toggle" "doco" t)
(autoload 'autotest-switch "autotest" "doco" t)
(autoload 'autotest "autotest" "doco" t)
(autoload 'which-function "which-func" "doco" t)
(autoload 'slime-setup "slime.el" "slime" t)
(autoload 'slime "slime.el" "slime" t)
(autoload 'semantic-load-enable-code-helpers "semantic" "semantic lib" t)

;; (require 'generic-x)

;; (defmacro with-library (symbol &rest body)
;;   `(condition-case nil
;;        (progn
;;          (require ',symbol)
;;          ,@body)

;;      (error (message "I guess we don't have %s available." ',symbol)
;;             nil)))

;; (defmacro with-library (symbol &rest body)
;;   `(if (require ',symbol nil t)
;;        (progn
;;          ,@body)))

(require 'javascript-mode)
(require 'ecmascript-mode)

;; (with-library javascript-mode
;;               (add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode)))

;; (when (locate-library "javascript")
;;   (autoload 'javascript-mode "javascript" nil t)
;;   (add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode)))

(dolist (spec '(("\\.rb$"     . ruby-mode)
                ("\\.rhtml$"  . html-mode)
                ("\\.bash.*$" . ksh-mode)
                ("\\.org$"    . org-mode)
                ("\\.js$"     . ecmascript-mode)
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
;; Ruby:

(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files")
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(autoload 'ruby-index "ri.el" "ri utility" t)
(autoload 'ri "ri.el" "ri utility" t)
(autoload 'ri-show-term-at-point "ri.el" "ri utility" t)
(autoload 'ri-show-term-composite-at-point "ri.el" "ri utility" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(defconst ruby-expand-list
  (mapcar (lambda (l) (expand-parse (car l) (car (cdr l))))
          '(
            ("shebang" ("#!/usr/local/bin/ruby -w\n\n"))
            ("cls" ("class " n
                    "  def initialize\n"
                    "    " n
                    "  end\nend"))
            ("tst" ("class Test" p " < Test::Unit::TestCase\n"
                    "  def setup\n"
                    "    " n
                    "  end\n\n"
                    "  def test_" n
                    "    " n
                    "  end\nend"))))
  "Expansions for Ruby mode")

(defface my-red-face
  '((t (:foreground "white" :background "red")))
  "A red face for warnings that are not quite that bad."
 :group 'my-faces)

(defface my-yellow-face
  '((t (:background "yellow")))
  "A yellow face for warnings that are not quite that bad."
 :group 'my-faces)

(setq yellow-tokens (delete ?\s "\\<\\(F IX\\|D OC\\|R ETIRE\\|T ODO\\|W ARN\\).*\\>"))
(setq red-tokens (delete ?\s "\\<\\(H ACK\\|R EFACTOR\\).*\\>"))

(mapcar (lambda (mode)
          (font-lock-add-keywords
           mode
           (list (list yellow-tokens 0 ''my-yellow-face 'prepend)
                 (list red-tokens    0 ''my-red-face    'prepend))))
        '(ruby-mode lisp-mode scheme-mode emacs-lisp-mode))

;; (defun setup-abbrevs-for-modes (modes)
;;   (dolist (mode modes)
;;     (setq ,(concat-symbols mode '-mode-abbrev-table) '())))
;; ;;     (add-hook `(concat-symbols ',mode '-mode-hook)
;; ;;               (lambda ()
;; ;;                  (require 'expand)
;; ;;                  (expand-add-abbrevs
;; ;;                   `(concat-symbols ',mode '-mode-abbrev-table)
;; ;;                   `(concat-symbols ',mode '-expand-list))
;; ;;                  (abbrev-mode)))))

;; (setup-abbrevs-for-modes '(ruby scheme))

;; (mapcar
;;  (lambda (mode) (setq (concat-symbols mode '-mode-abbrev-table) '()))
;;  '(ruby scheme))

(setq ruby-mode-abbrev-table '())
(setq save-abbrevs nil)

;; (defun rails-file-cache (dir)
;;   "Adds all the ruby and rhtml files recursively in the current directory to the file-cache"
;;   (interactive "DAdd directory: ")
;;   (require 'filecache)
;;   (define-key minibuffer-local-completion-map [S-tab] 'file-cache-minibuffer-complete)
;;   (define-key minibuffer-local-map [S-tab] 'file-cache-minibuffer-complete)
;;   (define-key minibuffer-local-must-match-map [S-tab] 'file-cache-minibuffer-complete)
;;   (file-cache-clear-cache)
;;   (dolist (subdir '("app" "test" "lib" "db" "config"))
;;     (file-cache-add-directory-recursively (concat dir subdir) "\\.r\\(b\\|html\\|xml\\|js\\)"))
;;   (file-cache-delete-file-regexp "\\.svn"))

(defun ruby-completion-buffer ()
  (interactive)
  (switch-to-buffer "*ruby-completion*")
  (erase-buffer)
  (insert (shell-command-to-string "ri -l")))

;; (defun split-horizontally-not-vertically ()
;;   "If there's only one window (excluding any possibly active minibuffer), then
;;      split the current window horizontally."
;;   (interactive)
;;   (if (= (length (window-list nil 'dont-include-minibuffer-even-if-active)) 1)
;;       (split-window-horizontally)))
;; (add-hook 'temp-buffer-setup-hook 'split-horizontally-not-vertically)

;(add-to-list 'compilation-error-regexp-alist
;     '("test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) \\[\\(.*\\):\\([0-9]+\\)\\]:"
;       1 2))
;(add-to-list 'compilation-error-regexp-alist
;            '("^ *\\[?\\([^:\\n\\r]+\\):\\([0-9]+\\):in"
;              1 2))

(defun ruby-test-function ()
  "Test the current ruby function."
  (interactive)
  (let* ((funname (which-function))
         (fn (and (string-match "#\\(.*\\)" funname) (match-string 1 funname))))
    (compile
     (concat "ruby " (file-name-nondirectory (buffer-file-name)) " -n " fn))))

;; TODO: grab more from http://www.rubygarden.org/ruby?EmacsExtensions

;; FIX!
(setq ruby-program-name "/usr/local/bin/irb")
(setq ri-ri-command "/usr/local/bin/ri")
(setq ri-emacsrb "plain")

(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
             (define-key ruby-mode-map (kbd "C-c C-a") 'autotest-switch)
             (define-key ruby-mode-map (kbd "C-c C-r") 'ri-show-term-at-point)
             (define-key ruby-mode-map (kbd "C-c C-t") 'ri-show-term-composite-at-point)
             (require 'which-func)
             (add-to-list 'which-func-modes 'ruby-mode)
             (which-func-mode)
             (imenu-add-menubar-index)
             (local-set-key [mouse-3] 'imenu)
             (require 'expand)
             (expand-add-abbrevs ruby-mode-abbrev-table ruby-expand-list)
             (abbrev-mode)))

;;  (add-hook 'foo-mode-hook
;;            (lambda ()
;;               (set (make-local-variable imenu-generic-expression)
;;                    '(("Comments" "^\\s-*#" 1)
;;                      ...))))

;; TODO:
;; (setq ruby-imenu-generic-expression
;;       '(("Comments" "^-- \\(.+\\)" 1)
;;         ("Function Definitions" "^\\s-*\\(function\\|procedure\\)[ \n\t]+\\([a-z0-9_]+\\)\
;;  [ \n\t]*([a-z0-9 _,\n\t]*)[ \n\t]*\\(return[ \n\t]+[a-z0-9_]+[ \n\t]+\\)?[ai]s\\b" 2)
;;         ("Function Prototypes" "^\\s-*\\(function\\|procedure\\)[ \n\t]+\\([a-z0-9_]+\\)\
;;  [ \n\t]*([a-z0-9 _,\n\t]*)[ \n\t]*\\(return[ \n\t]+[a-z0-9_]+[ \n\t]*\\)?;" 2)
;;         ("Indexes" "^\\s-*create\\s-+index\\s-+\\(\\w+\\)" 1)
;;         ("Tables" "^\\s-*create\\s-+table\\s-+\\(\\w+\\)" 1)))


;; ============================================================
;; XML:

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
;; MMM

;; (setq mmm-global-mode 'maybe)
;; (setq mmm-submode-decoration-level 2)
;; (set-face-background 'mmm-output-submode-face  "LightBlue")
;; (set-face-background 'mmm-code-submode-face    "LightCyan")
;; (set-face-background 'mmm-comment-submode-face "LightCoral")
;; (mmm-add-classes
;;  '((erb-code
;;     :submode ruby-mode
;;     :match-face (("<%#" . mmm-comment-submode-face)
;;                  ("<%=" . mmm-output-submode-face)
;;                  ("<%"  . mmm-code-submode-face))
;;     :front "<%[#=]?"
;;     :back "%>"
;;     :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
;;              (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
;;              (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @))
;;     )))
;; (add-hook 'html-mode-hook
;;           (lambda ()
;;             (setq mmm-classes '(erb-code))
;;             (mmm-mode-on)
;;             (local-set-key [f8] 'mmm-parse-buffer)))

;; ============================================================
;; Scheme / Lisp:

(setq scheme-program-name "mzscheme")
(setq inferior-lisp-program "/opt/local/bin/sbcl")

(defconst scheme-expand-list
  (mapcar (lambda (l) (expand-parse (car l) (car (cdr l))))
          '(
            ("def" ("(define " n
                    "  (lambda (a)\n"
                    "    (cond\n"
                    "     ((null? a) 'fix)\n"
                    "\n"
                    "     (else 'fix))))\n")))))

(setq scheme-mode-abbrev-table '())

(add-hook 'scheme-mode-hook
          '(lambda ()
             (require 'expand)
             (expand-add-abbrevs scheme-mode-abbrev-table scheme-expand-list)
             (abbrev-mode)))

;; ============================================================
;; Misc Modes/Stuff:

(if running-emacs
    (add-hook 'shell-mode-hook
              '(lambda ()
                 (define-key shell-mode-map
                   (kbd "<M-return>") 'shell-resync-dirs))))

(add-hook 'text-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (define-key text-mode-map (kbd "M-s") 'fixup-whitespace)))

(add-hook 'c-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-r") 'recompile)))


(turn-on-auto-revert-mode)
(global-auto-revert-mode)

(defun my-load-cedet ()
  (interactive)
  (load-file (expand-file-name "~/Bin/elisp/third-party/cedet/common/cedet.el"))
  (require 'cedet)
  (require 'semantic)
  (semantic-load-enable-code-helpers))

; FIX: put this on specific modes
; (global-set-key (kbd "M-<return>") 'complete-tag)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; ============================================================
;; Whitespace Issues:

; (standard-display-ascii ?\t "^I")

(require 'show-wspace)

(dolist (hook '(ruby-mode-hook emacs-lisp-mode-hook))
  (add-hook hook
            (lambda ()
              (highlight-tabs)
              (highlight-trailing-whitespace))))
