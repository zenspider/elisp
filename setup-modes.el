;; ============================================================
;; Requires:

(require 'compile)
(require 'p4)
(require 'autorevert)
;; (require 'ecb-autoloads)
;; (require 'mmm-mode)
;; (require 'mmm-auto)

(autoload 'toggle-buffer "toggle" "doco" t)
(autoload 'autotest-switch "autotest" "doco" t)
(autoload 'autotest "autotest" "doco" t)

;; ============================================================
;; Simple mode toggles:

(setq
 tramp-default-method "ssh"
;;  ecb-toggle-layout-sequence '("left10" "left9" "left6")
 compilation-error-regexp-alist '(bash java gcc-include gnu))

;; ============================================================
;; Ruby:

(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files")
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(autoload 'ruby-index "ri.el" "ri utility" t)
(autoload 'ri "ri.el" "ri utility" t)
(autoload 'ri-show-term-at-point "ri.el" "ri utility" t)
(autoload 'ri-show-term-composite-at-point "ri.el" "ri utility" t)
(autoload 'slime-setup "slime.el" "slime" t)
(autoload 'slime "slime.el" "slime" t)
(autoload 'semantic-load-enable-code-helpers "semantic" "semantic lib" t)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(defun expand-parse (name l &optional str pos)
  (cond ((null l)
         (list name str (reverse pos)))
        ((equal 'n (car l))
         (expand-parse name (cdr l) (concat str "\n")
                       (cons (1+ (length str)) pos)))
        ((equal 'p (car l))
         (expand-parse name (cdr l) str (cons (1+ (length str)) pos)))
        (t (expand-parse name (cdr l) (concat str (car l)) pos))))

(defun insert-modeline ()
  (interactive)
  (let ((mode (symbol-name major-mode)))
    (insert "-*- ")
    (comment-region (line-beginning-position) (line-end-position))
    (insert (substring mode 0 (- (length mode) 5)))
    (insert " -*-")
    (insert "\n")))

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

(setq ruby-mode-abbrev-table '())
(setq save-abbrevs nil)

;(add-to-list 'compilation-error-regexp-alist
;     '("test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) \\[\\(.*\\):\\([0-9]+\\)\\]:"
;       1 2))
;(add-to-list 'compilation-error-regexp-alist
;            '("^ *\\[?\\([^:\\n\\r]+\\):\\([0-9]+\\):in"
;              1 2))

;; (defun ruby-test-function ()
;;   "Test the current ruby function (must be runable via ruby <buffer> --name <test>)."
;;   (interactive)
;;   (let* ((funname (which-function))
;;          (fn (and (string-match "#\\(.*\\)" funname) (match-string 1 funname))))
;;     (compile (concat "ruby " (file-name-nondirectory (buffer-file-name)) " --name " fn))))

(setq ruby-program-name "/usr/local/bin/irb")

(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
             (define-key ruby-mode-map (kbd "C-c C-a") 'autotest-switch)
             (define-key ruby-mode-map (kbd "C-c C-r") 'ri-show-term-at-point)
             (define-key ruby-mode-map (kbd "C-c C-t") 'ri-show-term-composite-at-point)
             (imenu-add-menubar-index)
             (require 'expand)
             (expand-add-abbrevs ruby-mode-abbrev-table ruby-expand-list)
             (abbrev-mode)))

;; FIX!
(setq ri-ri-command "/usr/local/bin/ri")
(setq ri-emacsrb "plain")

(setq scheme-program-name "/usr/local/bin/mzscheme")
(setq inferior-lisp-program "/opt/local/bin/sbcl")

(defun rb-compile-command (filename)
    "Find the unit test script for testing FILENAME.  I always organize my
packages in the same way.  The unit_test.rb script is in the package root.  The
individual unit tests go under root/test and the source goes under
root/lib/whatever.  This function figures out what the root should be, and then
sees if there's a unit_test.rb there.  If it can't find it at all, it just runs
ruby on the file I'm visiting."

    (let* ((pkg-root (cond
                      ((string-match "^\\(.*\\)/lib/.*$" filename)
                       (match-string 1 filename))
                      ((string-match "^\\(.*\\)/test/.*$" filename)
                       (match-string 1 filename))
                      ((string-match "^\\(.*\\)/.*$" filename)
                       (match-string 1 filename))))
           (unit-test (concat pkg-root "/unit_test.rb")))
      (if (file-readable-p unit-test)
          (concat "ruby " unit-test)
        (concat "ruby " filename))))

;; TODO: grab more from http://www.rubygarden.org/ruby?EmacsExtensions

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
;;             (mmm-mode-on)))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
;; (global-set-key [f8] 'mmm-parse-buffer)

;; ============================================================
;; Misc Modes/Stuff:

(add-to-list 'auto-mode-alist '("\\.bash.*$" . ksh-mode))
(add-to-list 'auto-mode-alist
             '("^I?\\(M\\|m\\|GNUm\\)akefile.*$" . makefile-mode))

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

;; ============================================================
;; PAbbrev:

;; (require 'pabbrev)
;; (dolist (hook '(text-mode-hook
;;                 html-mode-hook
;;                 emacs-lisp-mode-hook
;;                 latex-mode-hook
;;                 ruby-mode-hook))
;;   (add-hook hook (lambda () (pabbrev-mode))))

