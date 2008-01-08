(require 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.mab$"   . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$"    . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile"  . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(autoload 'autotest-switch "autotest" "doco" t)
(autoload 'autotest "autotest" "doco" t)
(autoload 'ruby-mode "ruby-mode" "doco")
(autoload 'run-ruby "inf-ruby" "doco")
(autoload 'inf-ruby-keys "inf-ruby" "doco")
(autoload 'ruby-index "ri.el" "doco" t)
(autoload 'ri "ri.el" "doco" t)
(autoload 'ri-show-term-at-point "ri.el" "doco" t)
(autoload 'ri-show-term-composite-at-point "ri.el" "doco" t)

(setq ruby-mode-abbrev-table '())
(setq ruby-program-name "/usr/local/bin/irb")
(setq ri-ri-command "/usr/local/bin/ri")
(setq ri-emacsrb "plain")

(defun expand-parse (name l &optional str pos)
  "Expands a specially encoded list for expand-mode."
  (cond ((null l)
         (list name str (reverse pos)))
        ((equal 'n (car l))
         (expand-parse name (cdr l) (concat str "\n")
                       (cons (1+ (length str)) pos)))
        ((equal 'p (car l))
         (expand-parse name (cdr l) str (cons (1+ (length str)) pos)))
        (t (expand-parse name (cdr l) (concat str (car l)) pos))))

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

(defun ruby-completion-buffer ()
  (interactive)
  (switch-to-buffer "*ruby-completion*")
  (erase-buffer)
  (insert (shell-command-to-string "ri -l")))

;; TODO: grab more from http://www.rubygarden.org/ruby?EmacsExtensions

;; (require 'flymake)

;; (defun flymake-ruby-init ()
;;   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;          (local-file  (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;     (list "ruby" (list "-c" local-file))))
;; (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
;; (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
;; (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

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
;;              ;; Don't want flymake mode for ruby regions in rhtml files
;;              (if (not (null buffer-file-name)) (flymake-mode))
             (abbrev-mode)))

(defun rails-file-cache (dir)
  "Adds all the ruby and rhtml files recursively in the current directory to the file-cache"
  (interactive "DAdd directory: ")
  (require 'filecache)
  (define-key minibuffer-local-completion-map [S-tab] 'file-cache-minibuffer-complete)
  (define-key minibuffer-local-map [S-tab] 'file-cache-minibuffer-complete)
  (define-key minibuffer-local-must-match-map [S-tab] 'file-cache-minibuffer-complete)
  (file-cache-clear-cache)
  (dolist (subdir '("app" "test" "lib" "db" "config"))
    (file-cache-add-directory-recursively (concat dir subdir) "\\.r\\(b\\|html\\|xml\\|js\\)"))
  (file-cache-delete-file-regexp "\\.svn"))

(require 'show-wspace)
;; (dolist (hook '(ruby-mode-hook))
;;   (add-hook hook
;;             (lambda ()
;;               (show-ws-highilight-tabs)
;;               (show-ws-highlight-trailing-whitespace))))
