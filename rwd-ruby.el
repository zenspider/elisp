
;; autoloads

;;;###autoload
(progn
  (autoload 'autotest                        "autotest"        "doco" t)
  (autoload 'autotest-switch                 "autotest"        "doco" t)
  (autoload 'inf-ruby-keys                   "inf-ruby"        "doco"  )
  (autoload 'racc-mode                       "racc-mode"       "doco"  )
  (autoload 'rcov-buffer                     "rcov-overlay.el" "doco" t)
  (autoload 'ri                              "ri.el"           "doco" t)
  (autoload 'ri-show-term-at-point           "ri.el"           "doco" t)
  (autoload 'ri-show-term-composite-at-point "ri.el"           "doco" t)
  (autoload 'ruby-index                      "ri.el"           "doco" t)
  (autoload 'ruby-mode                       "ruby-mode"       "doco"  )
  (autoload 'run-ruby                        "inf-ruby"        "doco"  )
  (autoload 'yaml-mode                       "yaml-mode"       "doco"  ))

;; auto-modes

;;;###autoload
(dolist (spec '(("\\.mab$"   . ruby-mode)
                ("\\.rb$"    . ruby-mode)
                ("Rakefile"  . ruby-mode)
                ("\\.rake$"  . ruby-mode)
                ("\\.rhtml$" . html-mode)
                ("\\.yml$"   . yaml-mode)
                ("ruby"      . ruby-mode)
                ("\\.gem$"   . tar-mode)))
  (add-to-list 'auto-mode-alist spec))

;; (setq ruby-mode-abbrev-table '())
;; (setq ruby-program-name "/usr/bin/irb")
;; (setq ri-ri-command "/usr/bin/ri")
;; (setq ri-emacsrb "plain")

;; (defun my-ruby-sexp (start end)
;;   (interactive "r")
;;   (save-excursion
;;     (save-match-data
;;       (replace-regexp "]" ")" nil start end)
;;       (replace-regexp "\\[" "s(" nil start end))))

;; (defun expand-parse (name l &optional str pos)
;;   "Expands a specially encoded list for expand-mode."
;;   (cond ((null l)
;;          (list name str (reverse pos)))
;;         ((equal 'n (car l))
;;          (expand-parse name (cdr l) (concat str "\n")
;;                        (cons (1+ (length str)) pos)))
;;         ((equal 'p (car l))
;;          (expand-parse name (cdr l) str (cons (1+ (length str)) pos)))
;;         (t (expand-parse name (cdr l) (concat str (car l)) pos))))

;; (defconst ruby-expand-list
;;   (mapcar (lambda (l) (expand-parse (car l) (car (cdr l))))
;;           '(
;;             ("cls" ("class " n
;;                     "  def initialize\n"
;;                     "    " n
;;                     "  end\nend"))
;;             ("tst" ("class Test" p " < Test::Unit::TestCase\n"
;;                     "  def setup\n"
;;                     "    " n
;;                     "  end\n\n"
;;                     "  def test_" n
;;                     "    " n
;;                     "  end\nend"))))
;;   "Expansions for Ruby mode")

;; (require 'expand)
;; (defconst text-expand-list
;;   (mapcar (lambda (l) (expand-parse (car l) (car (cdr l))))
;;           '(("*change" ("=== X.Y.Z / YYYY-MM-DD\n\n* N major enhancements:\n\n  * blah\n\n* N minor enhancements:\n\n  * blah\n\n* N bug fixes:\n\n  * blah"))))
;;   "Expansions for text mode")

;; ;; (expand-add-abbrevs text-mode-abbrev-table text-expand-list)
;; ;; (abbrev-mode)

;; ;; (require 'yasnippet)
;; ;; (yas/initialize)
;; ;; (yas/load-directory "~/Bin/elisp/third-party/yasnippet/snippets")
;; ;; (setq yas/window-system-popup-function #'yas/text-popup-for-template)
;; ;; (setq yas/text-popup-function #'yas/text-popup-for-template)
;; ; #'yas/dropdown-list-popup-for-template)
;; ; #'yas/dropdown-list-popup-for-template)
;; ; #'yas/x-popup-menu-for-template
;; ; (dropdown-list-at-point load-path)


;; (defun ruby-completion-buffer ()
;;   (interactive)
;;   (switch-to-buffer "*ruby-completion*")
;;   (erase-buffer)
;;   (insert (shell-command-to-string "ri -l")))

;; (add-hook 'ruby-mode-hook
;;           '(lambda ()
;;              (inf-ruby-keys)
;;              (define-key ruby-mode-map (kbd "C-c C-a") 'autotest-switch)
;;              (define-key ruby-mode-map (kbd "C-c C-r") 'rcov-buffer)
;;              (define-key ruby-mode-map (kbd "C-c C-t") 'ri-show-term-composite-at-point)

;;              (require 'which-func)
;;              (add-to-list 'which-func-modes 'ruby-mode)
;;              (which-func-mode)
;;              (imenu-add-menubar-index)
;;              (local-set-key [mouse-3] 'imenu)
;;              (require 'expand)
;; ;;             (expand-add-abbrevs ruby-mode-abbrev-table ruby-expand-list)
;; ;;              ;; Don't want flymake mode for ruby regions in rhtml files
;; ;;              (if (not (null buffer-file-name)) (flymake-mode))
;; ;;             (abbrev-mode)
;;              ))

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

;; (require 'blank-mode)
;; (add-hook 'ruby-mode-hook (lambda () (blank-mode)))
