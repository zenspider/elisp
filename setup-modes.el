;; ============================================================
;; Requires:

(require 'compile)
(require 'p4)
(require 'ecb-autoloads)

;; ============================================================
;; Java:

; (require 'jde) ; FIX: lost eieio, need to reinstall

; (require 'antlr-mode)
; (autoload 'antlr-set-tabs "antlr-mode")
; (add-hook 'java-mode-hook 'antlr-set-tabs)

(add-hook 'java-mode-hook
	  '(lambda ()
	     (local-set-key "\C-\c\C-r" 'recompile)))

;; ============================================================
;; Ruby:

(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
	     (define-key ruby-mode-map "\M-\C-x" 'bury-buffer)
	     (setq ruby-program-name "/usr/local/bin/ruby")
	     ;(require 'ruby-electric)
	     ;(ruby-electric-mode)
	     ))

(autoload 'ruby-index "ri.el" "ri utility" t)
(autoload 'ri "ri.el" "ri utility" t)
(autoload 'ri-show-term-at-point "ri.el" "ri utility" t)
(autoload 'ri-show-term-composite-at-point "ri.el" "ri utility" t)

(global-set-key "\C-c\C-c\C-r" 'ri-show-term-at-point)
(global-set-key "\C-c\C-c\C-t" 'ri-show-term-composite-at-point)

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

(add-to-list 'auto-mode-alist
	     (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng"
					       "xslt" "svg" "rss") t) "\\'")
		   'nxml-mode))

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

(require 'mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)
(set-face-background 'mmm-output-submode-face  "LightBlue")
(set-face-background 'mmm-code-submode-face    "LightCyan")
(set-face-background 'mmm-comment-submode-face "LightCoral")
(mmm-add-classes
 '((erb-code
    :submode ruby-mode
    :match-face (("<%#" . mmm-comment-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[#=]?" 
    :back "%>" 
    :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
             (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
             (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @))
    )))
(add-hook 'html-mode-hook
          (lambda ()
            (setq mmm-classes '(erb-code))
            (mmm-mode-on)))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
;; (global-set-key [f8] 'mmm-parse-buffer)

;; ============================================================
;; Misc Modes/Stuff:

(resize-minibuffer-mode 1)

(cond (running-xemacs
       (require 'func-menu)
       (add-hook 'find-file-hooks 'fume-add-menubar-entry)
       (define-key global-map "\C-cl" 'fume-list-functions)      
       (define-key global-map "\C-cg" 'fume-prompt-function-goto) 
       (define-key global-map '(button3) 'mouse-function-menu)
       
       ;; For descriptions of the following user-customizable variables,
       ;; type C-h v <variable>
       (setq
	diff-switches nil		; -c doesn't work well with GNU emacs
	fume-fn-window-position 3
	fume-auto-position-popup t
	fume-display-in-modeline-p t
	fume-buffer-name "*Function List*"
	fume-no-prompt-on-valid-default nil)
       ))

(add-to-list 'auto-mode-alist '("\\.bash.*$" . ksh-mode))
(add-to-list 'auto-mode-alist
	     '("^I?\\(M\\|m\\|GNUm\\)akefile.*$" . makefile-mode))

(add-hook 'shell-mode-hook
          '(lambda ()
	     (define-key shell-mode-map 
	       (kbd "<M-return>") 'shell-resync-dirs)))

(add-hook 'text-mode-hook
          '(lambda ()
	     (turn-on-auto-fill)
	     (define-key text-mode-map "\M-s" 'fixup-whitespace)))

(add-hook 'c-mode-hook
	  '(lambda ()
	     (local-set-key "\C-\c\C-r" 'recompile)))

(require 'autorevert)
(turn-on-auto-revert-mode)
(global-auto-revert-mode)

; "trivial-mode" defines external programs to open files
(defun define-trivial-mode(mode-prefix file-regexp &optional command)
  (or command
      (setq command mode-prefix))
  (let ((mode-command (intern (concat mode-prefix "-mode"))))
    (fset mode-command
          `(lambda ()
             (interactive)
             (start-process ,mode-prefix (current-buffer)
                            ,command (buffer-file-name))))
    (add-to-list 'auto-mode-alist (cons file-regexp mode-command))))
(define-trivial-mode "gv" "\\.ps$")
(define-trivial-mode "gv" "\\.pdf$")

(add-hook 'after-save-hook
	  '(lambda ()
             (progn
               (and (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (save-match-data (looking-at "^#!"))))
                    (shell-command (concat "chmod u+x " buffer-file-name))
                    (message (concat "Saved as script: " buffer-file-name))))))


(load-file (expand-file-name "~/Bin/elisp/third-party/cedet/common/cedet.el"))
(require 'cedet)
(require 'semantic)
(semantic-load-enable-code-helpers)

