;; (inf-ruby-setup-keybindings)

(define-key enh-ruby-mode-map (kbd "C-c C-a") 'autotest-switch)
(define-key enh-ruby-mode-map (kbd "C-c C-p") 'pastebin)
(define-key enh-ruby-mode-map (kbd "C-c C-r") 'rcov-buffer)
(define-key enh-ruby-mode-map (kbd "C-c C-b") 'ruby-run-buffer-clean)
(define-key enh-ruby-mode-map (kbd "C-c C-t") 'ri-show-term-composite-at-point)
(define-key enh-ruby-mode-map (kbd "C-c C-c") 'recompile)

(define-key enh-ruby-mode-map (kbd "C-c /") nil) ; I hate enh-ruby-insert-end

;; TODO: this doesn't affect spawning compilations windows?!?
(set (make-local-variable 'compilation-error-regexp-alist)
     '(
       ("\\[\\(.*\\):\\([0-9]+\\)\\]:$" 1 2)
       ("^ *\\(?:from \\)?\\([[+]\\)?\\([^:
]+\\):\\([0-9]+\\):in" 2 3)
       ("^.* at \\([^:]*\\):\\([0-9]+\\)$" 1 2)
       ))

(setq compilation-error-regexp-alist ;; HACK fix this for C-c C-c from here
      '(
        ("\\[\\(.*\\):\\([0-9]+\\)\\]:$" 1 2)
        ("^ *\\(?:from \\)?\\([[+]\\)?\\([^:
]+\\):\\([0-9]+\\):in" 2 3)
        ("^.* at \\([^:]*\\):\\([0-9]+\\)$" 1 2)
        ))

(add-hook 'compilation-filter-hook 'rwd-ansi-colorize-strip)
;; (remove-hook 'compilation-filter-hook 'rwd-ansi-colorize)

;; ;; TODO: fix this for just C-c C-c
;; (setq comint-output-filter-functions
;;       '(fuck-me
;;         comint-truncate-buffer
;;         comint-postoutput-scroll-to-bottom))
;;
;; (setq comint-preoutput-filter-functions
;;       '(fuck-me))

(defun ruby-outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.
This is actually either the level specified in `outline-heading-alist'
or else the number of characters matched by `outline-regexp'."

  (let ((level (or (and (looking-at outline-regexp)
                        (match-string 1)
                        (- (match-end 1) (match-beginning 1)))
                   0)))
    (1+ (/ level 2))))

(set (make-local-variable 'outline-level) 'ruby-outline-level)

(setq outline-regexp-ruby
      (rx (group (* " "))
          bow
          (or "BEGIN" "END" "begin" "case" "class" "def" "else" "elsif" "loop"
              ;; "end"
              "ensure" "for" "if" "module" "rescue" "unless" "until"
              "when" "while")
          eow))

(defun enh-update-outline ()
  (interactive)

  (set (make-local-variable 'outline-regexp) outline-regexp-ruby))

(require 'eval-in-repl)
(require 'eval-in-repl-ruby)
(define-key enh-ruby-mode-map (kbd "<C-return>") 'eir-eval-in-ruby)

(enh-update-outline)
(outline-minor-mode)

(imenu-add-menubar-index)
(flyspell-prog-mode)

(dumb-jump-mode)

;; enh-ruby-mode hooks into expand-region:

(require 'expand-region-core)

;; (defun er/add-enh-ruby-mode-expansions ()
;;   "Adds enh-ruby-specific expansions for buffers in enh-ruby-mode"
;;   (require 'ruby-mode-expansions)
;;
;;   (defalias 'er/mark-enh-ruby-instance-variable 'er/mark-ruby-instance-variable)
;;   (defalias 'er/mark-enh-ruby-block-up          'er/mark-ruby-block-up)
;;
;;   (set (make-local-variable 'er/try-expand-list)
;;        (append
;;         (remove 'er/mark-defun er/try-expand-list)
;;         '(er/mark-enh-ruby-instance-variable
;;           er/mark-enh-ruby-block-up))))
;;
;; (er/enable-mode-expansions 'enh-ruby-mode 'er/add-enh-ruby-mode-expansions)
