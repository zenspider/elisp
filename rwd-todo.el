(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq rwd-time-formats (make-hash-table))
(puthash 'date     "%Y-%m-%d"          rwd-time-formats)
(puthash 'datetime "%Y-%m-%dT%H:%M:%S" rwd-time-formats)
(puthash 'time     "%H:%M:%S"          rwd-time-formats)

(maphash
 (lambda (name format)
   `(defun ,(intern (concat "rwd-insert" (symbol-name name))) ()
     (interactive)
     (insert (format-time-string format)))
   `(defun ,(intern (concat "rwd-" (symbol-name name))) ()
     (format-time-string format)))
 rwd-time-formats)

;; TODO: fix the above


(defsubst goto-previous-mark ()
  "Jump to previous mark, rotating the (local) `mark-ring'.
  Does not affect the `global-mark-ring'.
  This is equivalent to `set-mark-command' with a non-nil argument."
  (interactive) (set-mark-command t))

(global-set-key (kbd "C-c SPC") 'goto-previous-mark)
(global-set-key (kbd "C-x SPC") 'goto-previous-global-mark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for rwd-ruby.el:

;; TODO: this kinda sorta works, but I'm not sure if I like it.
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

;; TODO: switch to rinari once I get the kinks worked out.
;; ============================================================
;; MMM

;; (require 'mmm-mode)
;; (require 'mmm-auto)
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

;; TODO: tweak out imenu
;; (setq ruby-imenu-generic-expression
;;       '(("Comments" "^-- \\(.+\\)" 1)
;;         ("Function Definitions" "^\\s-*\\(function\\|procedure\\)[ \n\t]+\\([a-z0-9_]+\\)\
;;  [ \n\t]*([a-z0-9 _,\n\t]*)[ \n\t]*\\(return[ \n\t]+[a-z0-9_]+[ \n\t]+\\)?[ai]s\\b" 2)
;;         ("Function Prototypes" "^\\s-*\\(function\\|procedure\\)[ \n\t]+\\([a-z0-9_]+\\)\
;;  [ \n\t]*([a-z0-9 _,\n\t]*)[ \n\t]*\\(return[ \n\t]+[a-z0-9_]+[ \n\t]*\\)?;" 2)
;;         ("Indexes" "^\\s-*create\\s-+index\\s-+\\(\\w+\\)" 1)
;;         ("Tables" "^\\s-*create\\s-+table\\s-+\\(\\w+\\)" 1)))
