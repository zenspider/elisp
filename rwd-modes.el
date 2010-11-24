;; ============================================================
;; Requiresx:

;;;###autoload
(progn
  (autoload 'etags-select-find-tag          "etags-select")
  (autoload 'etags-select-find-tag-at-point "etags-select")
  (autoload 'magit-status                   "magit" nil t)
  (autoload 'run-scheme                     "cmuscheme" nil t)
  (autoload 'scheme-smart-complete          "scheme-complete" nil t)
  (autoload 'scheme-complete-or-indent      "scheme-complete" nil t)
  (autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
  (autoload 'ssh                            "ssh" nil t))

;;;###autoload
(setq my-usual-programming-modes
      '(ruby-mode lisp-mode scheme-mode emacs-lisp-mode))

;;;###autoload
(dolist (spec '(("\\.bash.*$"                . ksh-mode)
                ("\\.js$"                    . ecmascript-mode)
                ("\\.haml$"                  . haml-mode)
                ("^\\(GNUm\\|M\\)akefile.*$" . makefile-mode)))
  (add-to-list 'auto-mode-alist spec))

;;;###autoload
(add-hook 'scheme-mode-hook
          (lambda ()
            (make-local-variable 'eldoc-documentation-function)
            (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
            (eldoc-mode)))

;;;###autoload
(eval-after-load 'scheme
  '(progn
     (require 'scheme-complete)
     (define-key scheme-mode-map "\t" 'scheme-complete-or-indent)))

;;;###autoload
(add-hook 'ssh-mode-hook 'ssh-directory-tracking-mode)

;; ============================================================
;; Misc Modes/Stuff:

;;;###autoload
(autoload 'kill-ring-search "kill-ring-search"
  "Search borkkk the kill ring in the minibuffer." t)

;;;###autoload
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;;###autoload
(progn
  (defun sm-try-smerge ()
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " 20480 t)
        (smerge-mode 1))))

  (add-hook 'find-file-hook 'sm-try-smerge t))
