; Compatibility Layer:

(setq running-xemacs (featurep 'xemacs))
(setq running-emacs (not running-xemacs))

(if running-xemacs
    (add-to-list 'Info-directory-list "/usr/share/info"))

(if running-emacs
    (progn
      (eval-when-compile
        (when (< emacs-major-version 20) (require 'cl)))
      (require 'vc-hooks)
      (setq apropos-do-all t)
      (transient-mark-mode 1)

      (add-hook 'comint-output-filter-functions
                'comint-watch-for-password-prompt)))

; Pathing:
(dolist (path (split-string
               (shell-command-to-string
                "find ~/Bin/elisp -maxdepth 2 -type d | sort -u") nil))
  (add-to-list 'load-path path))

(add-to-list 'load-path "~/Sites/emacs/elisp" t)

(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer." t)

; My libs:

(load "setup-aliases")
(load "setup-keys")
(load "setup-mail-and-news")
(load "setup-misc")
(load "setup-modes")
(load "setup-ruby")

(require 'window-numbering)
(window-numbering-mode 1)

;; (setq window-numbering-assign-func
;;       '(lambda () (when (equal (buffer-name) "*Calculator*") 9)))

(if (featurep 'mac-carbon)
    ;; deal with OSX's wonky enivronment by forcing PATH to be correct.
    (progn
      (setenv "PATH"
              (shell-command-to-string
               "/bin/bash -lc 'echo -n $PATH'")))
      (setenv "CDPATH"
              (shell-command-to-string
               "/bin/bash -lc 'echo -n $CDPATH'")))

  (let ((extra-path-dirs '("/opt/local/bin"
                         "/MyApplications/dev/lisp/PLTScheme/bin"
                         "/opt/local/lib/postgresql82/bin"))
      (path (split-string (getenv "PATH") ":" t)))
  (progn
    (dolist (dir extra-path-dirs)
      (add-to-list 'path dir t))
    (setenv "PATH" (list-join ":" path))))

(dolist (path (split-string (getenv "PATH") ":" t)) ; argh this is stupid
  (add-to-list 'exec-path path t))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(antlr-language-alist (quote ((java-mode "Java" nil "\"Java\"" "Java") (c++-mode "C++" "\"Cpp\"" "Cpp") (ruby-mode "Ruby" nil "\"Ruby\"" "Ruby"))))
 '(column-number-mode t)
 '(compilation-error-regexp-alist (quote (bash java gnu gcc-include)))
 '(dired-recursive-deletes (quote top))
 '(ecb-layout-window-sizes (quote (("left9" (0.325 . 0.975)))))
 '(ecb-options-version "2.32")
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(explicit-shell-file-name "/bin/bash")
 '(find-file-visit-truename t)
 '(global-font-lock-mode t nil (font-core))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-splash-screen t)
 '(locate-make-command-line (quote locate-make-mdfind-command-line))
 '(mouse-wheel-mode t nil (mwheel))
 '(org-agenda-files (quote ("~/Documents/org/work.org" "~/Documents/org/ruby.org" "~/Documents/org/home.org")))
 '(save-place t nil (saveplace))
 '(save-place-limit 250)
 '(search-highlight t)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-mode nil)
 '(tramp-default-method "ssh")
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(use-dialog-box nil)
 '(vc-handled-backends (quote (CVS SVN)))
 '(vc-path (quote ("/opt/local/bin" "/usr/local/bin")))
 '(vc-svn-program-name "/opt/local/bin/svn")
 '(visible-bell t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "Dark Blue"))))
 '(font-lock-constant-face ((((class color) (min-colors 88) (background light)) (:foreground "SlateBlue4"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "Forest Green")))))
