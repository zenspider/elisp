;; (package-initialize)

(require 'autoload)                     ; = ;;;###autoload

(unless user-init-file                  ; if running w/: -q --debug-init
  (setq user-init-file (expand-file-name "~/.emacs.el")))

(defvar user-init-dir (file-name-directory
                       (or (file-symlink-p user-init-file)
                           (or user-init-file
                               (expand-file-name "~/.emacs.el"))))
  "Root directory of emacs.el, after following symlinks, etc.")

(setq custom-file (concat user-init-dir "custom.el"))

(add-to-list 'load-path user-init-dir t)
(add-to-list 'load-path (concat user-init-dir "third-party") t) ; TODO: remove

(let* ((os-name     (symbol-name system-type))
       (host-list   (split-string (system-name) "\\."))
       (host-name   (car host-list))
       (domain-name (mapconcat 'identity (cdr host-list) ".")))
  (load (concat "os/" os-name) t)         ;; os/darwin
  (load (concat "domain/" domain-name) t) ;; domain/zenspider.com
  (load (concat "host/" host-name) t))    ;; host/greed

(load "rwd-autoloads")
(load "rwd-packages")

(rwd-autoloads)
(rwd-autohooks)

(load custom-file)

(put 'narrow-to-region 'disabled nil)
