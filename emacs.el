;; see also: early-init.el

(unless user-init-file                  ; if running w/: -q --debug-init
  (setq user-init-file (expand-file-name "~/.emacs.el")))

(defvar user-init-dir (file-name-directory
                       (or (file-symlink-p user-init-file)
                           user-init-file))
  "Root directory of emacs.el, after following symlinks, etc.")

(setq custom-file (concat user-init-dir "custom.el"))

(add-to-list 'load-path user-init-dir)
(add-to-list 'load-path (concat user-init-dir "third-party") t) ; TODO: remove

(require 'rwd-load)

(defvar normal-startup
  (and (not noninteractive)
       (not (cdr command-line-args))))
(defvar normal-gui-startup
  (and window-system
       normal-startup)
  "t if this is a plain GUI emacs startup (eg not batch nor task oriented).")

(let* ((os-name     (symbol-name system-type))
       (host-list   (split-string (system-name) "\\."))
       (host-name   (car host-list))
       (domain-name (mapconcat 'identity (cdr host-list) ".")))
  (rwd-load (concat "os/" os-name)         t)  ;; os/darwin
  (rwd-load (concat "domain/" domain-name) t)  ;; domain/zenspider.com
  (rwd-load (concat "host/" host-name)     t)) ;; host/greed

(rwd-require 'rwd-autoloads)
(rwd-require 'rwd-packages)
(rwd-require 'rwd-autohooks)
(rwd-require 'rwd-load-modes)

(rwd-autoloads)
(rwd-autohooks)

(rwd-load custom-file)

(put 'narrow-to-region 'disabled nil)
