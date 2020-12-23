;; -*- lexical-binding: t; -*-

(unless user-init-file                  ; if running w/: -q --debug-init
  (setq user-init-file (expand-file-name "~/.emacs.d/init.el")))

(defvar user-init-dir (file-name-directory
                       (or (file-symlink-p user-init-file)
                           user-init-file))
  "Root directory of emacs.el, after following symlinks, etc.")

(when (< emacs-major-version 27)
  (load (concat user-init-dir "early-init.el")))

(setq custom-file (concat user-init-dir "custom.el"))

(add-to-list 'load-path user-init-dir)
(add-to-list 'load-path (concat user-init-dir "third-party") t) ; TODO: remove

(require 'rwd-load)

(let* ((os-name     (symbol-name system-type))
       (host-list   (split-string (system-name) "\\."))
       (host-name   (car host-list))
       (domain-name (mapconcat 'identity (cdr host-list) ".")))
  (rwd-load (concat "os/" os-name)         t)  ;; os/darwin
  (rwd-load (concat "domain/" domain-name) t)  ;; domain/zenspider.com
  (rwd-load (concat "host/" host-name)     t)) ;; host/greed

(rwd-require 'rwd-autoloads)
(unless (rwd-packages-up-to-date)
  (delete-file package-quickstart-file) ; force refresh?
  (rwd-require 'rwd-packages))
(rwd-require 'rwd-autohooks)
(rwd-require 'rwd-load-modes)

(rwd-autoloads)
(rwd-autohooks)

(rwd-load custom-file)
