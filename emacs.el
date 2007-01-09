; Stupid compatibility checking

(setq running-xemacs (featurep 'xemacs))
(setq running-emacs (not running-xemacs))

(if running-xemacs (add-to-list 'Info-directory-list "/usr/share/info"))

(if running-emacs 
    (eval-when-compile
      (when (< emacs-major-version 20) (require 'cl))))

(if running-emacs (require 'vc-hooks))
(if running-emacs (transient-mark-mode 1))
(if running-emacs
    (add-hook 'comint-output-filter-functions
	      'comint-watch-for-password-prompt))

;; (if running-emacs
;;   (progn
;;     (message "Have I ever mentioned that GNU emacs sucks?")
;;     (defalias 'mapc 'mapcar)
;;     (load-library "cl")
;; ;    (setq apropos-do-all t)
;;     (transient-mark-mode 1)
;;     (add-hook 'comint-output-filter-functions
;; 	      'comint-watch-for-password-prompt)))

;; find ~/Bin/elisp/third-party ~/Bin/elisp/third-party/cedet-1.0pre3 ~/Bin/elisp/third-party/cedet-1.0pre3/semantic -maxdepth 1 -type d | sort -u

(add-to-list 'load-path "/Users/ryan/Bin/elisp")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/cedet-1.0pre3")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/cedet-1.0pre3/cogre")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/cedet-1.0pre3/common")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/cedet-1.0pre3/contrib")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/cedet-1.0pre3/ede")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/cedet-1.0pre3/eieio")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/cedet-1.0pre3/semantic")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/cedet-1.0pre3/semantic/bovine")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/cedet-1.0pre3/semantic/doc")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/cedet-1.0pre3/semantic/tests")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/cedet-1.0pre3/semantic/wisent")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/cedet-1.0pre3/speedbar")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/ecb-2.32")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/mmm-mode-0.4.8")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/slime")
(add-to-list 'load-path "/Users/ryan/Bin/elisp/third-party/tramp")

; (progn (cd "~/Bin/elisp") (normal-top-level-add-subdirs-to-load-path))

;; (mapc '(lambda (path)
;; 	 (pushnew (expand-file-name path) load-path :test 'string=))
;;       (append
;;        (remove-if-not (lambda (o)
;; 			(and (file-directory-p o)
;; 			     (not (string-match "\\.$" o))))
;; 		      (append (directory-files "~/Bin/elisp/third-party" t)
;; 			      (directory-files "~/Bin/elisp/third-party/cedet-1.0pre3" t)
;; 			      (directory-files "~/Bin/elisp/third-party/cedet-1.0pre3/semantic" t)))
;;        (list 
;; 	"~/Bin/elisp/third-party/"
;; 	"~/Bin/elisp/")))

;;; Load My Stuff
(if t
    (progn
      (load "setup-aliases")
      (load "setup-keys")
      (load "setup-mail-and-news")
      (load "setup-misc")
      (load "setup-modes")))

;; (if nil
;;     (require 'icicles)
;;   (progn
;;     (require 'icicles-face)
;;     (require 'icicles-opt)
;;     (require 'icicles-var)

;;     (require 'icicles-fn)
;;     (require 'icicles-mac)
;;     (require 'icicles-mcmd)
;;     (require 'icicles-cmd)
;;     (require 'icicles-mode)))

;; (icicle-mode 1)

(autoload 'toggle-buffer "toggle")

(put 'erase-buffer 'disabled nil) ;; nukes stupid warning
(setq explicit-shell-file-name "/bin/bash")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (custom-set-variables
;;  '(column-number-mode t))
;; ;; (custom-set-variables
;; ;;  '(cua-mode nil nil (cua-base)))
;; (custom-set-variables
;;  '(dired-recursive-deletes (quote top)))
;; (custom-set-variables
;;  '(ecb-layout-window-sizes (quote (("left9" (0.325 . 0.975))))))
;; (custom-set-variables
;;  '(ecb-options-version "2.32"))
;; (custom-set-variables
;;  '(global-font-lock-mode t nil (font-core)))
;; (custom-set-variables
;;  '(icicle-reminder-prompt-flag 0))
;; (custom-set-variables
;;  '(indent-tabs-mode nil))
;; (custom-set-variables
;;  '(indicate-empty-lines t))
;; (custom-set-variables
;;  '(inhibit-splash-screen t))
;; (custom-set-variables
;;  '(mouse-wheel-mode t nil (mwheel)))
;; (custom-set-variables
;;  '(save-place t nil (saveplace)))
;; (custom-set-variables
;;  '(show-paren-mode t))
;; (custom-set-variables
;;  '(tool-bar-mode nil nil (tool-bar)))
;; (custom-set-variables
;;  '(tooltip-mode nil))
;; (custom-set-variables
;;  '(transient-mark-mode t))
;; (custom-set-variables
;;  '(vc-path (quote ("/opt/local/bin" "/usr/local/bin"))))
;; (custom-set-variables
;;  '(vc-svn-program-name "/opt/local/bin/svn"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
; '(cua-mode nil nil (cua-base))
 '(dired-recursive-deletes (quote top))
 '(ecb-layout-window-sizes (quote (("left9" (0.325 . 0.975)))))
 '(ecb-options-version "2.32")
 '(global-font-lock-mode t nil (font-core))
 '(icicle-reminder-prompt-flag 0)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-splash-screen t)
 '(mouse-wheel-mode t nil (mwheel))
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-mode nil)
 '(transient-mark-mode t)
 '(vc-path (quote ("/opt/local/bin" "/usr/local/bin")))
 '(vc-svn-program-name "/opt/local/bin/svn"))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "slate blue")))))

