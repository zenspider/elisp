;; (company-mode)
(dante-mode)
(flycheck-mode)
(haskell-doc-mode)
(haskell-indentation-mode)
(hindent-mode)
(interactive-haskell-mode)
(subword-mode)

;; (haskell-decl-scan-mode)

;; (set (make-local-variable 'company-backends)
;;      (append '((company-capf company-dabbrev-code)) company-backends))

;; (define-key haskell-mode-map (kbd "C-c r") 'hindent-reformat-decl-or-fill)
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile)
(define-key haskell-mode-map (kbd "C-c h")   'haskell-hoogle)
(define-key haskell-mode-map (kbd "C-c C-e") 'flycheck-list-errors)
(setq haskell-hoogle-command "hoogle")

;;; HACKS to make it work with ghc 8.2.1:

(require 'haskell-load)
(require 'haskell-debug)

;; (haskell-interactive-process)

(defun haskell-get-module-names ()
  "Get the list of modules currently set."
  (let ((string (haskell-process-queue-sync-request
                 (haskell-process)
                 ":show modules")))
    (if (string= string "")
        (list)
      (mapcar (lambda (pl) (plist-get asc :module))
       (mapcar #'haskell-debug-parse-module
               (split-string string "\n" string))))))

;; (with-current-buffer "week4test.hs"
;;   (haskell-get-module-names))
;;
;; (with-current-buffer "week4test.hs"
;;   (mapcar (lambda (asc) (plist-get asc :module)) (haskell-get-module-names)))

;; via https://github.com/haskell/haskell-mode/issues/1553 (with tweaks)
(defun haskell-process-load-complete (session process buffer reload module-buffer &optional cont)
  (message "FUUUUCK YOU")
  "Handle the complete loading response. BUFFER is the string of
text being sent over the process pipe. MODULE-BUFFER is the
actual Emacs buffer of the module being loaded."
  (when (get-buffer (format "*%s:splices*" (haskell-session-name session)))
    (with-current-buffer (haskell-interactive-mode-splices-buffer session)
      (erase-buffer)))
  (let* ((ok (cond
              ((haskell-process-consume
                process
                "Ok, modules loaded: \\(.+\\)\\.$")
               t)
              ((haskell-process-consume
                process
                "Ok, \\([[:digit:]]+\\) modules? loaded.$")
               t)
              ((haskell-process-consume
                process
                "Failed, modules loaded: \\(.+\\)\\.$")
               nil)
              (t
               (error (message "Unexpected response from haskell process in '%s'" (haskell-process-response process))))))
         (modules (haskell-process-extract-modules buffer))
         (cursor (haskell-process-response-cursor process))
         (warning-count 0))
    (message "modules = %s" modules)
    ;; (message "real modules = %s" (mapcar 'cadr (haskell-debug-get-modules)))
    (haskell-process-set-response-cursor process 0)
    (haskell-check-remove-overlays module-buffer)
    (while
        (haskell-process-errors-warnings module-buffer session process buffer)
      (setq warning-count (1+ warning-count)))
    (haskell-process-set-response-cursor process cursor)
    (if (and (not reload)
             haskell-process-reload-with-fbytecode)
        (haskell-process-reload-with-fbytecode process module-buffer)
      (haskell-process-import-modules process (car modules)))
    (if ok
        (haskell-mode-message-line (if reload "Reloaded OK." "OK."))
      (haskell-interactive-mode-compile-error session "Compilation failed."))
    (when cont
      (condition-case-unless-debug e
          (funcall cont ok)
        (error (message "%S" e))
        (quit nil)))))
