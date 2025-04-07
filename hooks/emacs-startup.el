(rwd-load-modes)

(when (and normal-gui-startup (not (getenv "INSIDE_EMACS")))
  (server-start))

(when-idle rwd-idle-time
  (setq file-name-handler-alist rwd-file-name-handler-alist) ; see early-init.el

  (setq gc-cons-percentage 0.2
        gc-cons-threshold 1600000))

;; original
;; "#![ \t]?\\([^ \t\n]*/bin/env[ \t]\\)?\\([^ \t\n]+\\)"

;; original, as rx
;; (rx (seq "#!"
;;          (opt (any "\t "))
;;          (opt (group
;;                (zero-or-more (not (any "\t\n ")))
;;                "/bin/env"
;;                (any "\t ")))
;;          (group (one-or-more (not (any "\t\n "))))))

;; mine:
(setq auto-mode-interpreter-regexp
      (rx (seq "#!"
               (opt (any "\t "))
               (opt (group
                     (zero-or-more (not (any "\t \n")))
                     "/bin/env"
                     (any "\t ")
                     (opt "-S" (+ (any "\t "))) ; ryand: added optional -S
                     ))
               (group (one-or-more (not (any "\t \n")))))))

;; mine, back to regexp:
;; "#![\t ]?\\([^\t\n ]*/bin/env[\t ]\\(?:-S[\t ]+\\)?\\)?\\([^\t\n ]+\\)"
