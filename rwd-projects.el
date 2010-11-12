
;; TODO: test this out and see if it is worth it
;;;###autoload
;; (require 'eproject)
;; 
;; ;;;###autoload
;; (define-project-type elisp (generic)
;;   (look-for "emacs.el")
;;   :relevant-files ("\\.el$"))
;; 
;; ;;;###autoload
;; (define-project-type ruby (generic)
;;   (look-for "Rakefile")
;;   :relevant-files ("\\.rb$"))
;; 
;; ;;;###autoload
;; (setq tags-add-tables nil)
;; 
;; ;;;###autoload
;; (def-hook ruby-project-file-visit
;;   (let ((path (concat (eproject-root) "/TAGS")))
;;     (unless (file-exists-p path)
;;       (shell-command (concat "cd " (eproject-root) "; retag") nil nil))
;;     ;; (ignore-errors
;;     (require 'etags)
;;     ;; (tags-reset-tags-tables)
;;     (visit-tags-table path t)))

;; (add-hook 'window-configuration-change-hook #'eproject-maybe-turn-on)

