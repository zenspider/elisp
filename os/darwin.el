(setq running-osx (or (and (featurep 'mac) 'mac)
                      (and (featurep 'cocoa) 'cocoa)))

(when running-osx
  (when (and (< emacs-major-version 27)
             (string= "/" default-directory)) ; GUI starts up in root
    (cd "~"))

  (prefer-coding-system 'utf-8)

  ;; resets cmd-~ on emacs 23 and up
  (unless (< emacs-major-version 23)
    (global-set-key (kbd "M-`") 'other-frame)))

(unless (getenv "TERM_PROGRAM")
  (setenv "EDITOR" "emacsclient")
  (setenv "VISUAL" "emacsclient")
  ;; deal with OSX's wonky enivronment by forcing PATH to be correct.
  ;; argh this is stupid
  (when-idle rwd-idle-time
   (message "Setting PATH and CDPATH in osx")
   (let* ((paths  (split-string
                   (shell-command-to-string
                    (format "/bin/bash -ilc 'echo -n $%s $%s' 2>/dev/null" "PATH" "CDPATH"))))
          (path   (car paths))
          (cdpath (cadr paths)))
     (setenv "PATH" path)
     (setenv "CDPATH" cdpath)
     (dolist (p (split-string path ":" t))
       (add-to-list 'exec-path p t)))))

;; (getenv "PATH")
;; (getenv "CDPATH")
;; (getenv "EDITOR")
;; (getenv "VISUAL")
;; (getenv "LANG")
