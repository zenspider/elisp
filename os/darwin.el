(defvar running-osx (memq window-system '(mac ns)))

(when running-osx
  (setenv "LANG" "en_US.UTF-8") ; comes from terminal.app, not bash itself

  (when (string= "/" default-directory) ; GUI starts up in root
    (cd "~"))

  ;; BSDs have an INPUT_MAX of 1024 and emacs shell gets bit by that
  ;; so, we drop pty for our shells on osx by setting
  ;; process-connection-type to nil temporarily
  (defun zenspider/osx/shell (orig-shell &rest args)
    (let ((process-connection-type nil))
      (apply orig-shell args)))
  (advice-add 'shell :around 'zenspider/osx/shell)

  ;; Helloooo overkill.
  ;; (set-language-environment    'utf-8)
  ;; (set-default-coding-systems  'utf-8)
  ;; (set-locale-environment      "en_US.UTF-8")
  ;; (prefer-coding-system        'utf-8)
  (set-charset-priority 'unicode)
  (setq locale-coding-system   'utf-8)   ; pretty
  (set-terminal-coding-system  'utf-8)   ; pretty
  (set-keyboard-coding-system  'utf-8)   ; pretty
  (set-selection-coding-system 'utf-8)   ; please
  (prefer-coding-system        'utf-8)   ; with sugar on top
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  (setq ns-function-modifier 'hyper) ; set Mac's Fn key to type Hyper

  ;; resets cmd-~ on emacs 23 and up
  (unless (version< emacs-version "23")
    (global-set-key (kbd "M-`") 'other-frame)))

(unless (getenv "TERM_PROGRAM")
  (setenv "EDITOR" "emacsclient")
  (setenv "VISUAL" (getenv "EDITOR"))

  ;; deal with OSX's wonky enivronment by forcing PATH to be correct.
  ;; argh this is stupid
  (let ((fix-env
         (lambda (v) (setenv v (shell-command-to-string
                                (format "/bin/bash -ilc 'echo -n $%s'" v))))))

    (funcall fix-env "PATH")
    (funcall fix-env "CDPATH")

    (dolist (p (split-string (getenv "PATH") ":" t))
      (add-to-list 'exec-path p t))))
