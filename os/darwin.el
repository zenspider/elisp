(defvar running-osx (or (featurep 'cocoa) (eq 'ns window-system)))

(when running-osx
  (setenv "LANG" "en_US.UTF-8") ; comes from terminal.app, not bash itself

  ;; Helloooo overkill.
  (set-language-environment    'utf-8)
  (set-default-coding-systems  'utf-8)
  (set-locale-environment      "en_US.UTF-8")
  (prefer-coding-system        'utf-8)

  (setq ns-function-modifier 'hyper) ; set Mac's Fn key to type Hyper

  ;; resets cmd-~ on emacs 23 and up
  (unless (version< emacs-version "23")
    (global-set-key (kbd "M-`") 'other-frame)))

(unless (getenv "TERM_PROGRAM")
  ;; deal with OSX's wonky enivronment by forcing PATH to be correct.
  ;; argh this is stupid
  (let ((fix-env
         (lambda (v) (setenv v (shell-command-to-string
                                (concat "/bin/bash -ilc 'echo -n $" v "'"))))))

    (funcall fix-env "PATH")
    (funcall fix-env "CDPATH")

    (dolist (p (split-string (getenv "PATH") ":" t))
      (add-to-list 'exec-path p t))))
