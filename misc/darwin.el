(defvar running-osx (or (featurep 'mac-carbon) (eq 'ns window-system)))

(when running-osx
  ;; Fixes a tweaky thing on 10.8
  (ns-set-resource nil "ApplePressAndHoldEnabled" "NO")

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
