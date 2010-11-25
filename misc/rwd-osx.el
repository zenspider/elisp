;;;###autoload
(setq running-osx (or (featurep 'mac-carbon) (eq 'ns window-system)))

;;;###autoload
(if (and running-osx (not (getenv "CDPATH")))
    ;; deal with OSX's wonky enivronment by forcing PATH to be correct.
    ;; argh this is stupid
    (let* ((path   (shell-command-to-string "/bin/bash -lc 'echo -n $PATH'"))
           (cdpath (shell-command-to-string "/bin/bash -lc 'echo -n $CDPATH'"))
           (path-list (split-string path ":" t)))
      (setenv "PATH" path)
      (setenv "CDPATH" cdpath)
      (dolist (p path-list) (add-to-list 'exec-path p t))))

