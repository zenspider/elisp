;; (ssh-directory-tracking-mode) -- causes an error on connection

(shell-dirtrack-mode t)

(setq ssh-directory-tracking-mode t)
(setq shell-dirtrackp t)
(setq comint-file-name-prefix (make-comint-file-name-prefix))
