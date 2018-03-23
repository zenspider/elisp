(define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)
(define-key dired-mode-map "k" 'dired-kill-subdir)
(define-key dired-mode-map "=" 'dired-ediff-marked-files)

;; Inspired by https://www.emacswiki.org/emacs/DiredOmitMode
(setq rwd-dired-dotfiles nil)

;; TODO: make this persistent somehow... probably modifying dired-listing-switches

(defun rwd-toggle-dired-dotfiles ()
  (interactive)
  (setq rwd-dired-dotfiles (not rwd-dired-dotfiles))
  (dired-sort-other
   (apply 'string (remove-if (lambda (c) (and rwd-dired-dotfiles (= ?a c)))
                             (string-to-list dired-listing-switches)))))

(define-key dired-mode-map "." 'rwd-toggle-dired-dotfiles)
