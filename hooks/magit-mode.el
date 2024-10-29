(transient-append-suffix
  'magit-diff
  "p"                                   ; after diff-paths
  '("m" "main.." rwd/magit-diff-file-master))

(transient-append-suffix
  'magit-log
  "h"                                   ; after diff head
  '("m" "main.." rwd/magit-log-main))

(transient-append-suffix
  'magit-bisect
  "s"                                   ; after start script
  '("a" "all" rwd/magit-bisect-all))

(transient-append-suffix
  'magit-show-refs
  "y"                                   ; after normal show refs
  '("Y" "Show refs, sorted by date" rwd/magit-show-refs-head-sorted))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
(define-key magit-mode-map        (kbd "M-w") nil)
(define-key magit-mode-map        (kbd "@") #'endless/visit-pull-request-url)

;; TODO: "hub pull-request --no-edit -o"
;; TODO: "git absorb"

(endless/add-PR-fetch)
;; (turn-on-magit-gh-pulls) ; broken on current versions of magit

(defadvice magit-visit-item (around always-other-window compile activate)
  (ad-set-arg 0 t)
  ad-do-it)

(add-to-list 'magit-section-initial-visibility-alist '(tags . hide))
(add-to-list 'magit-section-initial-visibility-alist '(remotes . hide))

(remove-hook 'magit-refs-sections-hook   'magit-insert-tags)
(remove-hook 'magit-status-headers-hook  'magit-insert-tags-header)
(remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
(remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
(remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
(remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
(remove-hook 'magit-status-sections-hook 'magit-insert-stashes)

(add-to-list 'magit-status-sections-hook 'magit-insert-recent-commits t)

(magit-delta-mode +1)

;; (eval . (magit-disable-section-inserter 'magit-insert-status-headers))
;; (eval . (magit-disable-section-inserter 'magit-insert-staged-changes))
;; (eval . (magit-disable-section-inserter 'forge-insert-pullreqs))
;; (eval . (magit-disable-section-inserter 'magit-insert-stashes))
;; (eval . (magit-disable-section-inserter 'magit-insert-unpulled-from-upstream))
;; (eval . (magit-disable-section-inserter 'magit-insert-unpushed-to-upstream-or-recent))
