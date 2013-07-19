(defun outline-cycle-fast ()
  "cycles through: expand one level -> show entry -> expand children -> collapse"
  (interactive)

  (unless (looking-at outline-regexp)
    (outline-next-heading))

  (cond
   ((eq last-command 'outline-cycle-fast-expand)
    (show-entry)
    (setq this-command 'outline-cycle-fast-entry))
   ((eq last-command 'outline-cycle-fast-entry)
    (show-subtree)
    (setq this-command 'outline-cycle-fast-children))
   ((eq last-command 'outline-cycle-fast-children)
    (hide-subtree))
   (t
    (hide-subtree)
    (show-children 1)
    (setq this-command 'outline-cycle-fast-expand))))

(defun outline-expand-1-level ()
  (interactive)

  (show-children 1)
  (hide-entry))
