(paredit-mode 1)
(racket-unicode-input-method-enable)

(coterm-mode -1)
;; HACK: coterm-mode -1 doesn't fully disconnect
(remove-function (process-filter (get-buffer-process (current-buffer)))
                 #'coterm--t-emulate-terminal)
