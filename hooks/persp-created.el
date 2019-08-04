(require 'f)

(let ((curr-name (persp-name (persp-curr))))
  (unless (equal curr-name persp-initial-frame-name)
    (let ((proj-dir (f-canonical (f-join "~/Links" curr-name))))
      (with-current-buffer (persp-scratch-buffer)
        (when (f-directory? proj-dir)
          (setq default-directory proj-dir))
        (rwd-shell)))))
