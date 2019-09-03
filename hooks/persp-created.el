(require 'f)

(let ((curr-name (persp-name (persp-curr))))
  (let ((proj-dir (concat (f-canonical (f-join "~/Links" curr-name)) "/")))
    (with-current-buffer (persp-scratch-buffer)
      (when (f-directory? proj-dir)
        (setq default-directory proj-dir))
      (rwd-shell))))
