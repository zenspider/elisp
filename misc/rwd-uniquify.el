;;;###autoload
(progn
  (require 'uniquify)
  (setq
   uniquify-strip-common-suffix t
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"))
