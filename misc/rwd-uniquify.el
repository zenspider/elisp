;;;###autoload
(progn
  (require 'uniquify)

  (setq
   uniquify-strip-common-suffix t
   uniquify-separator           "::"
   uniquify-buffer-name-style   'post-forward
   uniquify-after-kill-buffer-p t      ; rename after killing uniquified
   uniquify-ignore-buffers-re   "^\\*" ; don't  muck  with    special buffers
   ))
