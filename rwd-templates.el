;;;###autoload
(progn
  (require 'autoinsert)

  (add-hook 'find-file-hooks 'auto-insert)

  (add-to-list 'auto-insert-alist
               '(("\\.scm" . "Scheme File")
                 "#!/bin/sh\n"
                 "#| -*- scheme -*-\n"
                 "exec csi -s $0 \"$@\"\n"
                 "|#\n\n")))
