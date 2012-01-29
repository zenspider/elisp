;;;###autoload
(progn
  (require 'autoinsert)

  (setq auto-insert-directory "~/Bin/elisp/insert/")
  (setq auto-insert-query nil)

  (define-auto-insert "\.rb"  "template.rb")
  (define-auto-insert "\.scm" "template.scm"))
