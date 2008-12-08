;;; filecache

(require 'filecache)

(defun my-file-cache ()
  (interactive)
  (file-cache-clear-cache)
  (file-cache-add-directory-list load-path)
  (message "Filling file cache")
  (file-cache-add-file "~/Work/svn/ruby/ruby_1_8/ruby.h")
  (file-cache-add-file "~/Work/svn/ruby/ruby_1_8/intern.h")
  (file-cache-add-directory-using-find "~/Bin/elisp")
  (file-cache-add-directory-using-find "~/Work/p4/zss/src/RubyInline/dev")
  (file-cache-add-directory-using-find "~/Work/p4/zss/src/ZenTest/dev")
  (file-cache-add-directory-using-find "~/Work/p4/zss/src/hoe/dev")
  (file-cache-add-directory-using-find "~/Work/p4/zss/src/miniunit/dev")
  (file-cache-add-directory-using-find "~/Work/p4/zss/src/flog/dev/lib")
  (file-cache-add-directory-using-find "~/Work/p4/zss/src/flay/dev/lib")
  (file-cache-add-directory-using-find "~/Work/p4/zss/src/ParseTree/dev")
  (file-cache-add-directory-using-find "~/Work/p4/zss/src/sexp_processor/dev/lib")
  (file-cache-add-directory-using-find "~/Work/p4/zss/src/ruby_parser/dev/lib")
  (file-cache-add-directory-using-find "~/Work/p4/zss/src/ruby_parser/dev/test")
  (file-cache-add-directory-using-find "~/Work/p4/zss/src/ruby2ruby/dev")
  (message "Done filling file cache"))

(defun my-file-cache-rubinius ()
  (interactive)
  (file-cache-clear-cache)
  (shell-command "find ~/Work/git/rubinius/{spec,shotgun,kernel,lib} -name \\*.rb -o -name \\*.txt" file-cache-buffer)
  (file-cache-add-from-file-cache-buffer)
  (kill-buffer file-cache-buffer))

(run-with-idle-timer 5 nil #'my-file-cache)

(mapcar (lambda (map) (define-key map [S-tab] 'file-cache-minibuffer-complete))
        (list minibuffer-local-completion-map
              minibuffer-local-map
              minibuffer-local-must-match-map))
