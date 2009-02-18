;;;###autoload
(defcustom rwd-project-dirs
  '("~/Bin/elisp"
    "~/Work/p4/zss/src/RubyInline/dev"
    "~/Work/p4/zss/src/ZenTest/dev"
    "~/Work/p4/zss/src/hoe/dev"
    "~/Work/p4/zss/src/minitest/dev"
    "~/Work/p4/zss/src/metal/dev"
    "~/Work/p4/zss/src/flog/dev/lib"
    "~/Work/p4/zss/src/flay/dev/lib"
    "~/Work/p4/zss/src/ParseTree/dev"
    "~/Work/p4/zss/src/sexp_processor/dev/lib"
    "~/Work/p4/zss/src/ruby_parser/dev/lib"
    "~/Work/p4/zss/src/ruby_parser/dev/test"
    "~/Work/p4/zss/src/ruby2ruby/dev")

  "A list of important directory paths for my projects."
  :group 'rwd
  :type '(repeat string))
