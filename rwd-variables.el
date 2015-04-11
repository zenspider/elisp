
;; ruby -s ./lib/seattlerb_projects.rb -l | pbcopy
;;;###autoload
(defcustom rwd-project-names
  ;; TODO: make this auto-populate somehow
  '("hoe" "hoe-seattlerb" "ZenTest" "minitest" "minitest_tu_shim" "autotest-rails" "RubyInline" "sexp_processor" "ParseTree" "ruby_parser" "ruby2ruby" "event_hook" "heckle" "flog" "flay" "vlad" "gauntlet" "ruby_to_c" "zenprofile" "wilson" "image_science" "png" "imap_processor" "gmail_contacts" "imap_to_rss" "UPnP-ConnectionManager" "UPnP-MediaServer" "rails_analyzer_tools" "production_log_analyzer" "graph" "ZenWeb" "ZenGraph" "smtp_tls" "autotest-screen" "rubygems-bug" "change_class" "un" "orca_card" "yoda" "SuperCaller" "ZenCallGraph" "seattlerb_dashboard")
  "A list of important project names."
  :group 'rwd
  :type '(repeat string))


;;;###autoload
(defun rwd-project-dirs ()
  (remove-if-not 'file-directory-p
       (append
        '("~/Bin/elisp")
        (mapcar (lambda (s) (concat "~/Work/p4/zss/src/" s "/dev"))
                rwd-project-names))))

;;;###autoload
(defun rwd-find-project-files ()
  (split-string
   (shell-command-to-string
    (concat "find "
            (list-join " " (rwd-project-dirs))
            " \\( -name tmp -prune \\)"
            " -o -name \\*.rb -o -name \\*.el"
            " -o -name \\*.y -o -name Rakefile"))))
