(eval-when-compile
  (require 'ruby nil t))

;;;###autoload
(defun ruby-run-buffer-clean ()
  "Send the entire buffer to the inferior Ruby process.
Then switch to the process buffer."
  (interactive)
  (require 'inf-ruby)
  (save-excursion
    (let ((ruby-buffer-process (get-buffer-process ruby-buffer)))
      (if ruby-buffer-process
          (delete-process ruby-buffer-process)))
    (if (and ruby-buffer (get-buffer ruby-buffer))
        (kill-buffer ruby-buffer))
    (run-ruby ruby-program-name)
    (make-local-variable 'inferior-ruby-first-prompt-pattern)
    (make-local-variable 'inferior-ruby-prompt-pattern)
    (setq inferior-ruby-first-prompt-pattern ">>")
    (setq inferior-ruby-prompt-pattern       "\\?>"))
  (ruby-send-region-and-go (point-min) (point-max)))

;;;###autoload
(defun ruby-rails-run ()
  "Fire up script/server"
  (interactive)
  (let ((buffer (shell "*rails*")))
    (set (make-local-variable 'comint-output-filter-functions)
         '(comint-truncate-buffer comint-postoutput-scroll-to-bottom))
    (set (make-local-variable 'comint-buffer-maximum-size) 5000)
    (set (make-local-variable 'comint-scroll-show-maximum-output) t)
    (set (make-local-variable 'comint-scroll-to-bottom-on-output) t)

    (comint-send-string buffer (concat "script/server" "\n"))))

;;;###autoload
(defun ruby-pp-region (p m)
  (interactive "r")
  (shell-command-on-region p m
                           "ruby -rpp -e 'pp eval($stdin.read)'"
                           (current-buffer) t))

;;;###autoload
(set-register ?P "require \"pry\"; binding.pry\n")

;;;###autoload
(set-register ?I "require \"irb\"; binding.irb\n")

;;;###autoload
(set-register ?D "require \"debug\"; binding.break\n")

;;;###autoload
(set-register ?T "require 'minitest/autorun'

class TestXXX < Minitest::Test
  def test_xxx
    flunk
  end
end")

(defun rwd-ws (s)
  (concat "\\( *\\)" (replace-regexp-in-string "[LR]HS" "\\\\(.+\\\\)" s)))

;;;###autoload
(defun rwd-ruby-spec-to-test ()
  (interactive)
  (multi-replace-regexp
   (rwd-ws "LHS.must_be RHS")               "\\1assert_operator \\2, \\3"
   (rwd-ws "LHS.must_be_empty")             "\\1assert_empty \\2"
   (rwd-ws "LHS.must_be_instance_of(RHS)")  "\\1assert_instance_of \\3, \\2"
   (rwd-ws "LHS.must_be_kind_of *RHS")      "\\1assert_kind_of \\3, \\2"
   (rwd-ws "LHS.must_be_nil")               "\\1assert_nil \\2"
   (rwd-ws "LHS.must_be_truthy")            "\\1assert \\2"
   (rwd-ws "LHS.must_equal *RHS")           "\\1assert_equal \\3, \\2"
   (rwd-ws "LHS.must_include RHS")          "\\1assert_includes \\2, \\3"
   (rwd-ws "LHS.must_match(RHS)")           "\\1assert_match(\\2, \\3)"

   (rwd-ws "LHS.wont_be RHS")               "\\1refute_operator \\2, \\3"
   (rwd-ws "LHS.wont_be_empty")             "\\1refute_empty \\2"
   (rwd-ws "LHS.wont_be_instance_of(RHS)")  "\\1refute_instance_of \\3, \\2"
   (rwd-ws "LHS.wont_be_kind_of *RHS")      "\\1refute_kind_of \\3, \\2"
   (rwd-ws "LHS.wont_be_nil")               "\\1refute_nil \\2"
   (rwd-ws "LHS.wont_be_truthy")            "\\1refute \\2"
   (rwd-ws "LHS.wont_equal *RHS")           "\\1refute_equal \\3, \\2"
   (rwd-ws "LHS.wont_include RHS")          "\\1refute_includes \\2, \\3"
   (rwd-ws "LHS.wont_match(RHS)")           "\\1refute_match(\\2, \\3)"

   (rwd-ws "lambda *{LHS}.must_raise(RHS)") "\\1assert_raises \\3 do\n\\2\nend"

   ;; more complex example because we need to remove spaces for underscores
   "it \"\\(.+\\)\" do"
   (lambda ()
     (concat "def test_"
             (replace-regexp-in-string " " "_" (match-string 1))))
  ))

;;;###autoload
(defun rwd-ruby-rspec-to-minitest-spec ()
  (interactive)
  (multi-replace-regexp
   (rwd-ws "LHS.should == RHS")             "\\1\\2.must_equal \\3"
   (rwd-ws "LHS.should be RHS")             "\\1\\2.must_be_same_as \\3"
   (rwd-ws "LHS.should be_nil")             "\\1\\2.must_be_nil"
   (rwd-ws "LHS.should_not be_nil")         "\\1\\2.wont_be_nil"
   (rwd-ws "LHS.should_not == RHS")         "\\1\\2.wont_equal \\3"
   (rwd-ws "LHS.should != RHS")             "\\1\\2.wont_equal \\3"
  ))

;;;###autoload
(defun rwd-ruby-bacon-to-minitest-spec ()
  (interactive)
  (multi-replace-regexp
   (rwd-ws "LHS.should.equal RHS")           "\\1\\2.must_equal \\3"
   (rwd-ws "LHS.should == RHS")              "\\1\\2.must_equal \\3"
   (rwd-ws "LHS.should.equal(RHS)")          "\\1\\2.must_equal \\3"
   (rwd-ws "LHS.should.eql RHS")             "\\1\\2.must_equal \\3"
   (rwd-ws "LHS.should.eql(RHS)")            "\\1\\2.must_equal \\3"
   (rwd-ws "LHS.should.not.equal RHS")       "\\1\\2.wont_equal \\3"

   (rwd-ws "LHS.should.be.instance_of RHS")  "\\1\\2.must_be_instance_of \\3"
   (rwd-ws "LHS.should.be.instance_of(RHS)") "\\1\\2.must_be_instance_of \\3"
   (rwd-ws "LHS.should.be.kind_of RHS")      "\\1\\2.must_be_kind_of \\3"
   (rwd-ws "LHS.should.be.kind_of(RHS)")     "\\1\\2.must_be_kind_of \\3"

   (rwd-ws "LHS.should.empty")               "\\1\\2.must_be_empty"

   (rwd-ws "LHS.should.be.nil")              "\\1\\2.must_be_nil"
   (rwd-ws "LHS.should.be.RHS")              "\\1\\2.must_be :\\3?"
   (rwd-ws "LHS.should.not.be.RHS")          "\\1\\2.wont_be :\\3?"

   (rwd-ws "LHS.should.be.true")             "\\1\\2.must_equal true"
   (rwd-ws "LHS.should.be.false")            "\\1\\2.must_equal false"

   (rwd-ws "LHS.should.include\\?? RHS")     "\\1\\2.must_include \\3"
   (rwd-ws "LHS.should.include\\??(RHS)")    "\\1\\2.must_include \\3"
   (rwd-ws "LHS.should.not.include RHS")     "\\1\\2.wont_include \\3"
   (rwd-ws "LHS.should.not.include(RHS)")    "\\1\\2.wont_include \\3"

   (rwd-ws "LHS.should.match RHS")           "\\1\\2.must_match(\\3)"
   (rwd-ws "LHS.should.match(RHS)")          "\\1\\2.must_match(\\3)"
   (rwd-ws "LHS.should =~ RHS")              "\\1\\2.must_match(\\3)"
   (rwd-ws "LHS.should.not =~ RHS")          "\\1\\2.wont_match(\\3)"
   (rwd-ws "LHS.should.not.match RHS")       "\\1\\2.wont_match(\\3)"
   (rwd-ws "LHS.should.not.match(RHS)")      "\\1\\2.wont_match(\\3)"

   (rwd-ws "LHS.should.raise(RHS)")          "\\1\\2.must_raise \\3"
   (rwd-ws "LHS.should.raise RHS")           "\\1\\2.must_raise \\3"

   (rwd-ws "LHS.should.throw(RHS)")          "\\1\\2.must_throw \\3"
   (rwd-ws "LHS.should.throw RHS")           "\\1\\2.must_throw \\3"

   (rwd-ws "LHS.should.respond_to(RHS)")     "\\1\\2.must_respond_to \\3"
   (rwd-ws "LHS.should.respond_to RHS")      "\\1\\2.must_respond_to \\3"
   (rwd-ws "LHS.should.not.respond_to(RHS)") "\\1\\2.wont_respond_to \\3"
   (rwd-ws "LHS.should.not.respond_to RHS")  "\\1\\2.wont_respond_to \\3"

  ))

;;;###autoload
(defun rwd-ruby-dejsonify ()
  (interactive)

  (shell-command-on-region (region-beginning) (region-end) "ruby -rpp -rubygems -e 'require \"json\"; pp JSON.parse(eval($stdin.read))'" (quote (4)) (quote (4)) nil t))

;;;###autoload
(fset 'rwd-ruby-sort-paragraphs
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([down 67108896 up 134217734 1 134217787 24 24 134217787 134217765 13 33 134217788 19 99 108 97 115 115 1 down 67108896 up 134217734 1 134217848 115 111 114 116 45 112 97 114 97 9 13 134217848 100 101 108 101 9 116 114 9 13 down 134217730] 0 "%d")) arg)))
