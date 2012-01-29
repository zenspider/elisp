(defconst *web-top* "~/Sites")

(defun web (path)
  (concat *web-top* path))

(defconst *dired-dirs*
  (list (cons "mrc-theme" (web "/blog/mrc/wp-content/themes/mrc"))
        (cons "mrc-pages" (web "/pages/mrc"))
        (cons "webtest"   (web "/test"))))

;; TODO: read these dynamically
;; A       -> SVN/apprema
;; ALGO    -> SRC/Algometer/dev
;; BFTS    -> SRC/bfts/dev
;; BS      -> CVS/brightside
;; C       -> SRC/cocor/dev
;; CAST    -> SVN/cast/trunk/cast
;; CVS     -> ../Work/cvs
;; DNS     -> USR/root/conf/namedb
;; EL      -> ../Bin/elisp
;; EL2     -> ../Sites/emacs/elisp
;; ELTP    -> EL/third-party
;; F       -> SRC/flog/dev
;; FI      -> CVS/FreeImage
;; FL      -> SRC/flay/dev
;; G       -> SRC/gauntlet/dev
;; GEMDIR  -> /Library/Ruby/Gems/1.8
;; GEMS    -> SVN/rubygems
;; GIT     -> ../Work/git
;; H       -> SRC/heckle/dev
;; HACK    -> SRC/ZenHacks/dev
;; HOE     -> SRC/hoe/dev
;; HW      -> ../Sites/ruby/class/homework
;; IS      -> SRC/image_science/dev
;; LS      -> SVN/limespot/trunk
;; M       -> ../Work/mirrors
;; MP      -> /opt/local/var/macports/sources/rsync.macports.org/release/ports
;; MR      -> SRC/metaruby/dev
;; MT      -> SRC/minitest/dev
;; OB      -> CVS/zenspider/obfuscator
;; PC      -> SRC/poopcode/dev
;; PNG     -> SRC/png/dev
;; PT      -> SRC/ParseTree/dev
;; R2C     -> SRC/ruby_to_c/dev
;; R2R     -> SRC/ruby2ruby/dev
;; RF      -> SVN/codeforpeople/rubyforge/trunk
;; RH      -> SRC/rubyholic/dev
;; RI      -> SRC/RubyInline/dev
;; RP      -> SRC/ruby_parser/dev
;; RU      -> GIT/rubinius_cpp
;; RUBY    -> SVN/ruby/ruby_1_8
;; RUBY19  -> SVN/ruby/ruby
;; RUBYLIB -> /usr/local/lib/ruby/1.8
;; SEA     -> SRC/seattlerb/dev
;; SP      -> SRC/sexp_processor/dev
;; SRC     -> ../Work/p4/zss/src
;; SRC2    -> ../Work/p4/zss-old1/src
;; SS      -> SRC/stuporslow/dev
;; SVN     -> ../Work/svn
;; USR     -> ../Work/p4/zss/usr
;; V       -> SRC/vlad/dev
;; W       -> SRC/wilson/dev
;; WEB     -> SRC/ZenWeb/dev
;; WWW     -> ../Work/p4/zss/www
;; ZL      -> SRC/ZenLibrary/dev
;; ZSS     -> WWW/zenspider.com
;; ZT      -> SRC/ZenTest/dev

(defconst *dired-aliases*
  (mapcar (lambda (e) (car e)) *dired-dirs*))

(defun dired-open-alias (&optional alias)
  (interactive)
  (unless alias
    (setq alias
          (ido-completing-read "Alias: "
                               *dired-aliases*
                               nil t)))
  (if (and (stringp alias) (> (length alias) 0 ))
      (let ((pair (assoc alias *dired-dirs*)))
        (if pair
            (dired (cdr pair))
          (error "Invalid alias %s" alias)))
    (error "Invalid alias %s" alias)))
