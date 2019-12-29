;; from http://www.emacswiki.org/emacs/CocoAspell

;; NOTE: you must modify /usr/local/etc/aspell.conf and set dict-dir
;;       to work properly!!!

(setq ispell-dictionary-alist
      (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
                       ("-B" "-d" "english" "--dict-dir"
                        "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
                       nil iso-8859-1)))
        `((nil ,@default)
          ("english" ,@default))))
