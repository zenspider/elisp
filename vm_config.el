;; -*-Lisp-*-

(require 'vm)

(add-to-list 'vm-mime-default-face-charsets "Windows-1251")
(add-to-list 'vm-mime-default-face-charsets "Windows-1252")
(add-to-list 'vm-mime-default-face-charsets "Windows-1257")
(add-to-list 'vm-mime-default-face-charsets "iso-8859-9")

(defun vm-my-spool-file (name)
  (list (concat "~/Mail/" name) 
	(concat "~/Mail/spool/" name)
	(concat "~/Mail/" name ".CRASH")
	)
  )

(setq 

 ;; Don't display HTML automatically
 vm-mime-internal-content-type-exceptions '("text/html")
 vm-auto-displayed-mime-content-type-exceptions '("text/html")

 ;; Automatically get mail every 10 seconds (INBOX only?)
 vm-use-toolbar nil
 vm-auto-get-new-mail 10
 vm-mail-check-interval 30

 vm-raise-frame-at-startup nil
 
 vm-included-text-prefix "> "

 vm-delete-after-saving t
 vm-delete-after-archiving t
 
 vm-delete-empty-folders nil
 vm-confirm-new-folders t
 
 ;; For use with filtering... Filter will write to X.spool for X mailbox
 vm-folder-directory "~/Mail/"
 vm-primary-inbox "~/Mail/INBOX"
 vm-spool-file-suffixes '(".spool")
 vm-crash-box-suffix ".crash"

 ;; Add "Re:" to messages, and ignore "Re" (w/ possible #) when grouping
vm-reply-subject-prefix "Re: "
vm-subject-ignored-prefix "^\\(re\\(\\[[0-9]+\\]\\)?: *\\)+"

;; vm-print-command-switches lpr-switches

 ;; Ignore case
 vm-auto-folder-case-fold-search t

 ;; Spool setup
 vm-spool-files (mapcar '(lambda (x) (vm-my-spool-file x))
			(list 
			 "INBOX"
			 "GrayBox"
			 "BlackBox"
			 "antlr"
			 "backup"
			 "bill"
			 "brazil"
			 "build"
			 "commit"
			 "commit-main"
			 "crontab"
			 "cvs"
			 "depend"
			 "downtown"
			 "frank"
			 "junit"
			 "obidos"
			 "other"
			 "perf"
			 "progress"
			 "qa"
			 "ryand"
			 "software"
			 "ssc"
			 "test"
			 "xmas"
			 "zss"
			 ))

 vm-virtual-folder-alist '(
			   ("search"
			    (("~/Mail")
			     (text "poker")))
			   ("old-files"
			    (("~/Mail")
			     (sent-before "Oct 1 2001")))
			   )


; vm-virtual-folder-alist
; '(
;   ;; start virtual folder definition
;   ("virtual-folder-name"
;    (("all-mail"
;     (header "foo")
;     (header "bar")
;     )
;    (("/path/to/folder3" "/path/to/folder4")
;     (and (header "baz") (header "woof"))
;     )
;    )
;   ;; end of virtual folder definition
;   )

 )

(eval-after-load "bbdb"
  '(progn
     (bbdb-initialize 'vm)
     (bbdb-insinuate-vm)))

(add-to-list 'vm-mime-type-converter-alist
             '("text/html" "text/plain" "lynx -force_html -dump -stdin"))

(vm-my-spool-file "blah")
