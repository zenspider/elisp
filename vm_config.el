;; -*-Lisp-*-

(add-to-list 'vm-mime-default-face-charsets "Windows-1251")
(add-to-list 'vm-mime-default-face-charsets "Windows-1252")
(add-to-list 'vm-mime-default-face-charsets "Windows-1257")
(add-to-list 'vm-mime-default-face-charsets "iso-8859-9")

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
; vm-reply-subject-prefix "Re: "
; vm-subject-ignored-prefix "^\\(re\\(\\[[0-9]+\\]\\)?: *\\)+"

;; vm-print-command-switches lpr-switches

 ;; Ignore case
 vm-auto-folder-case-fold-search t

 ;; Spool setup
 vm-spool-files (list 
 		 (list "~/Mail/INBOX" "~/Mail/spool/INBOX" "~/Mail/INBOX.CRASH")
 		 (list "~/Mail/GrayBox" "~/Mail/spool/GrayBox" "~/Mail/GrayBox.CRASH")
 		 (list "~/Mail/BlackBox" "~/Mail/spool/BlackBox" "~/Mail/BlackBox.CRASH")
 		 (list "~/Mail/antlr" "~/Mail/spool/antlr" "~/Mail/antlr.CRASH")
 		 (list "~/Mail/bill" "~/Mail/spool/bill" "~/Mail/bill.CRASH")
 		 (list "~/Mail/build" "~/Mail/spool/build" "~/Mail/build.CRASH")
 		 (list "~/Mail/commit" "~/Mail/spool/commit" "~/Mail/commit.CRASH")
 		 (list "~/Mail/commit-main" "~/Mail/spool/commit-main" "~/Mail/commit-main.CRASH")
 		 (list "~/Mail/cvs" "~/Mail/spool/cvs" "~/Mail/cvs.CRASH")
 		 (list "~/Mail/downtown" "~/Mail/spool/downtown" "~/Mail/downtown.CRASH")
 		 (list "~/Mail/frank" "~/Mail/spool/frank" "~/Mail/frank.CRASH")
 		 (list "~/Mail/crontab" "~/Mail/spool/crontab" "~/Mail/crontab.CRASH")
 		 (list "~/Mail/kathy" "~/Mail/spool/kathy" "~/Mail/kathy.CRASH")
 		 (list "~/Mail/perf" "~/Mail/spool/perf" "~/Mail/perf.CRASH")
 		 (list "~/Mail/progress" "~/Mail/spool/progress" "~/Mail/progress.CRASH")
 		 (list "~/Mail/junit" "~/Mail/spool/junit" "~/Mail/junit.CRASH")
 		 (list "~/Mail/obidos" "~/Mail/spool/obidos" "~/Mail/obidos.CRASH")
 		 (list "~/Mail/other" "~/Mail/spool/other" "~/Mail/other.CRASH")
 		 (list "~/Mail/qa" "~/Mail/spool/qa" "~/Mail/qa.CRASH")
 		 (list "~/Mail/ryand" "~/Mail/spool/ryand" "~/Mail/ryand.CRASH")
 		 (list "~/Mail/software" "~/Mail/spool/software" "~/Mail/software.CRASH")
 		 (list "~/Mail/ssc" "~/Mail/spool/ssc" "~/Mail/ssc.CRASH")
 		 (list "~/Mail/test" "~/Mail/spool/test" "~/Mail/test.CRASH")
 		 (list "~/Mail/backup" "~/Mail/spool/backup" "~/Mail/backup.CRASH")
 		 (list "~/Mail/zss" "~/Mail/spool/zss" "~/Mail/zss.CRASH")
 		 (list "~/Mail/xmas" "~/Mail/spool/xmas" "~/Mail/xmas.CRASH")
		 )

 vm-virtual-folder-alist '(
			   ("search"
			    (("~/Mail")
			     (text "poker")))
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

; (bbdb-insinuate-vm)
