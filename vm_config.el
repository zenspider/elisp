;; -*-Lisp-*-

(add-to-list 'vm-mime-default-face-charsets "Windows-1251")
(add-to-list 'vm-mime-default-face-charsets "Windows-1252")
(add-to-list 'vm-mime-default-face-charsets "Windows-1257")

(setq
 
 ;; Automatically get mail every 10 seconds (INBOX only?)
 vm-use-toolbar nil
 vm-auto-get-new-mail 10
 vm-mail-check-interval 30
 
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

 ;; My list of folders to automatically save to
; vm-auto-folder-alist '(
;			("From:"
;			 ("ryand" . "ryand")
;			 ("bosik" . "kathy")
;			 ("pwilk" . "phil")
;			 ("alanh" . "alan")
;			 ("bill@" . "bill")
;			 ("snoc"  . "snoc")
;			 )
;			(
;			 "Subject:"
;			 ("checkin" . "commit")
;			 ("commit" . "commit")
;			 )
;			(
;			 "To:"
;			 ("software@" . "software")
;			 ("qa@" . "qa")
;			 ("downtown" . "downtown")
;			 )
;			(
;			 "CC:"
;			 ("software@" . "software")
;			 ("qa@" . "software")
;			 ("downtown" . "downtown")
;			 )
;			)
 vm-spool-files (list 
 		 (list "~/Mail/INBOX" "~/Mail/spool/INBOX" "~/Mail/INBOX.CRASH")
 		 (list "~/Mail/bill" "~/Mail/spool/bill" "~/Mail/bill.CRASH")
 		 (list "~/Mail/build" "~/Mail/spool/build" "~/Mail/build.CRASH")
 		 (list "~/Mail/commit" "~/Mail/spool/commit" "~/Mail/commit.CRASH")
 		 (list "~/Mail/downtown" "~/Mail/spool/downtown" "~/Mail/downtown.CRASH")
 		 (list "~/Mail/frank" "~/Mail/spool/frank" "~/Mail/frank.CRASH")
 		 (list "~/Mail/kathy" "~/Mail/spool/kathy" "~/Mail/kathy.CRASH")
 		 (list "~/Mail/ryand" "~/Mail/spool/ryand" "~/Mail/ryand.CRASH")
 		 (list "~/Mail/software" "~/Mail/spool/software" "~/Mail/software.CRASH")
 		 (list "~/Mail/qa" "~/Mail/spool/qa" "~/Mail/qa.CRASH")
 		 (list "~/Mail/ssc" "~/Mail/spool/ssc" "~/Mail/ssc.CRASH")
 		 (list "~/Mail/test" "~/Mail/spool/test" "~/Mail/test.CRASH")
 		 (list "~/Mail/zss" "~/Mail/spool/zss" "~/Mail/zss.CRASH")
		 )

 ;; vm-virtual-folder-alist '(("regexp-virtual-folder"
 ;;			    (("INBOX")
 ;;			     (text "regexp")))
 ;;			   ("all-mail"
 ;;			    (("~/Mail")
 ;;			     (text "another-regexp")))))
 )

; (bbdb-insinuate-vm)
