;; setup gnus and mail
(autoload 'gnus-post-news "gnuspost" "Post a news." t)
(load "setup-bbdb")
(setq 
 mail-archive-file-name "~/Mail/sent-mail"
 gnus-nntp-server nil
 gnus-select-method '(nntp "news")
 gnus-local-domain "amazon.com"
;; browse-url-netscape-command "/usr/local/10bin/netscape-404"
 )

