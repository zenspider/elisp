(require 'bbdb)
(autoload 'bbdb         "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-name    "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-company "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-net     "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-notes   "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-insinuate-vm       "bbdb-vm"    "Hook BBDB into VM")
(autoload 'bbdb-insinuate-gnus     "bbdb-gnus"  "Hook BBDB into GNUS")
(autoload 'bbdb-insinuate-sendmail "bbdb"       "Hook BBDB into sendmail")
;; NOTE: VM hook is in ~/.vm

(setq mail-setup-hook 'bbdb-insinuate-sendmail) ;; Hook into sendmail.

;; if machine.gemstone.com is in bbdb and later sees gemstone.com, 
;; remove previous (redundant) version.
(bbdb-add-hook 'bbdb-change-hook 'my-bbdb-delete-redundant-nets)

(defun my-bbdb-delete-redundant-nets (record)
  "Deletes redundant network addresses.
For use as a value of `bbdb-change-hook'.  See `bbdb-net-redundant-p'."
  (let* ((nets (bbdb-record-net record))
	 (rest nets)
	 net new redundant)
    (while rest
      (setq net (car rest))
      (if (bbdb-net-redundant-p net nets)
	  (setq redundant (cons net redundant))
	(setq new (cons net new)))
      (setq rest (cdr rest)))
    (cond (redundant
	   (message "Deleting redundant nets %s..."
		    (mapconcat 'identity (nreverse redundant) ", "))
	   (setq new (nreverse new))
	   (bbdb-record-set-net record new)
	   t))))
