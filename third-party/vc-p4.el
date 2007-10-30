;;; vc-p4.el --- Integrate Perforce support into VC mode in Emacs 21

;; Copyright (C) 2002 Curl Corporation.

;; Author: Jonathan Kamens <jik@kamens.brookline.ma.us>
;; Maintainer: Jonathan Kamens <jik@kamens.brookline.ma.us>

;; $Id: //guest/jonathan_kamens/vc-p4/vc-p4.el#19 $
;; The path above is on the Perforce server public.perforce.com:1666.
;; You can get this file using a P4 client talking to that depot, or
;; from the URL
;; http://public.perforce.com/guest/jonathan_kamens/vc-p4/vc-p4.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file adds support for Perforce to VC mode in Emacs 21, 22 and any
;; other Emacs variant which uses the VC mode included in Emacs 21.
;;
;; To use this file, you also need p4-lowlevel.el somewhere in your
;; load path (or load it explicitly with "load" if it's not in your
;; load path).
;;
;; The easiest way to use this functionality is to put this file
;; somewhere in your load-path and put this in one of your init files:
;;
;;   (require 'vc-p4)
;;
;; Alternatively, as long as this file is in your load path, you can
;; customize the variable vc-handled-backends and add "P4" to it; this
;; will cause VC to load this file automatically when it is needed.
;;
;; You can't use this support and the full functionality of Rajesh
;; Vaidheeswarran's "p4.el" at the same time.  You can, however, use
;; much of p4.el's functionality by setting the customization variable
;; "p4-do-find-file" to nil.  This will prevent p4.el from attempting
;; to "take ownership" of a Perforce file when you load it, but it
;; will allow you to use all of the p4.el commands that don't apply to
;; the current buffer.

;;; Code:

(eval-when-compile
  (require 'vc-hooks)
  (require 'vc)
  (require 'ediff))
;; FIXME: setq ediff-quit-hook maybe should be add-hook...

(eval-and-compile
  (require 'p4-lowlevel))

(if (not (memq 'P4 vc-handled-backends))
    (setq vc-handled-backends (cons 'P4 vc-handled-backends)))
; This is useful during development to ensure that we can simply
; reeval this buffer to get any new functions that have been added.
(put 'P4 'vc-functions nil)

; We need to fix some functions that are broken in vc.el.

(if (fboundp 'vc-default-resolve-select-yours) t
  (defun vc-maybe-resolve-conflicts (file status &optional name-A name-B)
    (vc-resynch-buffer file t (not (buffer-modified-p)))
    (if (zerop status) (message "Merge successful")
      (if (fboundp 'smerge-mode) (smerge-mode 1))
      (if (y-or-n-p "Conflicts detected.  Resolve them now? ")
	  (if (and (fboundp 'smerge-ediff)
		   (not (fboundp (vc-make-backend-sym (vc-backend file)
						      'resolve-select-yours))))
	      (smerge-ediff)
	    (vc-resolve-conflicts name-A name-B))
	(message "File contains conflict markers"))))

  (defun vc-default-resolve-select-yours (backend)
    (goto-char (point-min))
    (let ((found nil))
      (while (re-search-forward (concat "^<<<<<<< "
					(regexp-quote file-name) "\n") nil t)
	(setq found t)
	(replace-match "")
	(if (not (re-search-forward "^=======\n" nil t))
	    (error "Malformed conflict marker"))
	(replace-match "")
	(let ((start (point)))
	  (if (not (re-search-forward "^>>>>>>> [0-9.]+\n" nil t))
	      (error "Malformed conflict marker"))
	  (delete-region start (point))))
      found))

  (defun vc-default-resolve-select-theirs (backend)
    (goto-char (point-min))
    (while (re-search-forward (concat "^<<<<<<< "
				      (regexp-quote file-name) "\n") nil t)
      (let ((start (match-beginning 0)))
	(if (not (re-search-forward "^=======\n" nil t))
	    (error "Malformed conflict marker"))
	(delete-region start (point))
	(if (not (re-search-forward "^>>>>>>> [0-9.]+\n" nil t))
	    (error "Malformed conflict marker"))
	(replace-match "")))
    t)

  (defun vc-default-resolve-select-original (backend)
    nil)

  (defun vc-resolve-conflicts (&optional name-A name-B)
    "Invoke ediff to resolve conflicts in the current buffer.
The conflicts must be marked with rcsmerge conflict markers."
    (interactive)
    (vc-ensure-vc-buffer)
    (let* ((found nil)
	   (file-name (file-name-nondirectory buffer-file-name))
	   (backend (vc-backend buffer-file-name))
	   (your-buffer   (generate-new-buffer
			   (concat "*" file-name
				   " " (or name-A "WORKFILE") "*")))
	   (other-buffer  (generate-new-buffer
			   (concat "*" file-name
				   " " (or name-B "CHECKED-IN") "*")))
	   (ancestor-buffer (generate-new-buffer
			     (concat "*" file-name
				     " " (or name-B "ORIGINAL") "*")))
	   (result-buffer (current-buffer)))
      (save-excursion
	(set-buffer your-buffer)
	(erase-buffer)
	(insert-buffer-substring result-buffer)
	(if (not (vc-call-backend backend 'resolve-select-yours))
	    (progn
	      (kill-buffer your-buffer)
	      (kill-buffer other-buffer)
	      (kill-buffer ancestor-buffer)
	      (error "No conflict markers found")))
      
	(set-buffer other-buffer)
	(erase-buffer)
	(insert-buffer-substring result-buffer)
	(vc-call-backend backend 'resolve-select-theirs)

	(set-buffer ancestor-buffer)
	(erase-buffer)
	(insert-buffer-substring result-buffer)
	(goto-char (point-min))
	(if (not (vc-call-backend backend 'resolve-select-original))
	    (progn
	      (kill-buffer ancestor-buffer)
	      (setq ancestor-buffer nil)))

	(let ((config (current-window-configuration))
	      (ediff-default-variant 'default-B))

	  ;; Fire up ediff.

	  (set-buffer
	   (if ancestor-buffer
	       (ediff-merge-buffers-with-ancestor your-buffer other-buffer
						  ancestor-buffer)
	     (ediff-merge-buffers your-buffer other-buffer)))

	  ;; Ediff is now set up, and we are in the control buffer.
        ;; Do a few further adjustments and take precautions for exit.

	  (make-local-variable 'vc-ediff-windows)
	  (setq vc-ediff-windows config)
	  (make-local-variable 'vc-ediff-result)
	  (setq vc-ediff-result result-buffer)
	  (make-local-variable 'ediff-quit-hook)
	  (setq ediff-quit-hook
		(lambda ()
		  (let ((buffer-A ediff-buffer-A)
			(buffer-B ediff-buffer-B)
			(buffer-C ediff-buffer-C)
			(result vc-ediff-result)
			(windows vc-ediff-windows))
		    (ediff-cleanup-mess)
		    (set-buffer result)
		    (erase-buffer)
		    (insert-buffer-substring buffer-C)
		    (kill-buffer buffer-A)
		    (kill-buffer buffer-B)
		    (kill-buffer buffer-C)
		    (set-window-configuration windows)
		    (message "Conflict resolution finished; you may save the buffer"))))
	  (message "Please resolve conflicts now; exit ediff when done")
	  nil)))))

(defcustom vc-p4-require-p4config nil
  "*If non-nil and the `P4CONFIG' environment variable is set, then
only perform p4 operations on a file when a P4CONFIG file can be found
in one of its parent directories.  This is useful if P4 operations are
expensive to start, e.g., if you are connect to the network over a
slow dialup connection and/or using a SSH tunnel for P4 access.  To
avoid delays when opening non-P4 files, simply set P4CONFIG as
described in the Perforce documentation, create an empty P4CONFIG file
at the root of your client workspace, and set `vc-p4-require-p4config'
to t."
  :type 'boolean
  :group 'vc)

(defcustom vc-p4-annotate-command nil
  "*Specifies the name of a command to call to annotate Perforce files.  
If nil, then `vc-p4-annotate-command-internal' will be used.
I recommend //guest/jonathan_kamens/p4pr.perl in the Perforce
repository public.perforce.com:1666.  Note that you need a version of
this script which accept `--after=date', if you want to be able to
specify a starting date when you run C-u C-x v g."
  :type 'string
  :group 'vc)


(defun vc-p4-create-repo ()
  (error "create-repo not supported yet for P4"))

(defun vc-p4-registered (file)
  "Return non-nil is FILE is handled by Perforce."
  (if (and vc-p4-require-p4config
	   (getenv "P4CONFIG")
	   (not (vc-p4-find-p4config (file-name-directory file))))
      nil
    (let* ((fstat (p4-lowlevel-fstat file nil t))
	   (action (cdr (or (assoc "action" fstat)
			    (assoc "headAction" fstat)))))
      (if (or (not fstat)
	      (string= action "delete"))
	  nil
	; This sets a bunch of VC properties
	(vc-p4-state file fstat)
	t))))

(defun vc-p4-state (file &optional fstat-list force)
  "Returns the current version control state of FILE in Perforce.  If
optional FSTAT-LIST is non-nil, use that list of attributes from
p4-lowlevel-fstat instead of calling it.  If optional FORCE is
non-nil, refetch all properties even if properties were previously
fetched."
  (if (and (not force) (vc-file-getprop file 'vc-p4-did-fstat))
      (vc-file-getprop file 'vc-state)
    (let* (
	   (alist (or fstat-list (p4-lowlevel-fstat file nil)))
	   (headRev (cdr (assoc "headRev" alist)))
	   (haveRev (cdr (assoc "haveRev" alist)))
	   (depotFile (cdr (assoc "depotFile" alist)))
	   (action  (cdr (assoc "action" alist)))
	   (state (if action
		      (let ((opened (p4-lowlevel-opened file)))
			(if (string-match " by \\([^@]+\\)@" opened)
			    (match-string 1 opened)
			  (if (equal headRev haveRev)
			      'edited
			    'needs-merge)))
		    (if (p4-lowlevel-diff-s file "e")
			'unlocked-changes
		      (if (equal headRev haveRev)
		      'up-to-date
		      'needs-patch))))
	   )
      (vc-file-setprop file 'vc-p4-did-fstat t)
      (vc-file-setprop file 'vc-p4-depot-file depotFile)
      (vc-file-setprop file 'vc-p4-action action)
      (vc-file-setprop file 'vc-backend 'P4)
      (vc-file-setprop file 'vc-checkout-model 'announce)
      (vc-file-setprop file 'vc-latest-version headRev)
      (vc-file-setprop file 'vc-name file)
      (vc-file-setprop file 'vc-state state)
      (vc-file-setprop file 'vc-workfile-version haveRev)
      state)))

; Here's something that would work faster, but I'm not going to
; actually try to use this unless I find that it's really too slow to
; just do all the work all the time.
;(defun vc-p4-state-heuristic (file)
;  "Estimates the current version control state of FILE in Perforce."
;  (if (and (file-exists-p file)
;	   (file-writable-p file))
;      'edited
;    'up-to-date))

(defun vc-p4-dir-state (dir)
  "Determines the current version control state of the files in DIR in
Perforce and sets the appropriate VC properties."
  (let ((lists (p4-lowlevel-fstat (format "%s/*" dir) nil t))
	this-list this-file this-action)
    (if (stringp (caar lists))
	(setq lists (list lists)))
    (while lists
      (setq this-list (car lists)
	    lists (cdr lists)
	    this-file (cdr (assoc "clientFile" this-list))
	    this-action (cdr (or (assoc "action" this-list)
				 (assoc "headAction" this-list))))
      (if (and this-file
	       (not (string= this-action "delete")))
	  (vc-p4-state this-file this-list)))))

(defun vc-p4-workfile-version (file)
  "Returns the Perforce version of FILE."
  (vc-p4-state file)
  (vc-file-getprop file 'vc-workfile-version))

(defun vc-p4-latest-on-branch-p (file)
  "Returns non-nil if the Perforce version of FILE is the head
revision."
  (vc-p4-state file)
  (string= (vc-file-getprop file 'vc-latest-version)
	   (vc-file-getprop file 'vc-workfile-version)))

(defun vc-p4-checkout-model (file)
  "Returns the checkout model for Perforce (`announce')."
  'announce)

(defun vc-p4-workfile-unchanged-p (file)
  "Returns non-nil if FILE is unchanged from the version in Perforce."
  (let ((state (vc-p4-state file)))
    (and (not (equal (vc-file-getprop file 'vc-p4-action) "add"))
	 (or (equal state 'up-to-date)
	     (equal state 'needs-patch)
	     (p4-lowlevel-diff-s file "r")))))

(defun vc-p4-mode-line-string (file)
  "Return string for placement into the modeline for FILE.
Compared to the default implementation, this function handles the
special case of a Perforce file that is added but not yet committed."
  (let ((state   (vc-state file))
	(rev     (vc-workfile-version file)))
    (if (or (not rev) (string= rev "0"))
	(setq rev "@@"))
    (cond ((or (eq state 'up-to-date)
	       (eq state 'needs-patch))
	   (concat "P4-" rev))
          ((stringp state)
	   (concat "P4:" state ":" rev))
          (t
           ;; Not just for the 'edited state, but also a fallback
           ;; for all other states.  Think about different symbols
           ;; for 'needs-patch and 'needs-merge.
           (concat "P4:" rev)))))

(defun vc-p4-register (files &optional rev comment)
  (if (and rev (not (string= rev "1")))
      (error "Can't specify revision when registering Perforce file."))
  (if (and comment (not (string= comment "")))
      (error "Can't specify comment when registering Perforce file."))
  ;; In emacs-23 vc-register has a list of files as a parameter,
  ;; before it used to be just a single file. We don't support that
  ;; interface yet, so just use the first file in the list.
  (let* ((file (if (listp files) (car files) files))
	 (fstat (p4-lowlevel-fstat file nil t))
	 (action (cdr (assoc "action" fstat))))
    (if (string= action "delete")
	(if (yes-or-no-p 
	     "File already opened for delete; revert and edit it? ")
	    (progn
	      (if (yes-or-no-p "Preserve current contents? ")
		  (let ((tempfile (format "%s.vc-register~" file)))
		    (rename-file file tempfile)
		    (p4-lowlevel-revert file)
		    (delete-file file)
		    (rename-file tempfile file))
		(p4-lowlevel-revert file))
	      (p4-lowlevel-edit file))
	  (error "File %s already opened for delete." file))
      (p4-lowlevel-add file))))

(defun vc-p4-init-version ()
  "Returns `1', the default initial version for Perforce files."
  "1")

(defun vc-p4-responsible-p (file)
  "Returns true if FILE refers to a file or directory that is
administered by Perforce."
  (if (and vc-p4-require-p4config
	   (getenv "P4CONFIG")
	   (not (vc-p4-find-p4config file)))
      nil
    (or (p4-lowlevel-fstat file nil t)
	(vc-p4-is-in-client (if (file-directory-p file)
				      (file-name-as-directory file)
				    file)))))

(defun vc-p4-find-version (file rev buffer)
  (p4-lowlevel-print file rev buffer t))

(defun vc-p4-checkin (files rev comment)
  "Check FILE into Perforce.  Error if REV is non-nil.  Check in with
comment COMMENT."
  (if rev
      (error "Can't specify revision for Perforce checkin."))
  ;; In emacs-23 vc-checkin has a list of files as a parameter, before
  ;; it used to be just a single file. We don't support that interface
  ;; yet, so just use the first file in the list.
  (let* ((file (if (listp files) (car files) files))
	 (default-directory (file-name-directory file))
	 (change-buffer (p4-lowlevel-change))
	 (indent-tabs-mode 1)
	 insertion-start change-number)
    (if (vc-p4-has-unresolved-conflicts-p file)
	(error "File %s has unresolved conflicts" file))
    (save-excursion
      (set-buffer change-buffer)
      (goto-char (point-min))
      (re-search-forward "^Description:\\s-*\n")
      (kill-line 1)
      (setq insertion-start (point))
      (insert comment "\n")
      (indent-rigidly insertion-start (point) 8)
      (re-search-forward "^Files:\\s-*\n")
      (delete-region (point) (point-max))
      (insert "\t" (vc-file-getprop file 'vc-p4-depot-file) "\n")
      (setq change-number (p4-lowlevel-change (current-buffer) t))
      (p4-lowlevel-change (current-buffer) change-number)
      (p4-lowlevel-submit (current-buffer))
      ; Update its properties
      (vc-p4-state file nil t)
      (vc-mode-line file))))

;;; FIXME: this should not have a DESTFILE argument
(defun vc-p4-checkout (file &optional editable rev destfile)
  (if (and editable destfile (not (string= file destfile)))
      (error "Can't lock a Perforce file in an alternate location."))
  (if (string= file destfile)
      (setq destfile nil))
  (let ((default-directory (file-name-directory file))
	buffer)
    ; Make sure we've got all the current state of the file
    (vc-p4-state file)
    (cond
     ((not rev)
      (setq rev (vc-file-getprop file 'vc-workfile-version)))
     ((string= rev "")
      (setq rev (vc-file-getprop file 'vc-latest-version))))
    (if destfile
	(progn (setq buffer (p4-lowlevel-print file rev 'buffer t))
	       (set-buffer buffer)
	       (write-file destfile))
      (if (not (string= rev (vc-file-getprop file 'vc-workfile-version)))
	  (p4-lowlevel-sync file rev))
      (if editable
	  (p4-lowlevel-edit file))))
  (vc-p4-state file nil t))

(defun vc-p4-revert (file contents-done)
  "Revert FILE in Perforce.  Ignores CONTENTS-DONE."
  (let ((action (vc-file-getprop file 'vc-p4-action)))
    (p4-lowlevel-revert file)
    (if (string= action "add")
	(vc-file-clearprops file)
      (vc-p4-state file nil t))))

(defun vc-p4-merge (file rev1 rev2)
  "Merge changes into Perforce FILE from REV1 to REV2."
  (p4-lowlevel-integrate file file rev1 rev2 t)
  (p4-lowlevel-resolve file)
  (vc-resynch-buffer file t t)
  (vc-p4-state file nil t)
  (if (vc-p4-has-unresolved-conflicts-p file)
      1
    0))

(defun vc-p4-merge-news (file)
  "Merge new changes from Perforce into FILE."
  (p4-lowlevel-sync file)
  (p4-lowlevel-resolve file)
  (vc-resynch-buffer file t t)
  (vc-p4-state file nil t)
  (if (vc-p4-has-unresolved-conflicts-p file)
      1
    0))

(defun vc-p4-resolve-select-yours ()
  (vc-p4-select-conflict-text (current-buffer) 3))

(defun vc-p4-resolve-select-theirs ()
  (vc-p4-select-conflict-text (current-buffer) 2))

(defun vc-p4-resolve-select-original ()
  (vc-p4-select-conflict-text (current-buffer) 1))

(defun vc-p4-steal-lock (file &optional version)
  "Steal Perforce lock on FILE."
  (if (and version (not (equal version (vc-workfile-version file))))
      (error "Can't specify version when stealing Perforce lock."))
  ; Must set default-directory because this is called in a mail send
  ; hook and thus not with the current buffer set to the file being
  ; reopened.
  (let ((default-directory (file-name-directory file)))
    (p4-lowlevel-reopen file)))

(defun vc-p4-print-log (files &optional buffer)
  "Print Perforce log for FILE into *vc* buffer."
  ;; `log-view-mode' needs to have the file name in order to function
  ;; correctly. "p4 logview" does not print it, so we insert it here by
  ;; hand.

  ;; `vc-do-command' creates the buffer, but we need it before running
  ;; the command.
  (vc-setup-buffer buffer)
  (let* ((inhibit-read-only t)
	 ;; In emacs-23 vc-print-log has a list of files as a
	 ;; parameter, before it used to be just a single file. We
	 ;; don't support that interface yet, so just use the first
	 ;; file in the list.
	 (file (if (listp files) (car files) files))
	 (default-directory (file-name-directory file)))
    (with-current-buffer
	buffer
      (p4-lowlevel-filelog file (current-buffer) t t)
      ;; Insert the file name at the beginning.
      (goto-char (point-min))
      (insert "File:        " (file-name-nondirectory file) "\n"))))

(defun vc-p4-show-log-entry (version)
  "Make sure Perforce log entry for VERSION is displayed in the
current buffer."
  (goto-char (point-min))
  (let (start end lines)
    (if (not (search-forward (format "\n#%s " version) nil t)) t
      (beginning-of-line)
      (setq start (point))
      (if (not (search-forward "\n#" nil t))
	  (setq end (point-max))
	(beginning-of-line)
	(setq end (point)))
      (setq lines (count-lines start end))
      (cond
       ;; if the global information and this log entry fit
       ;; into the window, display from the beginning
       ((< (count-lines (point-min) end) (window-height))
	(goto-char (point-min))
	(recenter 0)
	(goto-char start))
       ;; if the whole entry fits into the window,
       ;; display it centered
       ((< (1+ lines) (window-height))
	(goto-char start)
	(recenter (1- (- (/ (window-height) 2) (/ lines 2)))))
       ;; otherwise (the entry is too large for the window),
       ;; display from the start
       (t
	(goto-char start)
	(recenter 0))))))

(defun vc-p4-wash-log (file)
  "Remove all non-comment information from the Perforce log in the
current buffer."
  (goto-char (point-min))
  (delete-non-matching-lines "^\t"))

(defun vc-p4-update-changelog (&optional files)
  "Create ChangeLog entriers for FILES if it's non-nil, or for all
files under the default directory otherwise."
  (let ((odefault default-directory)
	(changelog (find-change-log))
	default-directory start-rev end-rev)
    (find-file-other-window changelog)
    (setq default-directory odefault)
    (goto-char (point-min))
    (if (looking-at
	 "^\\([0-9]\\{4\\}\\)[-/]\\([0-9]\\{2\\}\\)[-/]\\([0-9]\\{2\\}\\) ")
	(setq start-rev (format "@%s/%s/%s"
				(match-string 1)
				(match-string 2)
				(match-string 3))
	      end-rev "@now"))
    (if (not files)
	(setq files "..."))
    (message "Computing change log entries...")
    (insert (p4-lowlevel-info-lines
	     (p4-lowlevel-changes files nil start-rev end-rev
				  nil t nil "submitted")))
    (if (= (point) (point-min)) t
      (if (not (= (point) (point-max)))
	  (insert "\n"))
      (while (re-search-backward
	      (concat "^Change [0-9]+ on \\([0-9]+\\)/"
		      "\\([0-9]+\\)/\\([0-9]+\\) by \\(.+\\)")
	      nil t nil)
	(replace-match "\n\\1-\\2-\\3  \\4" t))
      (goto-char (point-min))
      (if (looking-at "\n")
	  (kill-line)))
    (message "Computing change log entries... done")))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)

(define-derived-mode vc-p4-log-view-mode log-view-mode "P4-Log-View"
  (require 'add-log) ;; we need the faces add-log
  (set (make-local-variable 'log-view-file-re) "^File:[ \t]+\\(.+\\)")
  (set (make-local-variable 'log-view-message-re)
       "^#\\([0-9]+\\) .*")
  (set (make-local-variable 'log-view-font-lock-keywords)
       (append `((,log-view-message-re . 'log-view-message-face)
		 (,log-view-file-re . 'log-view-file-face))
	'(("^user:[ \t]+\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)"
	   (1 'change-log-email))
	  ;; Handle the case:
	  ;; user: FirstName LastName <foo@bar>
	  ("^user:[ \t]+\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
	   (1 'change-log-name)
	   (2 'change-log-email))
	  ("^date: \\(.+\\)" (1 'change-log-date))
	  ("^summary:[ \t]+\\(.+\\)" (1 'log-view-message))))))

(defun vc-p4-diff (files &optional rev1 rev2 buff)
  "Do a Perforce diff."
  (let* ((buffer (or (bufferp buff) (get-buffer-create "*vc-diff*")))
	 (workfile-version (vc-file-getprop file 'vc-workfile-version))
	 (inhibit-read-only t)
	 ;; In emacs-23 vc-diff has a list of files as a parameter,
	 ;; before it used to be just a single file. We don't support
	 ;; that interface yet, so just use the first file in the list.
	(file (if (listp files) (car files) files)))
    (if (not rev1)
	(if (not rev2)
	    (if (string= (vc-file-getprop buffer-file-name 'vc-p4-action)
			 "add")
		; I can't figure out anything better to do here than
		; to use diff-switches.  It would be so much easier if
	        ; "p4 diff" and "p4 diff2" accepted real diff
	        ; arguments instead of arguments with "-d" in front of
	        ; them.
		(progn
		  (set-buffer buffer)
		  (erase-buffer)
		  (apply 'call-process
			 (append
			  (list diff-command
				nil
				buffer
				nil)
			  (if (listp diff-switches)
			      diff-switches
			    (list diff-switches))
			  (list "/dev/null"
				file))))
	      (p4-lowlevel-diff file nil buffer))
	  (p4-lowlevel-diff2 file file workfile-version rev2 buffer))
      (if rev2
	  (p4-lowlevel-diff2 file file rev1 rev2 buffer)
	(p4-lowlevel-diff file rev1 buffer)))))

(defun vc-p4-annotate-command (file buffer &optional version)
  "Annotate FILE into BUFFER file using `vc-p4-annotate-command'.
Annotate version VERSION if it's specified."
  (if vc-p4-annotate-command
      (let ((full-file (if version
			   (concat file 
				   (p4-lowlevel-canonicalize-revision version))
			 file))
	    (starting-date (if current-prefix-arg
			       (read-string "Starting date: (default none) ")))
	    log-buffer times args)
	(setq args (append (list  buffer nil vc-p4-annotate-command nil)
			   (if starting-date
			       (list "--after" starting-date))
			   (list full-file)))
	(apply 'vc-do-command args))
    (vc-p4-annotate-command-internal file buffer version)))

;;; Adapted from p4.el
(defun vc-p4-read-output (buffer)
  "Reads first line of BUFFER and returns it.
Read lines are deleted from buffer."
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (forward-line)
    (let ((line (buffer-substring (point-min) (point))))
      (if (string= line "")
	  nil
	(delete-region (point-min) (point))
	;; remove trailing newline
	(if (equal (substring line (1- (length line)) (length line)) "\n")
	    (substring line 0 (1- (length line)))
	  line)))))

;;; Adapted from p4.el
(defun vc-p4-annotate-command-internal (file buffer &optional version)
  "Execute \"hg annotate\" on FILE, inserting the contents in BUFFER.
Optional arg VERSION is a version to annotate from."
  ;; XXX maybe not needed, but just in case.
  (vc-setup-buffer buffer)
  ;;   (with-current-buffer buffer
  (let ((file-name file)
	(file-spec file)
	(blame-branch-regex
	 "^\\.\\.\\. \\.\\.\\. branch from \\(//[^#]*\\)#")
	(blame-change-regex   
	 (concat "^\\.\\.\\. #"     "\\([0-9]+\\)"  ;; revision
		 "\\s-+change\\s-+" "\\([0-9]+\\)"  ;; change
		 "\\s-+"            "\\([^ \t]+\\)" ;; type
		 "\\s-+on\\s-+"     "\\([^ \t]+\\)" ;; date	   
		 "\\s-+by\\s-+"     "\\([^ \t]+\\)" ;; author
		 "@"))
	head-name ;; file spec of the head revision for this blame assignment
	branch-p  ;; have we tracked into a branch?
	cur-file ;; file name of the current branch during blame assignment
	change ch-alist fullname head-rev headseen)

    ;; we asked for blame constrained by a change number
    (if (string-match "\\(.*\\)@\\([0-9]+\\)" file-spec)
	(progn
	  (setq file-name (match-string 1 file-spec))
	  (setq change (string-to-int (match-string 2 file-spec)))))

    ;; we asked for blame constrained by a revision
    (if (string-match "\\(.*\\)#\\([0-9]+\\)" file-spec)
	(progn
	  (setq file-name (match-string 1 file-spec))
	  (setq head-rev (string-to-int (match-string 2 file-spec)))))

    ;; make sure the filespec is unambiguous
    ;;(p4-exec-p4 buffer (list "files" file-name) t)
    (with-temp-buffer
      (vc-p4-command (current-buffer) nil nil "files" file-name)
      (save-excursion
	;; (set-buffer buffer)
	(if (> (count-lines (point-min) (point-max)) 1)
	    (error "File pattern maps to more than one file.")))
      )
    ;; get the file change history:
    ;;(p4-exec-p4 buffer (list "filelog" "-i" file-spec) t)
    (with-temp-buffer
      (vc-p4-command (current-buffer) 0 nil "filelog" "-i" file-spec)
      (setq fullname (vc-p4-read-output (current-buffer))
	    cur-file  fullname
	    head-name fullname)

      ;; parse the history:
      (save-excursion
	;; (set-buffer buffer)
	(goto-char (point-min))
	(while (< (point) (point-max))

	  ;; record the current file name (and the head file name,
	  ;; if we have not yet seen one):
	  (if (looking-at "^\\(//.*\\)$")
	      (setq cur-file (match-string 1)))

	  ;; a non-branch change:
	  (if (looking-at blame-change-regex)
	      (let ((rev (string-to-int (match-string 1)))
		    (ch (string-to-int (match-string 2)))
		    (op (match-string 3))
		    (date (match-string 4))
		    (author (match-string 5)))
		(cond
		 ;; after the change constraint, OR
		 ;; after the revision constraint _for this file_
		 ;;   [remember, branches complicate this]:
		 ((or (and change   (< change ch))
		      (and head-rev (< head-rev rev)
			   (string= head-name cur-file))) nil)
	       
		 ;; file has been deleted, can't assign blame:
		 ((string= op "delete") 
		  (if (not headseen) (goto-char (point-max))))

		 ;; OK, we actually want to look at this one:
		 (t
		  (setq ch-alist
			(cons
			 (cons ch (list rev date author cur-file)) ch-alist))
		  (if (not head-rev) (setq head-rev rev))
		  (setq headseen t)) ))

	    ;; not if we have entered a branch (this used to be used, isn't
	    ;; right now - maybe again later:
	    (if (and headseen (looking-at blame-branch-regex))
		(setq branch-p t)) )
	  (forward-line))))
    
    (if (< (length ch-alist) 1)
	(error "Head revision not available"))
  
    (let ((base-ch (int-to-string (caar ch-alist)))
	  (ch-buffer (get-buffer-create " *p4-ch-buf*"))
	  (tmp-alst (copy-alist ch-alist)))
      ;; (p4-exec-p4 ch-buffer (list "print" "-q" (concat cur-file "@" base-ch)) t)
      (vc-p4-command ch-buffer nil nil "print" "-q" (concat cur-file "@" base-ch))
      (save-excursion
	(set-buffer ch-buffer)
	(goto-char (point-min))
	(while (re-search-forward ".*\n" nil t)
	  (replace-match (concat base-ch "\n"))))
      (while (> (length tmp-alst) 1)
	(let ((ch-1 (car (car  tmp-alst)))
	      (ch-2 (car (cadr tmp-alst)))
	      (file1 (nth 3 (cdr (car  tmp-alst))))
	      (file2 (nth 3 (cdr (cadr tmp-alst))))
	      (blame-revision-regex
	       (concat "^\\([0-9]+\\),?"
		       "\\([0-9]*\\)"
		       "\\([acd]\\)"
		       "\\([0-9]+\\),?"
		       "\\([0-9]*\\)"))
	      ins-string)
	  (setq ins-string (format "%d\n" ch-2))
	  ;; (p4-exec-p4 buffer (list "diff2"
	  ;; 			   (format "%s@%d" file1 ch-1)
	  ;; 			   (format "%s@%d" file2 ch-2)) t)
	  (with-temp-buffer 
	    (vc-p4-command (current-buffer) nil nil
			   "diff2" (format "%s@%d" file1 ch-1) 
			   (format "%s@%d" file2 ch-2))
	    (save-excursion
	      ;;(set-buffer buffer)
	      (goto-char (point-max))
	      (while (re-search-backward blame-revision-regex nil t)
		(let ((la (string-to-int (match-string 1)))
		      (lb (string-to-int (match-string 2)))
		      (op (match-string 3))
		      (ra (string-to-int (match-string 4)))
		      (rb (string-to-int (match-string 5))))
		  (if (= lb 0)
		      (setq lb la))
		  (if (= rb 0)
		      (setq rb ra))
		  (cond ((string= op "a")
			 (setq la (1+ la)))
			((string= op "d")
			 (setq ra (1+ ra))))
		  (save-excursion
		    (set-buffer ch-buffer)
		    (goto-line la)
		    (let ((beg (point)))
		      (forward-line (1+ (- lb la)))
		      (delete-region beg (point)))
		    (while (<= ra rb)
		      (insert ins-string)
		      (setq ra (1+ ra))))))))
	  (setq tmp-alst (cdr tmp-alst))))
      ;; (p4-noinput-buffer-action "print" nil t
      ;; 			(list (format "%s#%d" fullname head-rev))
      ;; 			t)
      (vc-p4-command buffer nil nil
		     "print" (format "%s#%d" fullname head-rev))
      (let (line cnum (old-cnum 0) change-data
		 (blame-index-regex
		  (concat " *\\([0-9]+/[0-9]+/[0-9]+\\)" ;; date
			  "\\s-+\\([^ \t]*\\)"           ;; author
			  " *\\([0-9]+\\)"               ;; change
			  " *\\([0-9]+\\)"               ;; revision
			  " "))
		 xth-rev xth-date xth-auth xth-file)
	(save-excursion
	  (set-buffer buffer)
	  (goto-line 2)
	  (move-to-column 0)
	  (insert (format "%10s %7s %6s %4s\n" "Date" "Author" "Change"  "Rev"))
	  (while (setq line (vc-p4-read-output ch-buffer))
	    (setq cnum (string-to-int line))
	    (if (and nil (= cnum old-cnum))
		(insert (format "%29s " ""))

	      ;; extract the change data from our alist: remember,
	      ;; `eq' works for integers so we can use assq here:
	      (setq change-data (cdr (assq cnum ch-alist))
		    xth-rev     (nth 0 change-data)
		    xth-date    (nth 1 change-data)
		    xth-auth    (nth 2 change-data)
		    xth-file    (nth 3 change-data))
	      
	      (insert
	       (format "%10s %7s %6d %4d " xth-date xth-auth cnum xth-rev))
	      (move-to-column 0)
	      (if (looking-at blame-index-regex)
		  (let ((nth-cnum (match-string 3))
			(nth-revn (match-string 4))
			(nth-user (match-string 2)))
		    ;; truncate the user name:
		    (let ((start (+ (match-beginning 2) 7))
			  (end (match-end 2)))
		      (if (> end start)
			  (delete-region start end))))))
	    (setq old-cnum cnum)
	    (forward-line))))

      (kill-buffer ch-buffer))))

(defconst vc-p4-annotate-re 
  (concat "^\\([[:digit:]/]+\\)[[:space:]]*[[:digit:]]+[[:space:]]+"
	  "[^[:space:]]+[[:space:]]+\\([[:digit:]]+\\)"
	  "[[:space:]]+\\([[:digit:]]+\\) "))

(defun vc-p4-annotate-time ()
  "Returns the time of the next Perforce annotation at or after point,
as a floating point fractional number of days.
Moves the point to the end of the annotation."
  (when (looking-at vc-p4-annotate-re)
    (goto-char (match-end 0))
    (let ((timestr (match-string-no-properties 1)))
      (string-match "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)" timestr)
      (vc-annotate-convert-time
       (encode-time 0 0 0
		  (string-to-number (match-string 3 timestr))
		  (string-to-number (match-string 2 timestr))
		  (string-to-number (match-string 1 timestr)))))))

(defun vc-p4-annotate-extract-revision-at-line ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at vc-p4-annotate-re) (match-string-no-properties 3))))

(defun vc-p4-previous-version (file rev)
  "Return the Perforce revision of FILE prior to REV."
  (number-to-string (- (string-to-number rev) 1)))

(defun vc-p4-find-p4config (&optional dirname)
  "See if there is a $P4CONFIG file in DIRNAME or any of its parents.
If DIRNAME is not specified, uses `default-directory'."
  (let ((this-directory (expand-file-name (or dirname default-directory)))
	(p4config (getenv "P4CONFIG"))
	child)
    (if (not p4config)
	nil
      (catch 'found
	(while (not (equal this-directory child))
	  (if (file-exists-p (concat this-directory p4config))
	      (throw 'found (concat this-directory p4config)))
	  (setq child this-directory)
	  (setq this-directory (file-name-directory
				(directory-file-name this-directory))))))))

(defun vc-p4-is-in-client (file)
  "Return true if FILE is inside the p4 client hierarchy."
  (let* ((default-directory (file-name-directory file))
         (info (p4-lowlevel-info))
         (root-pair (assoc "Client root" info))
         (root (and root-pair (cdr root-pair)))
         (quoted-root (and root (concat "^" (regexp-quote root))))
         (cwd-pair (assoc "Current directory" info))
         (cwd (and cwd-pair (cdr cwd-pair))))
    (if (or (not quoted-root) (not (string-match quoted-root cwd)))
        nil
      (setq cwd (replace-match "" nil nil cwd))
      (if (or (string= cwd "") (string-match "^/" cwd))
          t
        nil))))

(defun vc-p4-has-unresolved-conflicts-p (file)
  "Search through FILE's buffer for unresolved P4 conflicts.
If FILE is a string, then the buffer visiting that file is searched;
no search occurs if no buffer is visiting the file.  If FILE is a
buffer, then that buffer is searched.

  Returns nil if there are no conflicts.  If there are conflicts,
returns a list of buffer positions containing the start and end of the
first conflict block in the file and then the start and end of each
subblock within it."
  (let ((buffer (if (bufferp file) file (get-file-buffer file)))
        block-start block-end block1-start block1-end block2-start block2-end
        block3-start block3-end)
    (if (not buffer)
        nil
      (save-excursion
        (save-restriction
          (set-buffer buffer)
          (widen)
          (goto-char (point-min))
          (if (not (re-search-forward "^>>>>\\( .*\\|\\)\n" nil t))
              nil
            (setq block-start (match-beginning 0)
                  block1-start (match-end 0))
            (if (not (re-search-forward "^<<<<\\( .*\\|\\)\n" nil t))
                nil
              ; Could actually be block 3, but but we'll figure that out later.
              (setq block2-end (match-beginning 0)
                    block-end (match-end 0))
              (goto-char block1-start)
              (if (not (re-search-forward "^====\\( .*\\|\\)\n" block-end t))
                  nil
                (setq block1-end (match-beginning 0)
                      block2-start (match-end 0))
                (if (not (re-search-forward "^====\\( .*\\|\\)\n" block-end t))
                    (list block-start block-end 
                          block1-start block1-end 
                          block2-start block2-end)
                  (setq block3-end block2-end
                        block2-end (match-beginning 0)
                        block3-start (match-end 0))
                  (list block-start block-end
                        block1-start block1-end
                        block2-start block2-end
                        block3-start block3-end))))))))))

(defun vc-p4-select-conflict-text (buffer which)
  "Search for P4 conflict markers in BUFFER and select the WHICH text of each.
WHICH should be either 1, 2, or 3 to indicate the first, second or
third subblock in each conflict block."
  (let (block-list block-start block-end sub-start sub-end sublist subcount
        replacement)
    (save-excursion
      (set-buffer buffer)
      (while (setq block-list (vc-p4-has-unresolved-conflicts-p buffer))
        (setq block-start (car block-list)
              block-end (cadr block-list)
              subcount which)
        (while (and block-list (> subcount 0))
          (setq block-list (cddr block-list)
                subcount (1- subcount)))
        (setq replacement (if block-list
                              (buffer-substring (car block-list) 
						(cadr block-list))
                            ""))
        (delete-region block-start block-end)
        (goto-char block-start)
        (insert replacement)))
    (if block-start t nil)))

(defun vc-p4-command (buffer okstatus file &rest flags)
  "A wrapper around `vc-do-command' for use in vc-p4.el.
The difference to vc-do-command is that this function always invokes `p4'."
  (apply 'vc-do-command buffer okstatus "p4" file flags))

(provide 'vc-p4)
