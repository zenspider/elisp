;;; auto-insert-tkld.el --- automatic insertion of text into new files.

;; Copyright (C) 1985, 1986, 1987, 1994 Free Software Foundation, Inc.

;; Author: Kevin Davidson <tkld@cogsci.ed.ac.uk>
;;	Charlie Martin <crm@cs.duke.edu>
;; Maintainer: Kevin Davidson <tkld@cogsci.ed.ac.uk>
;; Created: 1 Jul 1988
;; Version: $Revision: #1 $
;; Keywords: auto-insert new setup create default

;; Altered by tkld 1992/1994.

;; This file was part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; LCD Archive Entry:
;; auto-insert-tkld|Kevin Davidson|tkld@cogsci.ed.ac.uk|
;; An automatic file template package with keyword expansion for Emacs.|
;; $Date: 2000/12/20 $|$Revision: #1 $|~/packages/auto-insert.tar.Z|

;;;  Commentary:
;;
;;  The following defines an association list for files to be
;;  automatically inserted when a new file is created, and a function
;;  which automatically inserts these files; the idea is to insert
;;  default files much as the mode is automatically set using
;;  auto-mode-alist.
;;
;;  The auto-insert-alist consists of dotted pairs of
;;  ( REGEXP . TYPE ) where REGEXP is a regular expression, and
;;  TYPE is the type of file which is to be inserted into
;;  all new files matching the regular expression with which it is
;;  paired.
;;
;;  The auto-insert-type-alist consists of dotted pairs of
;;  ( TYPE . FILENAME ) where TYPE is a file type obtained from
;;  auto-insert-alist, or by querying the user and filename is the
;;  name of a file to be found in one of the directories listed in
;;  auto-insert-path.
;;
;;  To use: 
;;     load auto-insert-tkld.el, or (require 'auto-insert-tkld)
;;     setq auto-insert-path to a list of appropriate values
;;     e.g. (setq auto-insert-path
;;             (cons "/usr/local/lib/some-project-insert/" auto-insert-path))
;;     set user-mail-address to an appropriate value if wrong.
;;     append any extra patterns/filenames to auto-insert-alist 
;;     or auto-insert-type-alist as required.
;;     Prolog programmers may wish to do this:
;;     (setq auto-insert-alist (cons '("\.pl'" . "Prolog") auto-insert-alist))
;;     to override the default assumption that *.pl files are Perl.
;;     (global-set-key "\C-ci" 'insert-auto-insert-type)
;;

;; Interaction with other packages:
;;
;; If you use AUC TeX, you'll want to include this advice in your .emacs
;; to turn off auto-insert when creating auto .el files.
;;
;; Ensure AUC TeX auto/*.el files don't have default contents.
;;(defadvice TeX-auto-store (around no-auto-insert activate)
;;  "Ensure auto-insert is deactivated"
;;  (let ((auto-insert-automatically nil))
;;    ad-do-it))

;;  Original Author:  Charlie Martin
;;           Department of Computer Science and
;;           National Biomedical Simulation Resource
;;           Box 3709
;;           Duke University Medical Center
;;           Durham, NC 27710
;;	      (crm@cs.duke.edu,mcnc!duke!crm) 
;;

;;; Change log:
;;  Date: Fri Jul  1 16:15:31 EDT 1988
;; Much hacked about by Kevin Davidson <tkld@cogsci.ed.ac.uk>
;;                                     <Kevin.Davidson@ed.ac.uk>
;; $Log: auto-insert-tkld.el,v $
;; Revision 1.1  2000/12/21 02:18:06  ryand
;; Added.
;;
;; Revision 1.1.1.1  1998/11/26 08:41:23  ryand
;; Added to repository.
;;
;; Revision 1.1.1.1  1998/09/14 22:04:49  ryand
;; Imported sources
;;
;; Revision 1.23  1994/12/02  17:42:59  tkld
;; Commented out AUC TeX advice.
;; Changed regexps for %[ %] to be less greedy to avoid problems caused
;; by more than one pattern per line. Thanks Jim :)
;; <jimg@internet.sbi.com>
;;
;; Revision 1.22  1994/10/13  16:08:29  tkld
;; Slight mod to prevent failure when include file does not exist.
;;
;; Revision 1.21  1994/09/14  17:06:54  tkld
;; Update to use user-mail-address and file-name-as-directory
;;
;; Revision 1.20  1994/08/09  12:50:07  tkld
;; Fixed ) typo
;;
;;Revision 1.19  1994/07/28  15:28:04  tkld
;;Fixed up comment leaders.
;;
;; Revision 1.18  1994/07/28  15:25:24  tkld
;; Conform more closely to FSF source code structuring guidelines.
;; Only offer to insert if we found a file (from svm@kozmix.hacktic.nl).
;;
;; Revision 1.17  1994/05/17  13:37:33  tkld
;; Final tidying up. Stop tinkering with it now!
;;
;; Revision 1.16  1994/05/13  16:40:25  tkld
;; Oops. Last version was wrong :)
;;
;; Revision 1.15  1994/05/13  15:33:57  tkld
;; Added man page to alist.
;; Made login name, full name and system name lisp variables.
;;
;; Revision 1.14  1994/05/12  16:46:17  tkld
;; Document %1-%9 in comment.
;;
;; Revision 1.13  1994/05/12  16:39:52  tkld
;; Tidied up LCD entry. Added text formatting commands (filling etc.).
;; Allowed recall of prompted-for strings.
;; Generally much more rounded package :)
;;
;; Revision 1.12  1994/05/11  14:17:50  tkld
;; Removed path trailing / requirement.
;;
;; Revision 1.11  1994/05/10  16:42:17  tkld
;; Changed month/mail around.
;;
;; Revision 1.10  1994/05/09  16:44:34  tkld
;; Made more modular in preparation for removing huge (cond...).
;; Use gnus-local-{organization,domain} if set.
;; Added comments and prompted strings.
;;
;; Revision 1.9  1994/05/06  09:24:26  tkld
;; Fixed up LCD entry for newer guidelines. Separate LCD-entry file auto-generated
;; by Makefile.
;;
;; Revision 1.8  1994/04/27  16:09:14  tkld
;; Added LCD Archive entry
;;
;; Revision 1.7  1994/04/26  16:22:00  tkld
;; Added more types and wrote some TeXinfo documentation, along with a Makefile.
;;
;;
;; 14th April 1994
;; Added indirection of filename->type, type->insert file.
;; Recommended entry point is now auto-insert-type.
;;
;; 8th September 1992
;; Whole buffer is marked. If auto-insertion is not wanted, ^W will remove
;; it all if point has not been set by %@.
;; insert-auto-insert file can be called interactively.
;; Replaces %-escapes (listed in documentation string for 
;; insert-auto-insert-file below).
;;
;; Things to do:
;; Turn that cond into an alist.

;;; Code:

(defconst auto-insert-version (substring "$Revision: #1 $" 11 -2)
  "$Id: //depot/main/user/ryand/Bin/elisp/third-party/auto-insert-tkld.el#1 $

Report bugs to: Kevin Davidson <tkld@cogsci.ed.ac.uk>")

;; This is now in Emacs 19
(defvar user-mail-address
  (concat "<" (user-login-name)
	  "@" (if (boundp 'gnus-local-domain) gnus-local-domain (system-name))
	  ">" )
  "*Full mailing address of this user.")

(defvar auto-insert-organisation
  (or (if (boundp 'gnus-local-organization)
	  gnus-local-organization
	nil)
      (getenv "ORGANISATION")
      (getenv "ORGANIZATION")
      "")
  "*User's organisation for %o expansion.
Defaults to gnus-local-organization, environment variable $ORGANI[SZ]ATION,
or \"\" if none are set.")

(defvar auto-insert-system-name (system-name)
  "*Name of system.")
(defvar auto-insert-login-name (user-login-name)
  "*Login name of user.")
(defvar auto-insert-full-name (user-full-name)
  "*User's full name.")

(defvar auto-insert-alist '(("\\.tex$" . "LaTeX")
			    ("\\.texinfo$" . "TeXinfo")
			    ("\\.c$" . "C")
			    ("\\.cc$" . "C++")
			    ("\\.h[+p]*$" . "C Include")
			    ("\\.el$" . "Emacs Lisp")
			    ("[]>:/]\\..*emacs" . "Emacs Lisp")
			    ("[Mm]akefile" . "Makefile")
			    ("\\.bib$" . "LaTeX")
			    ("\\.sh$" . "Sh")
			    ("\\.csh$" . "Csh")
			    ("\\.[1-8]$" . "Manual Page")
			    ("\\.man$" . "Manual Page")
			    ("\\.pl$" . "Perl"))
  "Alist specifying text to insert by default into a new file.
Elements look like (REGEXP . TYPE); if the new file's name
matches REGEXP, then the auto insert file of type TYPE is inserted into
the buffer. Only the first matching element is effective.
See the variable auto-insert-type-alist for the list of types.")

(defvar auto-insert-type-alist '(("LaTeX" . "latex-insert.tex")
				 ("TeXinfo" . "texinfo-insert.texinfo")
				 ("C" . "c-insert.c")
				 ("C Include" . "h-insert.h")
				 ("C++" . "c++-insert.cc")
				 ("Emacs Lisp" . "elisp-insert.el")
				 ("Makefile" . "makefile.inc")
				 ("Perl" . "perl-insert.pl")
				 ("Prolog" . "prolog-insert.pl")
				 ("Tcsh" . "tcsh-insert.csh")
				 ("Csh" . "csh-insert.csh")
				 ("Sh" . "sh-insert.sh")
				 ("Manual Page" . "nroff-insert.man")
				 ("Test" . "test-insert"))
  "Alist specifying filename to insert using insert-auto-insert-file.
Elements look like (TYPE . FILENAME).")

(defvar auto-insert-automatically t
  "*Select whether to auto-insert text or not (or to ask).
Can have one of the following values:

nil      - do not insert text automatically.
t        - always insert text automatically.
ask      - ask whether to insert text or not.")

;;; Establish a default value for auto-insert-path
(defvar auto-insert-path 
  '("~/insert/" "~/lib/insert/" "/usr/local/share/lib/insert/")
  "*List of directories from which auto-inserted files are taken.")

(defvar auto-insert-search-current-dir t
  "*Search current directory of buffer for insert files first. 
Searches default-directory before those in auto-insert-path.")

(defvar auto-insert-num-prefix "0"
  "*String used as prefix for numerical days and months.
Suggested values are \" \", \"0\" and \"\".")

;; Private variables.
(defvar auto-insert-history nil
  "Minibuffer history list for insert-auto-insert-type")


;; This is not portable. Ho hum.
(defconst auto-insert-months
      '("Dec" "Nov" "Oct" "Sep" "Aug" "Jul"
	"Jun" "May" "Apr" "Mar" "Feb" "Jan")
      "Backward list of months as they appear in current-time-string")

(defconst auto-insert-date-pattern
  "^\\(\\w+\\)\\s-+\\(\\w+\\)\\s-+\\([0-9]+\\)\\s-+\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)\\s-+\\([0-9]+\\)"
  "Pattern to use for breaking up date into day, month, year etc.
Bracketed patterns should be: day month date hours mins secs year.")


(defun insert-auto-insert-file (file)
;;; MAGIC-README-TEXT-BEGIN
  "Auto insert FILE into current buffer, performing %-expansions.
%u is replaced by user's login name
%U is user's full name
%h is the name of the user's machine
%a is the user's mail address (from the variable user-mail-address)
%f is the file name (without directory part)
%F is the file name capitalised
%b is the base file name, without suffix
%B is the base file name capitalised
%e is the file's suffix (extension)
%E is the file's suffix capitalised
%p is the full path name of the file
%P is the directory part of the file name
%d is the current date
%y is the current year
%Y is the last two digits of the current year
%D is the day of the month
%M is the month in figures
%m is the month in words
%o is the user's Organisation (from the variable auto-insert-organisation)
%( and %) bound an ELisp form that is evalled and deleted.
%{ and %} bound a comment which will be removed.
$[ and %] bound a string which will be used to prompt user for a replacement.
%1-%9 refer to the nth strings prompted for with %[ %]
%< and %> bound a value for fill-prefix
%^ centres the current line
%+ fills the current paragraph
%= justifies the current paragraph
%% is a % 
%@ sets the initial position of `point'."
;;; MAGIC-README-TEXT-END
  (interactive "fAuto insert file: ")
  (if (file-readable-p file)
      (let* ((bmp (buffer-modified-p))
	     (case-fold-search nil)
	     (point-set nil)
	     (prompted-strings ())
	     (buffer-file-name (or buffer-file-name (buffer-name)))
	     (buffer-file-name-nondirectory
	      (file-name-nondirectory buffer-file-name))
	     (current-time-string (current-time-string))
	     (foo-bar   (string-match auto-insert-date-pattern
				      current-time-string))	;Get matches
	     (current-year-string
	      (substring current-time-string
			 (match-beginning 7) (match-end 7)))
	     (current-short-year-string (substring current-year-string -2 nil))
	     (current-date-string (concat (substring current-time-string 0 11)
					  current-year-string))
	     (current-month-string
	      (substring current-time-string
			 (match-beginning 2) (match-end 2)))
	     (current-month-number
	      (auto-insert-format
	       (int-to-string
		(auto-insert-month-number current-month-string))))
	     (current-day-string (auto-insert-format
				  (substring current-time-string
					     (match-beginning 3)
					     (match-end 3)))))
	(save-restriction
	  (narrow-to-region (point) (point))
	  (insert-file-contents file)
	  (while (not (eobp))
	    (if (search-forward "%" (point-max) "stay.at.end")
		(progn
		  (delete-backward-char 1)
		  ;; This really should be a user extensible lookup table
		  ;; and not a (cond ...)
		  (cond
		   ;; Straight replacement text.
		   ((looking-at "h")
		    (auto-insert-substitute auto-insert-system-name))
		   ((looking-at "a")
		    (auto-insert-substitute user-mail-address))
		   ((looking-at "u")
		    (auto-insert-substitute auto-insert-login-name))
		   ((looking-at "U")
		    (delete-char 1)
		    (insert auto-insert-full-name))
		   ((looking-at "f")
		    (auto-insert-substitute buffer-file-name-nondirectory))
		   ((looking-at "F")
		    (auto-insert-substitute buffer-file-name-nondirectory 'up))
		   ((looking-at "b")
		    (auto-insert-substitute (file-name-sans-suffix
			     buffer-file-name-nondirectory)))
		   ((looking-at "B")
		    (auto-insert-substitute (file-name-sans-suffix
				     buffer-file-name-nondirectory) 'up))
		   ((looking-at "e")
		    (auto-insert-substitute
		     (file-name-suffix buffer-file-name)))
		   ((looking-at "E")
		    (auto-insert-substitute (file-name-suffix buffer-file-name)
					    'up))
		   ((looking-at "p")
		    (auto-insert-substitute buffer-file-name))
		   ((looking-at "P")
		    (auto-insert-substitute
		     (or (file-name-directory buffer-file-name)
			 ".")))
		   ((looking-at "d")
		    (auto-insert-substitute current-date-string))
		   ((looking-at "y")
		    (auto-insert-substitute current-year-string))
		   ((looking-at "Y")
		    (auto-insert-substitute current-short-year-string))
		   ((looking-at "D")
		    (auto-insert-substitute current-day-string))
		   ((looking-at "m")
		    (auto-insert-substitute current-month-string))
		   ((looking-at "M")
		    (auto-insert-substitute current-month-number))
		   ((looking-at "o")
		    (auto-insert-substitute auto-insert-organisation))
		   ;; Magical codes
		   ((looking-at "@")
		    (auto-insert-set-point))
		   ((looking-at "\\^")
		    (auto-insert-center-line))
		   ((looking-at "\\+")
		    (auto-insert-fill-paragraph))
		   ((looking-at "=")
		    (auto-insert-justify-paragraph))
		   ((looking-at "[1-9]")
		    (auto-insert-nth-prompted
		     (string-to-int (buffer-substring
				     (match-beginning 0)
				     (match-end 0)))))
		   ;; Magical codes that take arguments
		   ((looking-at "<\\([^%]*\\)%>")
		    (let* ((start (match-beginning 1)) (end (match-end 1))
			   (fill-string (buffer-substring start end)))
		      (delete-region (1- start) (+ end 2))
		      (setq fill-prefix fill-string)))
		   ((looking-at "\\[\\([^%]*\\)%\\]")
		    (let* ((start (match-beginning 1)) (end (match-end 1))
			   (pr-string (buffer-substring start end)))
		      (delete-region (1- start) (+ end 2))
		      (auto-insert-prompt pr-string)))
		   ((looking-at "{\\([^%]*\\)%}")
		    (let* ((start (match-beginning 1)) (end (match-end 1)))
		      (delete-region (1- start) (+ end 2))))
		   ((looking-at "(\\([^%]*\\)%)")
		    (let* ((start (match-beginning 1)) (end (match-end 1))
			   (ev-string (buffer-substring start end)))
		      (delete-region (1- start) (+ end 2))
		      (save-excursion	;; Protect the innocent
			(save-restriction
			  (eval (car (read-from-string ev-string)))))))
		   ((looking-at "%") (forward-char 1))
		   ((looking-at ".") ;Default
		    (message "Auto-insert: unknown `%%' escape: %%%s"
			     (buffer-substring (match-beginning 0)
					       (match-end 0)))
		    (sit-for 5))))
	      (if (not (eobp))
		  (forward-char 1)))))
	(if (and point-set (not (interactive-p)))
	    (goto-char point-set)	;This is probably the wrong thing to
	  (mark-whole-buffer))		;do when called interactively...
	(set-buffer-modified-p bmp)	; ``I didn't see nuffin' officer''
	t)				;Succeed
    (message "Auto-insert: file %s not found" file)
    (sit-for 1)))			;Fail

;;; Function for placing in find-file-not-found-hooks
(defun insert-auto-insert-files ()
  "Insert default contents into a new file.
Matches the visited file name against the elements of `auto-insert-alist'."
  
  (let* ((the-file-name (or buffer-file-name (buffer-name)))
	 (alist auto-insert-alist)
	 (type-alist auto-insert-type-alist)
	 ;; remove backup suffices from file name
	 (name (file-name-sans-versions the-file-name))
	 (insert-file-type nil)
	 (insert-file nil))

    ;; find first file type matching alist entry
    (while (and (not insert-file-type) alist)
      (if (string-match (car (car alist)) name)
	  (setq insert-file-type (cdr (car alist)))
	(setq alist (cdr alist))))

    ;; find filename from type
    (while (and (not insert-file) type-alist)
      (if (string-equal (car (car type-alist)) insert-file-type)
	  (setq insert-file (cdr (car type-alist)))
	(setq type-alist (cdr type-alist))))
    
    ;; Now, if we found an appropriate insert file, insert it
    (if insert-file
	(let* ((dirs (if auto-insert-search-current-dir
			 (cons default-directory auto-insert-path)
		       auto-insert-path))
	       (file (auto-insert-concat-file (car dirs) insert-file)))
	  (while (and (not (file-readable-p file)) (cdr dirs))
	    (setq dirs (cdr dirs))
	    (setq file (auto-insert-concat-file (car dirs) insert-file)))
	  (if (or (equal t auto-insert-automatically)
		  (and auto-insert-automatically
		       (y-or-n-p "Insert file contents automatically ? ")))
	      (insert-auto-insert-file file))))))

;;; Recommended interactive entry point.
;;;###autoload
(defun insert-auto-insert-type (type)
  "Insert insert file of type TYPE into current buffer.
Uses types in variable auto-insert-type-alist.
See insert-auto-insert-file."
  (interactive
   (list
    (completing-read "Auto insert type: "
		     auto-insert-type-alist nil t nil 'auto-insert-history)))
  (let ((type-alist auto-insert-type-alist)
	(insert-file nil))
    ;; find filename from type
    (while (and (not insert-file) type-alist)
      (if (string-equal (car (car type-alist)) type)
	  (setq insert-file (cdr (car type-alist)))
	(setq type-alist (cdr type-alist))))
    (if insert-file
	(let* ((dirs (if auto-insert-search-current-dir
			 (cons default-directory auto-insert-path)
		       auto-insert-path))
	       (file (auto-insert-concat-file (car dirs) insert-file)))
	  (while (and (not (file-readable-p file)) dirs)
	    (setq dirs (cdr dirs))
	    (setq file (auto-insert-concat-file (car dirs) insert-file)))
	  (insert-auto-insert-file file)))))

;;; Internal functions begin here.

(defun auto-insert-format (num-string)
  "Format a numerical string using auto-insert-num-prefix"
  (let ((res (concat auto-insert-num-prefix num-string)))
    (if (> (length res) 2)
	(substring res -2 nil) ;Only keep 2 chars
      res)))

(defun auto-insert-month-number (month-string)
  "Return number of month 1-12."
  (catch 'found
    (let ((list auto-insert-months))
      (while list
	(if (string= month-string (car list))
	    (throw 'found (length list))
	  (setq list (cdr list))))
      0)))

;; Basic replacement function
(defun auto-insert-substitute (exp &optional case)
  "Replace the character under point with the value of EXP. 
Change the case of EXP according to CASE.
nil   - insert as is.
'up   - make upper case.
'down - make lower case
'cap  - make initial caps."
  (delete-char 1)
  (insert (cond
	   ((equal case 'up)   (upcase (eval exp)))
	   ((equal case 'down) (downcase (eval exp)))
	   ((equal case 'cap)  (capitalize (eval exp)))
	   (t                  (eval exp)))))
  
;; Prompt user for replacement string
(defun auto-insert-prompt (prompt)
  "Prompt with PROMPT for replacement string for character under point."
  (let ((text (read-string prompt)))
    (insert text)
    (setq prompted-strings (append prompted-strings (list text)))))

(defun auto-insert-nth-prompted (n)
  "Insert the text of the Nth prompted for string."
  (delete-char 1)
  (let ((text (nth (1- n) prompted-strings)))
    (if (stringp text)
	(insert text)
      (message "No string number %d" n)
      (sit-for 1))))

(defun auto-insert-set-point ()
  "Set the variable point-set to be the position to return to after inserting."
  (if (not point-set)			; Save point to go back here later
      (setq point-set (point)))
  (delete-char 1))

(defun auto-insert-center-line ()
  "Centre the current line."
  (delete-char 1)
  (save-excursion
    (save-restriction
      (center-line))))

(defun auto-insert-fill-paragraph ()
  "Fill current paragraph. Remember to set fill prefix first with %<...%>
for non-text buffers."
  (delete-char 1)
  (save-excursion
    (save-restriction
      (fill-paragraph nil))))

(defun auto-insert-justify-paragraph ()
  "Justify current paragraph. Remember to set fill prefix first with %<...%>
for non-text buffers."
  (delete-char 1)
  (fill-paragraph t))


;; Misc functions that could be part of Emacs...

(defun file-name-sans-suffix (name)
  "Return file name NAME without suffix.
This is everything before the last dot."
  (substring name 0 (string-match "\\.[^.]*$" (file-name-nondirectory name))))

(defun file-name-suffix (name)
  "Return the suffix of file NAME, or \"\" if none."
  (let* ((fname (file-name-nondirectory name))
	 (pos (string-match "\\.[^.]*$" fname)))
    (if pos
	(substring fname (1+ pos) nil)
      "")))

(defun auto-insert-concat-file (dir file)
  "Concatenate DIR and FILE to make a filename.
Ensures there are no // sequences to avoid later functions stripping the
leading directory part."
  (concat (file-name-as-directory dir) file))

  


;; Make this feature take effect when a nonexistent file is visited.
;; Make this the last in the list, so rcs or vc can check out files that
;; really should exist already.
(add-hook 'find-file-not-found-hooks 'insert-auto-insert-files 'append)

(provide 'auto-insert-tkld)		; We're here
;(provide 'auto-insert)			; Eventually

;;; auto-insert-tkld.el ends here
