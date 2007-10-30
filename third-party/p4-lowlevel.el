;;; p4-lowlwevel.el --- low-level support for Perforce operations in Emacs

;; Copyright (C) 2002 Curl Corporation.

;; Author: Jonathan Kamens <jik@kamens.brookline.ma.us>
;; Maintainer: Jonathan Kamens <jik@kamens.brookline.ma.us>

;; $Id: //guest/jonathan_kamens/vc-p4/p4-lowlevel.el#4 $
;; The path above is on the Perforce server public.perforce.com:1666.
;; You can get this file using a P4 client talking to that depot, or
;; from the URL
;; http://public.perforce.com/guest/jonathan_kamens/vc-p4/p4-lowlevel.el.

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

;; This file provides an Emacs-lisp interface to the Perforce client
;; functionality which is necessary to implement support for Perforce
;; in Emacs VC mode.

;;; Code:

(defgroup p4-lowlevel nil
  "Emacs-lisp interface to Perforce operations."
  :group 'tools)

(defcustom p4-lowlevel-p4-program "p4"
  "*The `p4' program to run to execute Perforce commands."
  :type 'string
  :group 'p4-lowlevel)

(defcustom p4-lowlevel-diff-switches '("-du")
  "*The flags to use when calling `p4 diff' or `p4 diff2'."
  :type '(choice (const :tag "None" nil)
                 (repeat :tag "Argument List"
                         :value ("")
                         string))
  :group 'p4-lowlevel)

(defcustom p4-lowlevel-command-messages nil
  "*If non-nil, display run messages from P4 commands.
If vc-command-messages is bound and non-nil, it does the same thing."
  :type 'boolean
  :group 'p4-lowlevel)

(defun p4-lowlevel-command-to-buffer (args &optional input output)
  "Call `p4-lowlevel-p4-command' with specified list of ARGS.
ARGS can be a list or a single string argument.  Do not specify the
`-s' argument to p4; it is always specified by this function.  The
remaining arguments are optional.  If INPUT is non-nil, it is a buffer
or file to use as input.  If OUTPUT is non-nil, it is a buffer in
which to insert the command's output at point; otherwise, a temporary
buffer is created.

If the p4 invocation is unsuccessful, checks to make sure there's an
`exit:' line at the end of the output with a non-zero value in it; if
not, it adds such a line containing either the non-zero exit status or
the signal information from `call-process', after deleting any
previous `exit: 0' line (which shouldn't be there since the
`call-process' failed).

Returns the buffer containing the program output."
  (if (stringp args)
      (setq args (list args)))
  (let* (apply-args p4-args call-func string-command exit-status
         (temp-name " *p4-lowlevel-output*")
         (output-buffer (or output (p4-lowlevel-get-buffer-create temp-name)))
         (my-default-directory default-directory))
    (save-excursion
      (set-buffer output-buffer)
      (while (and my-default-directory 
                  (not (file-exists-p my-default-directory))
                  (not (string= "" my-default-directory)))
        (setq my-default-directory 
              (file-name-as-directory 
               (file-name-directory (directory-file-name 
                                     my-default-directory)))))
      (if (not (file-exists-p my-default-directory))
          (setq my-default-directory "/"))
      (erase-buffer)
      (if (bufferp input)
          (progn (set-buffer input)
                 (setq call-func 'call-process-region)
                 (setq apply-args (list (point-min) (point-max)
					p4-lowlevel-p4-program nil)))
        (setq call-func 'call-process)
        (setq apply-args (list p4-lowlevel-p4-program input)))
      (setq p4-args (append (list "-s") args))
      (if (p4-lowlevel-command-messages)
          (progn (setq string-command
                       (mapconcat (lambda (s) s)
                                  (append (list p4-lowlevel-p4-program)
					  p4-args) " "))
                 (message "Running %s..." string-command)))
      (let ((default-directory my-default-directory))
        (setq exit-status (apply call-func 
                                 (append apply-args (list output-buffer nil)
                                         p4-args))))
      (if (p4-lowlevel-command-messages)
          (message "Running %s... exit %s" string-command exit-status))
      (if (and (numberp exit-status) (zerop exit-status)) t
        (set-buffer output-buffer)
        (goto-char (point-max))
        (forward-line -1)
        (if (looking-at "^exit: [^0]") t
          (if (looking-at "^exit:")
              (kill-line 1)
            (goto-char (point-max))
            (or (looking-at "^") (insert "\n")))
          (insert "exit: ")
          (if (numberp exit-status)
              (insert (format "%d" exit-status))
            (insert exit-status))
          (insert "\n")))
      output-buffer)))

(defun p4-lowlevel-buffer-to-alist (&optional buffer)
  "Converts the current buffer containing p4 output to an alist.  If
optional argument BUFFER is non-nil, converts that buffer instead.
The returned alist contains one element for each `unit' in the buffer.
A `unit' is either a line containing a tag (`info:' or `info#:',
`error:', `exit:', etc.) or a block of untagged text.  The car of each
element is the tag for the line (i.e., the string to the left of the
colon, or `text' for text units), and the cdr is the value for the
line (i.e., everything after the first `: ' sequence) or the entire
block of untagged text (including newlines other than the last one).
The alist is in the same order as the contents of the buffer."
  (save-excursion
    (if buffer (set-buffer buffer))
    (let (alist tag value
                (last-match-end (point-min)))
      (goto-char last-match-end)
      (while (re-search-forward "^\\([A-Za-z0-9]+\\): \\(.*\\)" nil t)
        (if (not (= (match-beginning 1) last-match-end))
            (progn (setq tag "text"
                         value (buffer-substring last-match-end 
                                                 (- (match-beginning 1) 1))
                         alist (cons (cons tag value) alist))))
        (setq tag (match-string 1)
              value (match-string 2)
              alist (cons (cons tag value) alist)
              last-match-end (+ (match-end 2) 1)))
      (if (not (= last-match-end (point-max)))
          (progn (setq tag "text"
                       value (buffer-substring last-match-end
					       (- (point-max) 1))
                       alist (cons (cons tag value) alist))))
      (nreverse alist))))

(defun p4-lowlevel-command-to-alist (args &optional input)
  "Calls `p4-lowlevel-command-to-buffer' and then
`p4-lowlevel-buffer-to-alist'.  Passes ARGS and optional INPUT to
`p4-lowlevel-command-to-buffer'.  Hands the resulting buffer to
`p4-lowlevel-buffer-to-alist' for parsing.  Kills the output buffer
when it has been parsed.  Returns the resulting alist on success, or
the return value of `p4-lowlevel-command-to-buffer' on failure."
  (let ((output-buffer (p4-lowlevel-command-to-buffer args input))
	return-value)
    (setq return-value (p4-lowlevel-buffer-to-alist output-buffer))
    (kill-buffer output-buffer)
    return-value))

(defun p4-lowlevel-re-assoc (key alist)
  "Get elements matching regexp KEY in ALIST, or nil if none."
  (let (element matching-alist)
    (while alist
      (setq element (car alist)
            alist (cdr alist))
      (if (string-match key (car element))
          (setq matching-alist (cons element matching-alist))))
    (nreverse matching-alist)))

(defun p4-lowlevel-successful-alist-p (output)
  "Determines if OUTPUT, a buffer or alist, is from a successful p4 command.
Does this by confirming that OUTPUT is a buffer or alist, that there
are no `error' keys in it, and that its `exit' element (if any) has a
value of 0."
  (if (bufferp output)
      (setq output (p4-lowlevel-buffer-to-alist output)))
  (if (not (listp output))
      nil
    (let ((element (assoc "exit" output)))
      (if (not (or (not element) (equal (cdr element) "0")))
          nil
        (if (p4-lowlevel-re-assoc "^error" output)
            nil
          t)))))

(defun p4-lowlevel-items-matching-tag (tag output)
  "Returns a list of the items maching TAG in p4 OUTPUT, or nil if none.
OUTPUT may be a buffer or alist."
  (if (bufferp output)
      (setq output (p4-lowlevel-buffer-to-alist output)))
  (mapcar (lambda (pair) (cdr pair)) (p4-lowlevel-re-assoc tag output)))

(defun p4-lowlevel-lines-matching-tag (tag output)
  "Returns a string containing the lines matching TAG in p4 OUTPUT, or
nil if none.  OUTPUT may be a buffer or alist.  The lines are
terminated by newlines.  The tags are not included in the string."
  (if (bufferp output)
      (setq output (p4-lowlevel-buffer-to-alist output)))
  (let* ((alist (p4-lowlevel-re-assoc tag output)) 
         (lines (apply 'concat
                       (apply 'append 
                              (mapcar (lambda (pair)
                                        (list (cdr pair) "\n")) alist)))))
    (if (equal lines "")
        nil
      lines)))

(defun p4-lowlevel-errors (output)
  "Returns a string containing the errors in p4 OUTPUT, or nil if none.
OUTPUT may be a buffer or alist.  The error lines are separated by
newlines, but there is no ending newline on the string."
  (let ((errors (p4-lowlevel-lines-matching-tag "^error" output)))
    (if errors
        (substring errors 0 -1)
      nil)))

(defun p4-lowlevel-info-lines (output)
  "Returns a string containing the info in p4 OUTPUT, or nil if none.
OUTPUT may be a buffer or alist.  The info lines are terminated by
newlines."
  (p4-lowlevel-lines-matching-tag "^info" output))

(defun p4-lowlevel-text (output)
  "Returns a string containing the text in p4 OUTPUT, or nil if none.
OUTPUT may be a buffer or alist.  The text lines are terminated by
newlines."
  (p4-lowlevel-lines-matching-tag "^text" output))

(defun p4-lowlevel-command-or-error (args &optional input output-format noerror)
  "Executes p4 command specified by ARGS and returns output or signals error.
Pass optional argument INPUT to `p4-lowlevel-command-to-buffer'.  If optional
argument OUTPUT-FORMAT is \'string, return a string containing the
output (including tags).  If it is \'buffer, return the temporary
buffer containing the output.  If it is a buffer, put output in that
buffer and return it.  If it is anything else, return an alist of the
output.  If optional fourth argument NOERROR is true, then returns nil
rather than raising an error."
  (let* (errors error-buffer return-value
         (output-buffer (p4-lowlevel-command-to-buffer args input 
                                              (if (bufferp output-format)
                                                  output-format)))
         (output-alist (p4-lowlevel-buffer-to-alist output-buffer)))
    (if (p4-lowlevel-successful-alist-p output-alist) t
      (setq errors (or (p4-lowlevel-errors output-alist) "Unknown error"))
      (kill-buffer output-buffer)
      (or noerror
          (if (not (string-match "\n" errors))
              (error "P4 error: %s" errors)
            (setq error-buffer (p4-lowlevel-get-buffer-create 
				" *p4-lowlevel-error*"))
            (set-buffer error-buffer)
            (erase-buffer)
            (insert errors)
            (goto-char (point-min))
            (pop-to-buffer error-buffer nil t)
            (resize-temp-buffer-window)
            (error ""))))
    (cond
     (errors (setq return-value nil))
     ((eq output-format 'string)
      (save-excursion (set-buffer output-buffer)
                      (setq return-value (buffer-string)))
      (kill-buffer output-buffer))
     ((or (eq output-format 'buffer) (bufferp output-format))
      (setq return-value output-buffer))
     (t (setq return-value output-alist)
        (kill-buffer output-buffer)))
    return-value))

(defun p4-lowlevel-command-into-buffer (args buffer)
  "Executes p4 command specified by ARGS, raising errors when necessary.
If BUFFER is a string, then puts output in buffer whose name is formed
by concatenating ` *p4-lowevel-', BUFFER-NAME, and `*' (e.g., if BUFFER is
`diff', then output goes in buffer ` *p4-lowevel-diff*').  If BUFFER is a
buffer, then puts output in that buffer.  Returns the buffer."
  (let* ((output-alist (p4-lowlevel-command-or-error args))
         (output-buffer (if (bufferp buffer) buffer
                          (p4-lowlevel-get-buffer-create 
			   (concat " *p4-lowlevel-" buffer "*"))))
         text)
    (save-excursion
      (set-buffer output-buffer)
      (erase-buffer)
      (insert (p4-lowlevel-info-lines output-alist))
      (if (setq text (p4-lowlevel-text output-alist))
          (insert text))
      output-buffer)))

(defun p4-lowlevel-command-messages ()
  "Return t if p4-lowlevel-command-messages or vc-command-messages is
bound and true."
  (if (and (boundp 'vc-command-messages) vc-command-messages)
      t
    p4-lowlevel-command-messages))

(defun p4-lowlevel-canonicalize-revision (rev)
  "Turn REV into a form which can be concatenated to file names in P4
commands."
  ; There is some ambiguity here, since a number can be either a
  ; revision number (#rev) or a change number (@change).  We assume
  ; that a bare number is a revision number.
  (if rev
      (if (string= rev "")
          nil
        (if (string-match "\\`[0-9]+\\'" rev)
            (concat "#" rev)
          (if (not (string-match "^[#@]" rev))
              (concat "@" rev)
            rev)))))

; Here's what we need to support from the "p4 add" command, at least
; for the time being:
;
; Do NOT need to support "-c".
; Do NOT need to support "-t".
; Do NOT need to support the specification of multiple files.

(defun p4-lowlevel-add (file)
  "Tell Perforce to add FILE to the repository.
Returns nil or raises an error on failure."
  ; Note that because "p4 -s add" has bugs, at least as of p4 99.2,
  ; this won't necessarily detect when the add fails, e.g., because of
  ; an attempt to add a file which already exists in the repository.
  (p4-lowlevel-command-or-error (list "add" file)))

; Here's what we need to support from the "p4 change" command, at
; least for the time being:
; 
; Do NOT need to support "-f".
; Do NOT need to support "-d".
; DO need to support "-o".
; DO need to support "-i".
; DO need to support specified changelist #'s.

(defun p4-lowlevel-change (&optional buffer op)
  "Creates or edits a P4 changelist from/to BUFFER.
If optional OP is a number, then the corresponding changelist is
retrieved into BUFFER, or into a new buffer if BUFFER is nil.  If OP
is non-nil and not a number, then then BUFFER should contain an
existing changelist which is saved to the database; the number of the
new or updated changelist is returned.  If OP is nil then a new
changelist is retrieved into BUFFER (or a new buffer).  The output
buffer is returned."
  (let* ((input-buffer (if (and op (not (numberp op))) buffer nil))
         (flag-arg (if (or (not op) (numberp op)) "-o" "-i"))
         (number-arg (if (numberp op) (list (number-to-string op))))
         (args (append (list "change" flag-arg) number-arg))
         alist info)
    (setq alist (p4-lowlevel-command-or-error args input-buffer nil))
    (setq info (p4-lowlevel-info-lines alist))
    (if (and op (not (numberp op)))
        (if (string-match "\\([0-9]+\\)" info)
            (string-to-number (match-string 1 info))
          (error "P4 error: could not parse info \"%s\"" info))
      (if (not buffer)
          (setq buffer (p4-lowlevel-get-buffer-create 
			" *p4-lowlevel-change*")))
      (save-excursion
        (set-buffer buffer)
        (erase-buffer)
        (insert (p4-lowlevel-info-lines alist))
        buffer))))

(defun p4-lowlevel-changes (file-pattern &optional output-format rev1 rev2 i-flag l-flag m-val s-val)
  "Call `p4 changes' on FILE-PATTERN.  Optional OUTPUT-FORMAT is as
described in `p4-lowlevel-command-or-error'.  Optionally, limit output
to the revisions between REV1 and REV2.  If I-FLAG is non-nil, pass
`-i'; if L-FLAG is non-nil, pass `-l'; if M-VAL is non-nil, pass that
value with `-m'; if S-VAL is non-nil, pass that value with `-s'."
  (setq rev1 (p4-lowlevel-canonicalize-revision rev1)
	rev2 (p4-lowlevel-canonicalize-revision rev2))
  (let ((full-file
	 (if (or rev1 rev2)
	     (format "%s%s,%s" file-pattern (or rev1 "") (or rev2 ""))
	   file-pattern))
	(i-list (if i-flag (list "-i")))
	(l-list (if l-flag (list "-l")))
	(m-list (if m-val (list "-m" (if (numberp m-val)
					 (number-to-string m-val)
				       m-val))))
	(s-list (if s-val (list "-s" s-val))))
    (p4-lowlevel-command-or-error (append
				   (list "changes")
				   i-list l-list m-list s-list
				   (list full-file))
				  nil output-format)))

; Here's what we need to support from the "p4 diff" command, at
; least for the time being:
; 
; DO need to support "-d<flag>".
; DO need to support "-f" (in fact, need to specify it all the time).
; Do NOT need to support "-s<flag>".
; DO need to support "-t" (in fact, need to specify it all the time).
; DO need to support diffing a single file.
; Do NOT need to support diffing multiple files.

(defun p4-lowlevel-diff (file &optional rev buffer)
  "Run `p4 diff' on FILE at revision REV and return a buffer
containing the results.  REV is in the syntax described by `p4 help
revisions'.  If REV is nil, compare the client's sync'd revision to
the file on disk.  Uses `p4-lowlevel-diff-switches' to determine flags
to pass to `p4 diff'.  If optional BUFFER is non-nil, put output in
that buffer."
  (setq rev (p4-lowlevel-canonicalize-revision rev))
  (let* ((file-spec (if rev (concat file rev) file))
         (diff-args (append (list "diff") p4-lowlevel-diff-switches
                            (list "-f" "-t" file-spec)))
         (buffer (p4-lowlevel-command-into-buffer diff-args 
						  (or buffer "diff"))))
    buffer))

(defun p4-lowlevel-diff-s (file flag)
  "Run `p4 diff -s' on FILE, using FLAG as the argument to `-s', and
return a list of the matching files."
  (p4-lowlevel-items-matching-tag 
   "^info"
   (p4-lowlevel-command-or-error 
    (list
     "diff"
     (format "-s%s" flag)
     file))))

; Here's what we need to support from the "p4 diff2" command, at least
; for the time being:
;
; DO need to support "-d<flag>".
; Do NOT need to support "-q".
; DO need to support "-t" (in fact, need to specify it all the time).
; Do NOT need to support "-b".

(defun p4-lowlevel-diff2 (file1 file2 &optional rev1 rev2 buffer)
  "Run `p4 diff2' on FILE and FILE2 and return a buffer containing the
results.  If optional REV1 and/or REV2 are non-nil, they specify the
revisions to diff in the syntax described by `p4 help revisions'.  If
optional BUFFER is non-nil, output goes in that buffer.  Uses
`p4-lowlevel-diff-switches' to determine flags to pass to `p4 diff2'."
  (setq rev1 (p4-lowlevel-canonicalize-revision rev1)
        rev2 (p4-lowlevel-canonicalize-revision rev2))
  (let* ((file1-spec (if rev1 (concat file1 rev1) file1))
         (file2-spec (if rev2 (concat file2 rev2) file2))
         (diff-args (append (list "diff2") p4-lowlevel-diff-switches
                            (list "-t" file1-spec file2-spec)))
         (buffer (p4-lowlevel-command-into-buffer diff-args 
						  (or buffer "diff"))))
    buffer))

; Here's what we need to support from the "p4 edit" command, at least
; for the time being:
; 
; Do NOT need to support "-c".
; Do NOT need to support "-t".
; Do NOT need to support the specification of multiple files.

(defun p4-lowlevel-edit (file)
  "Tell Perforce we want to edit FILE.
Returns non-nil on success or nil on failure (or raises an error)."
  (p4-lowlevel-command-or-error (list "edit" file)))

; Here's what we need to support from the "p4 filelog" command, at
; least for the time being:
;
; DO need to support "-i".
; DO need to support "-l".
; Do NOT need to support "-m".
; Do NOT need to support the specification of multiple files.

(defun p4-lowlevel-filelog (file &optional buffer long follow-branches)
  "Fetch the p4 log of FILE and return a buffer containing it.
If optional BUFFER is non-nil, put output in that buffer.  If optional
LONG is non-nil, return long output (i.e., pass the `-l' flag).  If
optional FOLLOW-BRANCHES is non-nil, include pre-branch log entries in
output (i.e., pass the `-i' flag)."
  (let* ((long-flag (if long (list "-l") nil))
         (branch-flag (if follow-branches (list "-i") nil))
         (args (append (list "filelog") long-flag branch-flag (list file))))
    (p4-lowlevel-command-into-buffer args (or buffer "log"))))

(defun p4-lowlevel-opened (file)
  "Fetch the string returned by running `p4 opened' on FILE."
  (p4-lowlevel-command-or-error (list "opened" file) nil 'string))

; Here's what we need to support from the "p4 fstat" command, at least
; for the time being:
; 
; Do NOT need to support any command-line switches.
; Do NOT need to support the specification of multiple files.

(defun p4-lowlevel-fstat (file &optional rev noerror)
  "Fetch p4 information about FILE (optionally, at REV).
REV should be in the syntax described by `p4 help revisions'.  Returns
a list of field-name/value elements on success, or raises an error on
failure.  If optional third argument NOERROR is true, then returns nil
rather than raising an error on failure.  If FILE matches multiple
files, then returns a list of lists of field-name/value elements."
  (setq rev (p4-lowlevel-canonicalize-revision rev))
  (let* ((file-spec (if rev (concat file rev) file))
         (args (list "fstat" file-spec))
         (alist (p4-lowlevel-re-assoc 
		 "^info" (p4-lowlevel-command-or-error args nil nil noerror)))
         element line field value values lists)
    (while alist
      (setq element (car alist)
            alist (cdr alist)
            line (cdr element))
      (if (not (string-match " " line))
	  t
	(setq field (substring line 0 (match-beginning 0))
	      value (substring line (match-end 0)))
	(if (string= field "depotFile") ; we assume depotFile is
					; always first
	    (if (not values)
		t
	      (setq lists (cons (nreverse values) lists))
	      (setq values nil)))
	(setq values (cons (cons field value) values))))
    (when values
      (setq lists (cons (nreverse values) lists)))
    (if (= (length lists) 1)
	(car lists)
      lists)))

(defun p4-lowlevel-info ()
  "Return an alist representing the output of `p4 info'."
  (let* ((base-alist (p4-lowlevel-command-or-error "info" nil nil t))
         (info-elements (p4-lowlevel-re-assoc "^info" base-alist))
         line tag value info-alist element)
    (while info-elements
      (setq element (car info-elements)
            info-elements (cdr info-elements)
            line (cdr element))
      (if (string-match ": " line)
          (setq tag (substring line 0 (match-beginning 0))
                value (substring line (match-end 0))
                info-alist (cons (cons tag value) info-alist))))
    (nreverse info-alist)))

(defun p4-lowlevel-print (file &optional rev output-format quiet)
  "Retrieve the contents of FILE using `p4 print'.
If optional REV is non-nil, retrieve that revision, which should be in
the syntax described by `p4 help revisions'.  Optional OUTPUT-FORMAT
is interpreted as described for `p4-lowlevel-command-or-error'.  If optional
QUIET is non-nil, then the `-q' flag is passed to `p4 print'."
  (setq rev (p4-lowlevel-canonicalize-revision rev))
  (let* ((fullfile (if rev (concat file rev) file))
         (quiet-args (if quiet (list "-q")))
         (args (append (list "print") quiet-args (list fullfile))))
    (p4-lowlevel-command-or-error args nil output-format)))
  
; Here's what we need to support from the "p4 reopen" command, at
; least for the time being:
; 
; DO need to support "-c changelist#", so that we can reopen a file in
;   the default changelist before submitting it.
; Do NOT need to support "-t".
; Do NOT need to support the specification of multiple files.

(defun p4-lowlevel-reopen (file &optional changelist)
  "Call `p4 reopen' on FILE.
Optional CHANGELIST specifies the changelist to which to move it."
  (p4-lowlevel-command-or-error (append (list "reopen")
                               (if changelist (list "-c" changelist) nil)
                               (list file))))

; Here's what we need to support from the "p4 resolve" command, at
; least for the time being:
; 
; DO need to support "-af" (in fact, need to specify it all the time).
; Do NOT need to support "-am", "-as", "-at", "-ay".
; Do NOT need to support "-f".
; Do NOT need to support "-n".
; DO need to support "-t" (in fact, need to specify it all the time).
; Do NOT need to support "-v".
; Do NOT need to support the specification of multiple files.

(defun p4-lowlevel-resolve (file)
  "Call `p4 resolve' on FILE.
Specifies the `-af' and `-t' options to ensure a non-interactive
resolve.  Raises an error if the command fails."
  (p4-lowlevel-command-or-error (list "resolve" "-af" "-t" file)))

; Here's what we need to support from the "p4 revert" command, at
; least for the time being:
; 
; Do NOT need to support "-a".
; Do NOT need to support "-c".
; Do NOT need to support the specification of multiple files.

(defun p4-lowlevel-revert (file)
  "Tell Perforce to unedit FILE."
  (p4-lowlevel-command-or-error (list "revert" file)))

; Here's what we need to support from the "p4 submit" command, at
; least for the time being:
; 
; Only need to support non-interactive use; therefore, only need to
; support "p4 submit -i".

(defun p4-lowlevel-submit (change-spec)
  "Calls `p4 submit' on CHANGE-SPEC, which should be a string or buffer."
  (let (buffer)
    (if (bufferp change-spec)
        (setq buffer change-spec)
      (setq buffer (p4-lowlevel-get-buffer-create 
		    " *p4-lowlevel-submit-input*"))
      (save-excursion
        (set-buffer buffer)
        (erase-buffer)
        (insert change-spec)))
    (p4-lowlevel-command-or-error (list "submit" "-i") buffer)))

; Here's what we need to support from the "p4 sync" command, at least
; for the time being:
; 
; DO need to support "-f".
; Do NOT need to support "-n".
; DO need to support the specification of a file revision.
; Do NOT need to support the specification of multiple files.

(defun p4-lowlevel-sync (file &optional rev force)
  "Call `p4 sync' for FILE.
If optional REV is specified, use that revision specifier.  If
optional FORCE is non-nil, pass the `-f' flag."
  (setq rev (p4-lowlevel-canonicalize-revision rev))
  (let* ((fullfile (if rev (concat file rev) file))
         (force-args (if force (list "-f")))
         (args (append (list "sync") force-args (list fullfile))))
    (p4-lowlevel-command-or-error args)))

(defun p4-lowlevel-integrate (from-file to-file &optional rev1 rev2 force)
  "Call `p4 integrate' from FROM-FILE to TO-FILE, with optional revision
range specified by REV1 and REV2, forcing the integration (i.e.,
specifying `-f' to `p4 integrate' if FORCE is non-nil."
  (setq rev1 (p4-lowlevel-canonicalize-revision rev1)
	rev2 (p4-lowlevel-canonicalize-revision rev2))
  (let ((force-list (if force (list "-f")))
	(from-full (if (or rev1 rev2)
		       (format "%s%s,%s" from-file (or rev1 "") (or rev2 ""))
		     from-file)))
    (p4-lowlevel-command-or-error (append (list "integrate")
					  (if force (list "-f"))
					  (list from-full to-file)))))

(defun p4-lowlevel-client-version (&optional noerror)
  "Returns the Perforce client version string from `p4 -V'.
Returns the third field of the last line of output from `p4 -V', or
signals an error if the invocation failed.  if optional NOERROR is
non-nil, returns nil instead of signalling an error."
  (let ((version-string (p4-lowlevel-command-or-error "-V" nil 'string
						      noerror)))
    (if (string-match "\n[^/\n]+/[^/\n]+/\\([^\n/]+\\)/.*\n?\\'"
		      version-string)
        (setq version-string (match-string 1 version-string)))
    version-string))

(defun p4-lowlevel-get-buffer-create (name)
  "Like get-buffer-create, but always changes default-directory of the
returned buffer to the current default-directory, even if the buffer
already exists."
  (let ((buf (get-buffer-create name))
        (caller-default-directory default-directory))
    (save-excursion
      (set-buffer buf)
      (setq default-directory caller-default-directory))
    buf))

(provide 'p4-lowlevel)
