;;; dope.el --- DOtemacs ProfilEr.. A per-sexp-evaltime profiler.
;; Time-stamp: <2003-01-16 11:27:28 deego>
;; Copyright (C) 2002 Free Software Foundation, Inc.
;; Emacs Lisp Archive entry
;; Filename: dope.el
;; Package: dope
;; Author: D. Goel <deego@glue.umd.edu>, Steve Youngs <youngs@xemacs.org>
;; Keywords:  dotemacs, startup, speed, profile, error
;; Version: 1.3
;; Author's homepage: http://deego.gnufans.org/~deego
;; For latest version:

(defconst dope-home-page
  "http://deego.gnufans.org/~deego/emacspub/lisp-mine/dope/")


 
;; This file is a part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 

;; See also:
  ;; profile.el that comes with your Emacs.

;; Quick start:
(defconst dope-quick-start
  "Place this file \(dope\) somewhere in your load-path.  Compile it.

For best and unbiased profile-results, start with emacs -q, and now:

For a general file.el's load-time-profiling:
 \(M-x dope-reset-results\)
 M-x dope-load-file foo/bar/file.el \(or dope-load-file ~/.emacs\)
 M-x dope-display-results

 \(now, in the results, click on anything you like to be taken to that place..\)

For dot-emacs profiling:

 Start emacs -q.
 M-x load-file foo/bar/dope.elc
  \(or if dope is bundled with emacs and hence already in your load-path,
   use M-x load-library dope\).
 Optionally, M-x dope-reset-results
 M-x dope-load-file ~/.emacs.
 M-x dope-display-results.
 C-x C-c.

If your .emacs is split into multiple files and you DO want to profile
ALL sexps them all, that is more complicated---
Create a file ~/.emacs.dope.  Here's a sample .emacs.dope:
\(load foo/bar/\"dope.elc\"\) ;; saves you having to load it by hand.
\(setq dope-files
      '\(\"~/.emacs\" \"~/.emacs.console\" \"~/.emacs.colors\"\)\)
;; this variable contains the names of the files that your .emacs may
;; load.. and are to to be profiled..  these names should match
;; exactly the ones called from your dot-emacs.
;; if you like, customize dope-special-load-file in .emacs.dope.

Then, start emacs -q -l ~/.emacs/dope and


Now, either type M-x dope-load-multi-files-special

    Or, for more fine-tuned control, do the following steps by hand--->
    \(M-x dope-reset-results\)
    M-x dope-activate-advice
    M-x dope-load-file ~/.emacs
    M-x dope-display-results
    M-x dope-deactivate-advice <-- don't forget this!!
    C-x C-c


\(Once you have chosen one of the sequences above, you will probably
want to put these into your .emacs.dope instead of doing the M-x
calls..\)..

In the dope-display-results buffer, hot keys like TAB, RET and q work..
See also M-x dope-introduction and dope-commentary.

Finally, Any number of files can be dope-loaded at any time, and
combined results shown. ")

;;;###autoload
(defun dope-quick-start ()
  "Provides electric help regarding variable `dope-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert dope-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst dope-introduction
  "Dope is a per-sexp-evaltime-profiler.  Dope profiles the .emacs
\(rather any file\) ...  and helps you identify the sexps taking the most
*loading* time--- very different from the usual 'profilers'.
This 'load-time-profiling' may be best-suited to profiling a .emacs.

If your .emacs is split into multiple files, dope can handle that.

Type M-x dope-quick-start, M-x dope-introduction and M-x dope-commentary.
Once you identify the worst offenders, the commentary has some ideas
on how to shorten the start time..   See all the defcustoms for more
customization, etc.

Useful suggestions are appreciated from: David Masterdon, Dave Pearson,
Toby Speight, John Wiegley.  Tested on Emacs 21.1, 21.2 and XEmacs
21.1, 21.4, 21.5.  Full features may not be visible in emacscvs because
thingatpt.el still broken there.")

;;;###autoload
(defun dope-introduction ()
  "Provides electric help regarding variable `dope-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert dope-introduction) nil) "*doc*"))

;;; Commentary:
(defconst dope-commentary
  "Dope is a different kind of profiler.  \(Your emacs comes with
profile.el--the regular profiler..\).  While profile.el profiles the
time it takes to *call* a function, dope profiles the time taken to
*define* the function.  Thus, it is most suitable for profiling .emacs
and shortening the time.

DOPE does NOT profile the loading of byte-compiled .emacs. IMHO, the
*loading* time for .emacs.el and for .emacs.elc don't differ much, so it
should not matter.   And i guess, if they do differ then each
sexp should hopefully inherit the same constant factor.. So, if you
want to profile a .emacs.elc, simply profile the corresponding
.emacs.el---your aim, after all, is to identify the 'worst
offenders'.


CUSTOMIZING DOPE:
 Yes, you can probably use M-x customize-group to customize dope, but

 You want to move the customizing lines into your .emacs.dope.

  You want to replace stuff like \(custom-set-variables..\) in your
  .emacs.dope by \(setq ...\).  If not, dope will have already loaded
  custom.el before it starts profiling your .emacs, and that will
  result in a lesser time reported for eval'ing a call to customize in
  your .emacs.


Dope tries to be careful to not leave any advise enabled, or to
evaluate any expression of your .emacs twice, but that is not the main
aim.  We assume that you will use it once in a while, to profile, and
then restart your .emacs.  Nonetheless, please do report any such bugs
you find.   Here's a summary of what to do with the worst offenders
\(details follow below\).
 
 * Move them form your .emacs into idledo's

 * Your .emacs probably doesn't need a single \(require\).. All
   functions should really be autoloaded.
 
 * Any other mode-specific action should be performed, not by adding
   it to your .emacs, but by adding it to a relevant hook.

 * Consider not using the 'custom' commands in your .emacs
 
Details:
 

Most likely, there will be \(require\)'s or function-calls in your
.emacs that will be the main culprits.  Once you discover them, you
will want to eliminate most of those.  Moreover, functions and modes
that require loading of files should be moved out of emacs.. they
should be added as hooks to other functions.  Plug: When not moving
his require's into hooks, this author moves them into idledo
\(idledo.el\).  You emacs should ideally autoload all required
files---even the ones you have downloaded should be autoloaded.
\(Plug2: genauto.el\).  I move almost everything into my idledo's unless
it will really break code.  Then, if i find that i really need
something, I just C-u C-u M-x idledo-once.  That performs the
first 8 actions in the idledo list.

I have never used custom, so i don't know how well the don't-use-custom
recommendation will go down with a user of custom...  all i know is
that it took up *most* of the .emacs time for John Wiegley...

Thanks to dope and idledo, This author's .emacs now takes 0.55 seconds to load,
but does about 100 seconds worth of work \(rough guess\) as and when
emacs is idle.

Note that it is perfectly safe to let this file reside in your
load-path.  Even if this file gets loaded into a running dot-emacs, it
doesn't change any behavior of dot-emacs.  It does define advices, but
those advices are disabled and not activated unless you specifically
request.  Those advices will be useful in the special case when you
want to profile a multiply-split dot-emacs.

The reason i asked you to compile dope.el is so that whatever filed dope
uses: advice.el for instance, do not appear as 'loaded' to your
.emacs--- so that if your .emacs calls them, hopefully the right
amount of time gets recorded.


The file should be paren-balanced.  Other than that, for the purpose
of dope-loading a file, any errors due to bad expressions in the file
are skipped after a 1-sec pause and a beep.

Also note that dope works by 'find-file ing' your .emacsen buffer,
which is not really neat, and can break some of your .emacs code..
what is a better way to do things?

Here's useful aliases I use for emacs21 to quickly profile stuff..
\(please note that among the results of the time command, the *third*
entry is the one that supplies the time it took for the command to
execute\).

alias edtime 'time emacs21 -q -l ~/.emacs.editor --eval \"\(kill-emacs\)\"'
alias eddope 'cd ~/emacs/emacspub/lisp-mine/dope/dev; \\\\
 emacs21 -q -l dope.elc -l ~/.emacs.dope'

Note that the total time shown by dope.el can be much higher than the
actual time shown by the time function of GNU..

Note that according to my observation, the results of
 time emacs21 -q -l .emacs.editor  --eval \"\(kill-emacs\)\"
on tcsh \(where my .emacs.editor loads several other files, which were
also profiled..\)  have been seen to be smaller by as much as a factor
of 2--3 from the total time indicated by dope.el.  That, IMHO, is not
unreasonable given the difference between a bunch of eval-sexp's and
the time taken to get 2 \(current-time\)'s surrounding it.. and that of
evaling a file.  In other words, there's overhead.   But again, this
shouldn't affect the *relative* time taken by your individual
instructions --- that is what matters.

You can also interactively dope-load-file files, or you can also
interactively dope-eval-last-sexp-stats on sexps---viz. this will
update the dope-results...

See also the bugs and drawbacks mentioned elsewhere.

old history..

New in 0.3:
==========

  Clickable widget-magic on the dope-results.  Friendlier layout.

  M-x customize-group customizability though see commentary on use of
   customize- commands in .emacs.dope.

New in 0.1:
===========
 Most importantly:  Does not \(require 'cl\) now..
 Rechristened to dope.el, following David Masterdon's note that pod is
  reminiscent of Perl's PODs.
 Thanks also to John Wiegley for feedback leading to a customizable
  dope-separator.

"  )

;;;###autoload
(defun dope-commentary ()
  "Provides electric help regarding variable `dope-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert dope-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:
 ;; Drawbacks:
 ;;  Loading dope loads advice, thingatpt and (the small file) widget.el.
 ;;
 ;; This means that if, by any chance, you load those files, say by
;; doing (require 'advice) or
 ;; (require 'widget) in your .emacs, the time to load .emacs will seem
 ;; quicker because dope already loaded 'advice for you...

 
 

;;; New features:
(defconst dope-new-features
  " ( Note: pod.el has been renamed to dope.el )

New in 1.2
=========

Full XEmacs compliance, and many stylistic fixes, thanks to Steve
Youngs. 

New in 1.0
==========
More straightforward instructions for a split .emacs.  Some
XEmacs-compatibility code from Steve Youngs who is now a co-author :-)


"
)


;;;###autoload
(defun dope-new-features ()
  "Provides electric help regarding variable `dope-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert dope-new-features) nil) "*doc*"))

;;; TO DO:
(defconst dope-todo
  " nothing right now..
"
)

;;;###autoload
(defun dope-todo ()
  "Provides electric help from variable `dope-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert dope-todo) nil) "*doc*"))

(defconst dope-version "1.3")

;;;###autoload
(defun dope-version (&optional arg)
  "Display dope's version string.

With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (dope-message 89 "dope version %s" dope-version))
    (dope-message 88 "dope version %s" dope-version)))

;;==========================================
;;; Code:
(eval-when-compile (require 'custom))
(eval-when-compile (require 'cl))
(require 'thingatpt)
(require 'widget)
(eval-when-compile
  (require 'wid-edit))


(defgroup dope nil
  "Dope: DOtemacs ProfilEr.. A per-sexp-evaltime profiler."
  :group 'applications)


(defcustom dope-before-load-hooks nil
  "Hooks to run before dope loads."
  :type 'hook
  :group 'dope)

(defcustom dope-after-load-hooks nil
  "Hooks to run after dope loads."
  :type 'hook
  :group 'dope)

(run-hooks 'dope-before-load-hooks)



(defvar dope-buffer nil "The buffer (not name) holding dope...")

(defvar dope-current-file "dummy-file")
(defvar dope-current-point nil)
(defvar dope-current-depth -1 "Please do not change this..")
(defvar dope-results nil
  "List of results...
Each result is of the form \(time-taken file depth line-number expression)")

(defvar dope-file-results)
(defvar dope-sorted-results)

(defvar dope-filewise-results nil
  "List of (file total-time num-of-sexps)'s..")

(defcustom dope-special-load-file user-init-file
  "Name of the main Emacs init file (usually ~/.emacs).
Here, .emacs is the mother-dotemacs file that loads other
children-dotemacsen as needed..."
  :type '(file :must-match t)
  :group 'dope)

(defcustom dope-signal-error-function 'error
  "How to handle erros.. 
You might prefer the function 'dope-signal-error here."
  :type 'function
  :group 'dope)

(defcustom dope-display-one-result-function 'dope-display-one-result
  "Function to display one result."
  :type 'function
  :group 'dope)

(defcustom dope-separator
  "\n____________________________________________________________________\n"
  "Separates two expressions..
Example: \"\\n ===== \\n\"."
  :type 'string
  :group 'dope)

(defcustom dope-wait-for-user-p nil
  "If t, will wait after each instruction.."
  :type 'boolean
  :group 'dope)


(defcustom dope-verbosity 0
  "suggested: Anywhere from -100 to 100

The design is such that a value of 0 should be optimum. 
viz.: Once you are experienced with this library, you might prefer a value
of 0 for this variable if this is > 0 right now."
  :type 'integer
  :group 'dope
)




(defcustom dope-interactivity 0
  "suggested: Anywhere from -100 to 100

The design is such that a value of 0 should be optimum. 
viz.: Once you are experienced with this library, you might prefer a value
of 0 for this variable if this is > 0 right now."
  :type 'integer
  :group 'dope
)




(defcustom dope-what-line-function 'dope-what-line
  "Function to use for counting lines and returning an integer.

Default is 'dope-what-line."
  :type 'function
  :group 'dope)



(defcustom dope-error-wait 3
 "Seconds to wait when error encountered."
 :type 'number
 :group 'dope)

(defcustom dope-max-display 100
 "Max number of sexps to display in the results-buffer."
 :type 'integer
 :group 'dope)

(defcustom dope-files (list user-init-file)
  "Files for multiple .emacsen dope-profiling.
No need to use it unless you are profiling multiple .emacsen which
load each other...
See \\[dope-quick-start]"
  :type '(repeat file :must-match t)
  :group 'dope)

(defmacro dope-ignore-errors (&rest body)
  "Execute BODY but capture, and tell any error.
Execute BODY; if an error occurs, return nil.
Otherwise, return result of BODY."

  (let ((err (gensym)))
    `(condition-case ,err (progn ,@body)
       (error
	(funcall dope-signal-error-function (second ,err))))))

(defun dope-signal-error (&rest args)
  (ding t)
  (ding t)
  (ding t)
  (dope-message 90 "IGNORED ERROR: %s.. press RET" 
		(apply 'format args))
  (sit-for dope-error-wait)
  (when (>= dope-interactivity 0) (read-char "Press RET"))
  nil)


(defadvice load (around dope-advice-load
			(file &optional noerror nomessage nosuffix
			      mustsuffix))
  "Advise load to use `dope-load' instead."
  (if (member file dope-files)
      (dope-load file noerror nomessage nosuffix mustsuffix)
    (progn ad-do-it)))

(ad-disable-advice 'load 'around 'dope-advice-load)

(defadvice load-file (around dope-advice-load-file
			     (file))
  "Advise `load-file' to use `dope-load-file' instead."
  (if (member file dope-files)
      (dope-load-file file)
    (progn ad-do-it)))

(ad-disable-advice 'load-file 'around 'dope-advice-load-file)

;; XEmacs doesn't have a `replace-regexp-in-string'.
(if (fboundp 'replace-regexp-in-string)
    (defalias 'dope-replace-regexp-in-string 'replace-regexp-in-string)
  (defun dope-replace-regexp-in-string (regexp rep string &optional
					fixedcase literal subexp start)
    "Replace all matches for REGEXP with REP in STRING.
Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument."
    (let ((l (length string))
	  (start (or start 0))
	  matches str mb me)
      (save-match-data
	(while (and (< start l) (string-match regexp string start))
	  (setq mb (match-beginning 0)
		me (match-end 0))
	  (when (= me mb) (setq me (min l (1+ mb))))
	  (string-match regexp (setq str (substring string mb me)))
	  (setq matches
		(cons (replace-match (if (stringp rep)
					 rep
				       (funcall rep (match-string 0 str)))
				     fixedcase literal str subexp)
		      (cons (substring string start mb)
			    matches)))
	  (setq start me))
	(setq matches (cons (substring string start l) matches))
	(apply #'concat (nreverse matches))))))

;;;###autoload
(defun dope-reset-results ()
  "Reset `dope-results', make it ready for a fresh start."
  (interactive)
  (setq dope-results nil)
  ;;(dope-deactivate-advice)
  )

;;;###autoload
(defun dope-deactivate-advice ()
  "Remove advices on `load' and `load-file'."
  (interactive)
  (ad-disable-advice 'load-file 'around 'dope-advice-load-file)
  (ad-disable-advice 'load 'around 'dope-advice-load)
  ;; disactivates..
  (ad-activate 'load)
  (ad-activate 'load-file)
  (dope-message 10 "Dope advice deactivated")
  )

;;;###autoload
(defun dope-activate-advice ()
  "Advice `load' and `load-file'."
  (interactive)
  (ad-enable-advice 'load-file 'around 'dope-advice-load-file)
  (ad-enable-advice 'load 'around 'dope-advice-load)
  ;; disactivates..
  (ad-activate 'load)
  (ad-activate 'load-file)
  (dope-message 11
   "Warning: All dope advice activated.. All dope advice activated.. All")
  )

;;;###autoload
(defun dope-load (file &rest args)
  "Similar to `load', but keep track of sexp evaltimes.
Try to load FILE the same way as `load' does.  Pass all ARGS to `load'
as well."
  (let* ((fileel (concat file ".el"))
	 (locfileel (locate-library fileel nil))
	 (locfile (locate-library file nil)))
    (cond
     (locfileel
      (dope-load-file fileel))
     (locfile
      (dope-load-file file))
     (t (error "Couldn't find: %s or %s" file fileel)))))

;;;###autoload
(defun dope-load-file (file &optional beg end)
  "Similar to `load-file' but keep track of sexp evaltimes.
Load FILE.

Programmer: Don't think of making this use dope-eval-buffer.  They are
different.  for example, This one tries to find-file before every
expression, that one tries to switch-to-buffer..

This one tries to ensure that it doesn't break, that one tries to be
minimal and no checks..

When BEG and/or END are specified, (please ensure that BEG < END) Only
the region of the file between these two points will be evaluated. "

  (interactive "ffile: ")
  (let ((initbuf (buffer-name))
	(dope-current-file file)
	(dope-current-point (point-min))
	(dope-current-depth (+ 1 dope-current-depth)))
    (find-file file)
    (setq dope-current-file file)
    (setq dope-current-point (or beg (point-min)))

    (goto-char dope-current-point)
    (while
	(progn
	  ;; doing this jazz coz. some user-expression may have caused
	  ;; us to lose this filename...
	  (find-file dope-current-file)
	  (goto-char dope-current-point)
	  (setq dope-current-point (scan-sexps dope-current-point 1))
	  (and
	   dope-current-point
	   (or (null end) (> dope-current-point end)))
	  )
      (goto-char dope-current-point)
      (dope-wait-for-user)
      (save-excursion
	(dope-ignore-errors (dope-eval-last-sexp-stats dope-current-point))))

    (switch-to-buffer initbuf)
    (if (interactive-p)
	(dope-message 5
	 "(Now M-x dope-display-results) Done dope-loading file %s" file))))


;;;###autoload
(defun dope-eval-buffer (&optional buffer)
  ""
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (dope-eval-region nil nil buffer))


(defun dope-eval-region (pta ptb &optional buffer)
  (interactive "r")
  (unless buffer (setq buffer (current-buffer)))
  (let (beg end)
    (when (and (integerp pta) (integerp ptb))
      (cond
       ((< pta ptb) (setq beg pta) (setq end ptb))
       (t (setq beg ptb) (setq end pta))))
    (dope-load-file (buffer-file-name buffer) beg end)))

;;;###autoload
(defun dope-eval-buffer-no-file (&optional buffer)
  ""
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (dope-eval-region-no-file (point-min) (point-max) buffer))


;;;###autoload
(defun dope-eval-region-no-file (pta ptb &optional buffer)
  (interactive "r")
  (unless buffer (setq buffer (current-buffer)))
  (let (beg end
	(initbuf (buffer-name))
	(dope-current-buf buffer)
	(dope-current-file (concat "b:" (buffer-name buffer)))
	dope-current-point
	(dope-current-depth (+ 1 dope-current-depth)))
    (cond
     ((< pta ptb) (setq beg pta) (setq end ptb))
     (t (setq beg ptb) (setq end pta)))
    (setq dope-current-point beg)
    (switch-to-buffer dope-current-buf)
    ;;(setq dope-current-file file)
    (setq dope-current-point beg)

    (goto-char dope-current-point)
    (while
	(progn
	  ;; doing this jazz coz. some user-expression may have caused
	  ;; us to lose this filename...
	  (switch-to-buffer dope-current-buf)
	  (goto-char dope-current-point)
	  (setq dope-current-point (scan-sexps dope-current-point 1))
	  (and dope-current-point (< dope-current-point end)))

      (goto-char dope-current-point)
      (dope-wait-for-user)
      (save-excursion
	(dope-ignore-errors (dope-eval-last-sexp-stats dope-current-point))))

    (switch-to-buffer initbuf)
    (if (interactive-p)
	(dope-message 5
	 "(Now M-x dope-display-results) Done evaling from buffer %s"
	 buffer
	 ))))


;;;###autoload
(defun dope-eval-last-sexp-stats (&optional pt)
  "Eval run time of the last sexp and add the results to `dope-results'.
With argument PT, go to that point first."
  (interactive)
  (unless pt (setq pt (point)))
  (save-excursion
    (goto-char (point))
    (let ((timm (dope-last-sexp-runtime pt)))
      (push
       (list
	timm
	dope-current-file
	dope-current-depth
	(dope-get-line pt)
	(dope-sexp-at-point))
       dope-results)
      (when (interactive-p) (dope-message 85 "%s" timm))))
  nil)

(defun dope-sexp-at-point ()
  "Return the sexp at point."
  (save-excursion
    (backward-sexp 1)
    (sexp-at-point)))
  
(defun dope-get-line (pt)
  "Get the line-number of the start of the sexp point PT."
  (unless pt (setq pt (point)))
  (save-excursion
    (backward-sexp 1)
    (funcall dope-what-line-function)))

;;;###autoload
(defun dope-last-sexp-runtime (&optional pt)
  "Get the runtime of the sexp at point.
With argument PT, Get the runtime of the sexp at point PT.
Perhaps you want to use \\[dope-eval-last-sexp-stats] instead."
  (interactive)
  (unless pt (setq pt (point)))
  (let (ta tb tt)
    (setq ta (current-time))
    (eval-last-sexp nil)
    (setq tb (current-time))
    (setq tt (dope-time-diff tb ta))
    (when (interactive-p) (dope-message 86 "%S" tt))
    tt))

(defun dope-time-diff (tb ta)
  "Get the difference bet times TB and TA, in milliseconds.  A float."
  (+
   (* 0.001 (- (caddr tb) (caddr ta)))
   (* 1000.0
      (+
       (- (second tb) (second ta))
       (* 65536.0
	  (- (car tb) (car ta)))))))

(defun dope-total-time (results)
  "Total time taken spent by all RESULTS."
  (let ((tot 0))
    (while results
      (if (= (cadr (cdar results)) 0)
	  (setq tot (+ tot (caar results))))
      (pop results))
    tot))

(defun dope-display-results ()
  "Display the profiles results.."
  (interactive)
  (require 'wid-edit)
  ;;(setq dope-file-results nil)
  (dope-message 5 "Formatting results...")
  (let ((dope-copy (copy-sequence dope-results)))
    
    (if (bufferp dope-buffer)
	(kill-buffer dope-buffer))
    (setq dope-buffer
	  (get-buffer-create "*dope*"))
    (switch-to-buffer dope-buffer)
    (delete-other-windows)
    (setq dope-sorted-results
	  (sort dope-copy
		#'(lambda (a b)
		    (> (car a) (car b)))))
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (widget-insert "Net Load-time: "
		   (format "%.2fms"
			   (dope-total-time
		     dope-sorted-results))
	    "    Net Number of sexps (all levels): "
	    (format "%s" (length dope-sorted-results)))
    
    (let ((ctr 0) (ress dope-sorted-results))
      (while (and (<= ctr dope-max-display) ress)
	;; can i use pop here??
	(funcall dope-display-one-result-function  (car
						    ress))
	(setq ress (cdr ress))
	(setq ctr (+ ctr 1)))

      (if ress (widget-insert "\n.\n.\n.\n.")
	(widget-insert
	 "\n==============================================================="))
      (use-local-map widget-keymap)
      (local-set-key "q" 'dope-bury-dope
		     )
      (goto-char (point-min))
      (widget-setup)))
  (dope-message 5 "Useful keypresses:  RET, TAB on results, (q)"))

(defun dope-bury-dope ()
  "Bury the `dope-buffer'."
  (interactive)
  (bury-buffer dope-buffer)
  (other-window 1)
  (delete-other-windows)
  (switch-to-buffer (other-buffer dope-buffer))
  )
  
(defun dope-display-one-result (dope-one-result)
"Add one result DOPE-ONE-RESULT into the display buffer.

Using such a lengthy name dope-one-result, to avoid possible namespace
conflicts in the (lambda) within this code."

  (require 'pp)
  (let (wid)
    (widget-insert dope-separator)
    (setq
     wid
     (widget-create
      'push-button
      :notify
      (lambda (wid &rest arg)
	(delete-other-windows)
	(split-window-vertically)
	(find-file (widget-get wid :dopefile))
	(goto-line (widget-get wid :dopeline))
	(dope-message 5 "Going to line %s" (widget-get wid :dopeline))
	(recenter 1)
	(other-window 1))
      ;; (dope-limit-string ;;(pp-to-string
      (format "%.2fms for line %s of file %s at file-depth %S"
	      (car dope-one-result)
	      (cadr (cddr dope-one-result))
	      (cadr dope-one-result)
	      (car (cddr dope-one-result))
	      ))) ; end of setq and widget-create
    (widget-put wid :dopefile (cadr dope-one-result))
    (widget-put wid :dopeline (cadr (cddr dope-one-result)))
    (widget-insert
     "\n  "
     (dope-limit-string (format "%S" (car (last dope-one-result))) 70))))
  
(defun dope-limit-string (str len)
  "Limit string STR to a length LEN and replace newlines by J."
  (let ((str (dope-replace-regexp-in-string "$^" " J" str) ))
    (if (> (length str) len)
	(concat (substring str 0 (- len 3)) " ..")
      str)))

(defun dope-wait-for-user ()
  "Possible, wait for user-confirmation before continuing."
  (if dope-wait-for-user-p
      (while (not (input-pending-p))
	(dope-message 20  "Press a key")
	(sit-for 1)
	0)
    (discard-input)))

;;; 2002-06-19 T00:27:57-0400 (Wednesday)    D. Goel
;; this fcn lifted verbatim from cl-extra's subseq.
(defun dope-subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (if (stringp seq) (substring seq start end)
    (let (len)
      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
      (if (< start 0) (setq start (+ start (or len (setq len (length seq))))))
      (cond ((listp seq)
	     (if (> start 0) (setq seq (nthcdr start seq)))
	     (if end
		 (let ((res nil))
		   (while (>= (setq end (1- end)) start)
		     (cl-push (cl-pop seq) res))
		   (nreverse res))
	       (copy-sequence seq)))
	    (t
	     (or end (setq end (or len (length seq))))
	     (let ((res (make-vector (max (- end start) 0) nil))
		   (i 0))
	       (while (< start end)
		 (aset res i (aref seq start))
		 (setq i (1+ i) start (1+ start)))
	       res))))))

(defun dope-what-line (&optional given-point )
  "Like `what-line', except: return integer.

Lifted from my lines.el.  Tell the current line.  Ignores any narrowing
when counting lines, but does not disrupt the narrowing..

Hacked from the code of `what-line', and i still don't understand some
stuff about the relevance of start here.

Thus, even if the buffer has been narrowed, `lines-what' will try to
return the true line-number.  This may slow things down for large
files, but makes sense to me.  When Optional argument GIVEN-POINT is
given, get the line of that point.  ."
  (interactive)
  (let ((opoint (if given-point given-point (point)))
	start)
    (save-excursion
      (goto-char (point-min))
      (beginning-of-line)
      (setq start (point))
      (goto-char opoint)
      (beginning-of-line)
      (let
	  ((result
	    (if (/= start 1)
		(1+ (count-lines 1 (point)))
	      (1+ (count-lines 1 (point))))))
	(if (interactive-p)
	    (dope-message 79 (format "%S" result)))
	result)))
  )

(defun dope-special-load-multiple-files ()
  "Not a general utility function, see \\[dope-quickstart] for intro."
  (interactive)
  (dope-reset-results)
  (dope-activate-advice)
  (dope-load-file dope-special-load-file)
  (dope-deactivate-advice)
  (dope-display-results)
  )

(defun dope-message (points &rest args)
  (unless (minusp (+ points dope-verbosity))
    (apply #'message args)))

(provide 'dope)
(run-hooks 'dope-after-load-hooks)



;;; dope.el ends here
