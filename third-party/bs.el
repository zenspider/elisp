;;; bs.el --- menu for selecting and displaying buffers,
;;; alternative for C-xC-b.

;;; Copyright (C) 1997-1999 Olaf Sylvester
;;;
;;; Author: Olaf Sylvester <Olaf.Sylvester@kiel.netsurf.de>
;;; Web site: http://homes.cls.net/~Olaf.Sylvester/emacs
;;; Keywords: extensions
;;; Version: $Id: //depot/main/user/ryand/Bin/elisp/third-party/bs.el#1 $

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;

;;; For updating see web site: http://homes.cls.net/~Olaf.Sylvester/emacs
;;; or observe news group gnu.emacs.sources

;;; Commentary:

;; The bs-package has contain a main-function bs-show for poping up a
;; buffer in the way like electric-buffer-list:
;;
;; -----------------------------------------------------------------------
;; | MR Buffer          Size  Mode          File                         |
;; | -- ------          ----  ----          ----                         |
;; |.   bs.el           14690  Emacs-Lisp    /home/sun/sylvester/el/bs.e$|
;; |  % executable.el    9429  Emacs-Lisp    /usr/share/emacs/19.34/lisp$|
;; |  % vc.el          104893  Emacs-Lisp    /usr/share/emacs/19.34/lisp$|
;; |  % test_vc.el        486  Emacs-Lisp    /home/sun/sylvester/el/test$|
;; |  % vc-hooks.el     43605  Emacs-Lisp    /usr/share/emacs/19.34/lisp$|
;; -----------------------------------------------------------------------
;;
;; The key sequence C-x C-b will be bind to bs-show.
;; C-x C-b will generate a new buffer named *buffer-selection* and shows all
;; buffers or a subset of all buffers and has possibilities for deleting,
;; saving and selecting buffers and so on...
;;
;; The package bs unions the advantages of EMACS function list-buffers and
;; electric-buffer-list:
;;
;; features from function electric-buffer-list:
;;  - goto in new generated buffer
;;  - restore window configuration when leaving buffer-selection
;;
;; features from function buffer-list
;;  - bs implements nearly all functions of buffer-list
;;    (see following using section)
;;  - work over keyboard macros (no own eventloop)
;;
;; new features:
;;  - cycling navagation:
;;      - goes to top of bufferlist if you are on last line and press down.
;;      - goes to end of bufferlist if you are on first line and press up.
;;  - configurable list of buffers (show only file etc.)
;;  - show sorted list of buffers
;;  - you can toggle a buffer for showing by 'm'
;;
;; Quick Installation:
;; ===================
;; You only need (if bs.el is in your loadpath)
;; (require 'bs)
;; or
;; (load ".../somewhere/.../bs")
;;
;; Using:
;; ======
;; By C-x C-b a buffer named *buffer-selection* pops up and then
;; - choose a buffer by SPACE or RETURN  or
;; - quit selection by 'q', 'z' or C-g.
;; - toggle "show all buffers" by a
;; - (un)mark a buffer for showing always by 'm'
;; - toggle configurations by 'c'
;; - save a buffer by 's'
;; - toggle read-only by '%'
;; - delete a buffer by 'd'
;;
;; Overview
;; --------
;; Select a buffer                  : SPACE or RETURN.
;; Navigate                         : UP or DOWN, 'p' or 'n'
;; Toggle show all                  : 'a'
;; Save                             : 's'
;; Delete                           : 'd'
;; Stepps throuph configuration     : 'c'
;; Quit                             : 'q' or 'z' or C-g
;; Bury buffer                      : 'b'
;; Toggle read-only                 : '%'
;; set not modified                 : '~'
;; Show sorted list in varous modes : 'S'
;; (un)marking                      : 'm'
;; mark to show always              : '+'
;; mark to show never               : '-'
;;
;;
;;
;; Configuration
;; =============
;; You can define configurations by defining a function, which sets the
;; variables for configuration. There are some configuration (see list
;; bs-configurations). Give your configuration a name and add
;; a new entry to bs-configurations. Activate your configuration
;; by
;;      (bs-set-configuration "yourname")
;; You can change a configuration by
;;     M-x bs-set-configuration
;; or
;;     pressing keys 'c' in buffer *buffer-selection*
;;
;; The default configuration is the configuration with the name in variable
;; bs-default-configuration (here "files" which means show only files)
;;
;; if you always want to see all buffers, you have to set
;;    (setq bs-default-configuration "all")
;;
;;
;; How do I configure?
;; -------------------
;; If you don't want to see intern buffers beginning with '*'
;; you have to set
;; (setq bs-dont-show-regexp "^\\*")
;;
;;
;; ;; If you don't want to see intern Buffer beginning with '*'
;; but you want to see buffer *scratch* then:
;; (setq bs-dont-show-regexp "^\\*")
;; (setq bs-must-show-regexp "^\\*scratch*")
;;
;;
;; ;; If you want to show only buffer containing a file then
;; ;; you have to set
;; (setq bs-dont-show-function 'bs-visits-non-file)
;;
;; Configure sorting
;; -----------------
;; You can define any functions for sorting the buffer list.
;; When selecting buffers, you can step through available sorting
;; methods by key 'S'.
;; To define a new way for sorting an example:
;; (bs-define-sort-function
;;    "by something"
;;    (function my-sort-function))
;;
;; There are four basic functions for sorting:
;;   by name of buffer, by mode, by size or by filename
;;
;; Finally
;; -------
;; My favorite configuration:
;;
;; ;; I want to see *-Buffers at the end
;; (setq bs-buffer-sort-function 'bs-sort-buffer-interns-are-last)
;;
;; ;; Don't show files, who don't belong to a file
;; (setq bs-dont-show-function 'bs-visits-non-file)
;;
;; ;; But show Buffer named "*scratch*"
;; (setq bs-must-show-regexp "^\*scratch\*")

;;; Change Log:


;; $Log: bs.el,v $
;; Revision 1.1  2000/12/21 02:18:06  ryand
;; Added.
;;
;; Revision 1.1  1999/08/30 09:38:07  ryand
;; added
;;
;; Revision 1.11  1999/1/6 00:01:11  sylvestr
;; - some XEmacs integrations (thanks Matthias Helmling)
;;
;; Revision 1.10  1998/11/30 22:51:11  sylvestr
;; - Totally overwork of sorting possibilities which override the
;;   normal way of sorting. There exists a settled sort function
;;   which won't be removed after leaving buffer selection.
;;   Turning into a new buffer selection action, the old override
;;   of sorting will be used.
;; - supports any methods for sorting buffer list
;; - new user function bs-define-sort-function
;;
;; $Log: bs.el,v $
;; Revision 1.1  2000/12/21 02:18:06  ryand
;; Added.
;;
;; Revision 1.1  1999/08/30 09:38:07  ryand
;; added
;;
;; Revision 1.9  1998/10/12 22:50:01  sylvestr
;; - bs-up and bs-down work with positive numeric arguments.
;; - new variables for user configuration:
;;          bs-minimal-buffer-name-column
;;          bs-maximal-buffer-name-column
;; - new function bs-set-current-buffer-to-show-always
;; - new function bs-set-current-buffer-to-show-never
;; - new function bs-help
;;
;;
;; Third state for marking buffers.
;; Now: normally, never and always
;;
;; Revision 1.7  1998/03/12 22:59:55  sylvestr
;; Implementing new features:
;; - marking/unmarking buffers for showing by 'm'
;; - set configurations by 'c'
;; - more comments
;;
;; Revision 1.6  1997/11/08 16:28:09  sylvestr
;; Insert code for toggle show-all.
;; New binding for key 'a'.
;;
;; Revision 1.5  1997/11/08 01:04:03  sylvestr
;; Deleted problems deeeleting current buffer.
;; Correct windows height after deleting.
;; Insert save-window-excursion at vc-toggle-read-only.
;;
;; Revision 1.4  1997/11/08 00:27:22  sylvestr
;; New key definition for 'n' and 'p'.
;; Variable truncate-lines set to t.
;;
;; Revision 1.3  1997/11/05 20:40:22  sylvestr
;; Always show buffer we are starting bs-show.
;; Insert new email.
;;
;; Revision 1.2  1997/11/01 20:07:59  sylvestr
;; Functions bs-sort-buffer-interns-are-last and
;; bs-visits-non-file now belongs to this package.
;;
;; Revision 1.1  1997/11/01 20:02:02  sylvestr
;; Initial revision
;;
;; Thanks for suggestions:
;;
;; Christian Mondrup
;; Juanma Barranquero
;; Kai Grossjohann
;; Matthias Helmling
;; Nishina Shigeaki
;;
;; dizhao@mjordan.hq.primeon.com
;;
;;; Code:

;;; ----------------------------------------------------------------------
;;; Globals for customization
;;; ----------------------------------------------------------------------

(defvar bs-dont-show-regexp nil
  "Regular expression for specifing buffers by name
who must not be shown.")

(defvar bs-must-show-regexp nil
  "Regular expression for specifing buffers by name who must be shown.")

(defvar bs-dont-show-function nil
  "Function (predicate), for specifing buffers who must not be shown.")

(defvar bs-must-show-function nil
  "Function (predicate), for specifing buffers who must be shown.")

(defvar bs-buffer-sort-function nil
  "Sort-function for sorting buffer for apperaring
in buffer *buffer-selection*.")

(defvar bs-maximal-buffer-name-column 45
  "Maximum width of column for buffernames.")

(defvar bs-minimal-buffer-name-column 15
  "Minimum width of column for buffernames.")

;;(defconst bs-header-lines
(defconst bs-header-lines
  '(
    " MR Buffer %s  Size  Mode        File"
    " -- ------ %s  ----  ----        ----"
    )
  "List of strings for headerlines.")

(defvar bs-configurations
  '(
    ("all-intern-last"    . bs-config--all-intern-last)
    ("all"                . bs-config--all)
    ("files"              . bs-config--only-files)
    ("files-and-scratch"  . bs-config--files-and-scratch)
    )
  "List of assocs CONFIGURATION-NAME and his FUNCTION-NAME
to set configuration.")

(defvar bs-default-configuration "files"
  "Name of default configuration for presenting bufferlist.")

(defvar bs-current-configuration bs-default-configuration
  "Actual name off configuration.")

(defconst bs-string-show-always    ">"
  "String for indicating: buffer always will be shown.")
(defconst bs-string-show-never     "-"
  "String for indicating: buffer will be shown never.")
(defconst bs-string-current        "."
  "String for indicating: buffer is current-buffer")
(defconst bs-string-show-normally  " "
  "String for indicating: buffer will be shown normally.")

(defvar bs--name-entry-length 20
  "Length of buffer-names.")

;;; ----------------------------------------------------------------------
;;; Intern globals
;;; ----------------------------------------------------------------------


(defvar bs--running-in-xemacs (string-match "XEmacs" (emacs-version))
  "Wheather we are running under XEmacs")

(defvar bs-buffer-show-flag nil
  "Flag of kind of buffer if showing.")

(make-variable-buffer-local 'bs-buffer-show-flag)

;; Make face named region (for XEmacs)
(if (facep 'region)
    nil
  (make-face 'region)
  (set-face-background 'region "gray75"))


(defun bs--sort-by-name (b1 b2)
  (string< (buffer-name b1)
    (buffer-name b2)))

(defun bs--sort-by-filename (b1 b2)
  (string< (or (buffer-file-name b1) "")
    (or (buffer-file-name b2) "")))

(defun bs--sort-by-mode (b1 b2)
  (save-excursion
    (string< (progn (set-buffer b1) (format "%s" major-mode))
      (progn (set-buffer b2) (format "%s" major-mode)))))

(defun bs--sort-by-size (b1 b2)
  (save-excursion
    (< (progn (set-buffer b1) (buffer-size))
       (progn (set-buffer b2) (buffer-size)))))

(defun bs-define-sort-function (name function &optional
         regexp-for-sorting face)
  "Defines a new function FUNCTION (a predicate) for sorting
under the name NAME."
  (let ((tupel (assoc name bs-sort-functions))
 )
    (if tupel
 (setcdr tupel (list function regexp-for-sorting face))
      (setq bs-sort-functions
     (cons (list name function regexp-for-sorting face)
    bs-sort-functions)))))

(defvar bs-sort-functions
  '()
  "Inventar of all possible sorting aspects as association.")

;; Define a sorting which does nothing.
(bs-define-sort-function
 "by nothing"  nil nil)

(defvar bs--current-sort-function
  (car bs-sort-functions)
  "Current assoc of function for comparing buffers for a sorted list.")

;; Define some more sortings.
(bs-define-sort-function
 "by filename" (function bs--sort-by-filename) "File" 'region)
(bs-define-sort-function
 "by mode" (function bs--sort-by-mode) "Mode" 'region)
(bs-define-sort-function
 "by size" (function bs--sort-by-size) "Size" 'region)
(bs-define-sort-function
 "by name" (function bs--sort-by-name) "Buffer" 'region)

(defvar bs--buffer-coming-from nil)

(defvar bs--show-all nil)

(defvar bs--window-config-coming-from nil)

(defvar bs--intern-show-never "^ \\|\*buffer-selection*")

(defvar bs-current-list nil
  "Intern list of buffers shown in buffer *buffer-selection*
and correspondening to lines of buffer.")

(defvar bs-mode-map ()
  "Keymap in bs-Mode.")

(if (and nil bs-mode-map)
    ()
  (setq bs-mode-map (make-sparse-keymap))
  (define-key bs-mode-map " "       'bs-select)
  (define-key bs-mode-map "f"       'bs-select)
  (define-key bs-mode-map [mouse-2] 'bs-mouse-select) ;; for GNU EMACS
  (define-key bs-mode-map [button2] 'bs-mouse-select) ;; for XEmacs
  (define-key bs-mode-map [up]      'bs-up)
  (define-key bs-mode-map "n"       'bs-down)
  (define-key bs-mode-map "p"       'bs-up)
  (define-key bs-mode-map [down]    'bs-down)
  (define-key bs-mode-map "\C-m"    'bs-select)
  (define-key bs-mode-map "b"       'bs-bury-buffer)
  (define-key bs-mode-map "s"       'bs-save)
  (define-key bs-mode-map "S"       'bs-show-sorted)
  (define-key bs-mode-map "a"       'bs-toggle-show-all)
  (define-key bs-mode-map "d"       'bs-delete)
  (define-key bs-mode-map "C"       'bs-set-configuration-and-refresh)
  (define-key bs-mode-map "c"       'bs-select-next-configuration)
  (define-key bs-mode-map "q"       'bs-kill)
  (define-key bs-mode-map "z"       'bs-kill)
  (define-key bs-mode-map "\C-g"    'bs-abort)
  (define-key bs-mode-map "%"       'bs-toggle-readonly)
  (define-key bs-mode-map "~"       'bs-clear-modified)
  (define-key bs-mode-map "m"       'bs-toggle-current-to-show)
  (define-key bs-mode-map "+"       'bs-set-current-buffer-to-show-always)
  (define-key bs-mode-map ">"       'bs-set-current-buffer-to-show-always)
  (define-key bs-mode-map "-"       'bs-set-current-buffer-to-show-never)
  (define-key bs-mode-map "?"       'bs-help)
  )

;;; ----------------------------------------------------------------------
;;; Functions
;;; ----------------------------------------------------------------------

(defun bs-buffer-list (&optional list sort-description)
  "Returns list of buffers to be shown.
If SORT-DESCRIPTION isn't nil, the buffer will be sorted by
a special function."
  (setq sort-description (or sort-description bs--current-sort-function))
  (if (null list)
      (setq list (buffer-list)))
  (let ((result nil))
    (while list
      (let* ((buffername (buffer-name (car list)))
      (intern-show-never
       (string-match bs--intern-show-never buffername))
      (extern-show-never
       (and bs-dont-show-regexp
     (string-match bs-dont-show-regexp buffername)))
      (extern-must-show
       (and bs-must-show-regexp
     (string-match bs-must-show-regexp buffername)))
      (extern-show-never-from-fun
       (and bs-dont-show-function
     (funcall bs-dont-show-function (car list))))
      (extern-must-show-from-fun
       (and bs-must-show-function
     (funcall bs-must-show-function (car list))))
      (show-flag (save-excursion
     (set-buffer (car list))
     bs-buffer-show-flag))
      )
      (if (or (eq show-flag 'always)
       (and (or bs--show-all (not (eq show-flag 'never)))
            (not intern-show-never)
     (or bs--show-all
         extern-must-show
         extern-must-show-from-fun
         (and (not extern-show-never)
       (not extern-show-never-from-fun)))))
   (setq result (cons (car list)
        result)))
      (setq list (cdr list))))
    (setq result (reverse result))
    ;; buffer we are coming from should be shown in list, so
    ;; that we can leave with space and we are in the buffer we
    ;; started bs-show
    (if (and bs--buffer-coming-from
      (buffer-live-p bs--buffer-coming-from)
      (not (memq bs--buffer-coming-from result)))
 (setq result (cons bs--buffer-coming-from result)))
    ;; sorting
    (if (and sort-description
      (nth 1 sort-description))
 (setq result (sort result (nth 1 sort-description)))
      ;; standard sorting
      (bs-buffer-sort result)
      )))


(defun bs-buffer-sort (buffer-list)
  (if (not bs-buffer-sort-function)
      buffer-list
    (sort buffer-list bs-buffer-sort-function)))


(defun bs-show-in-buffer (list)
  (setq bs-current-list list)
  (switch-to-buffer (get-buffer-create "*buffer-selection*"))
  (bs-mode)
  (let* ((inhibit-read-only t)
  (max-length-of-names (apply 'max
         (cons 0 (mapcar
          (lambda (entry)
     (length (buffer-name entry)))
          list))))
  (name-entry-length
   (min bs-maximal-buffer-name-column
        (max bs-minimal-buffer-name-column max-length-of-names)))
 )
    (erase-buffer)
    (bs--show-header name-entry-length)
    (setq bs--name-entry-length name-entry-length)
    (while list
      (bs--insert-one-entry (car list) name-entry-length)
      (insert "\n")
      (setq list (cdr list)))
    (delete-backward-char 1)
    (bs--set-window-height)
    (goto-line (+ 1 (length bs-header-lines)))
    (bs--goto-current-buffer)
    (bs-apply-sort-faces)
    ))


(defun bs--show-header (name-entry-length)
  (let ((list bs-header-lines))
    (while list
      (insert (format (car list)
        (format (format "%%%ss" (- name-entry-length 5)) ""))
       "\n")
      (setq list (cdr list)))))


(defun bs--redisplay (&optional keep-line-p sort-description)
  (let ((line (1+ (count-lines 1 (point)))))
    (bs-show-in-buffer (bs-buffer-list nil sort-description))
    (if keep-line-p
 (goto-line line))
    (beginning-of-line)))


(defun bs--goto-current-buffer ()
  (let (point)
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp "^\\." nil t)
   (setq point (- (point) 1))))
    (if point
 (goto-char point))))


(defun bs--insert-one-entry (buffer &optional name-entry-length)
  "Generates one entry for buffer BUFFER."
  (setq name-entry-length (or name-entry-length bs--name-entry-length))
  (let (name modified read-only size modename filename
 start flag)
    (save-excursion
      (set-buffer buffer)
      (setq
       name       (buffer-name)
       modified   (buffer-modified-p)
       read-only  buffer-read-only
       size       (buffer-size)
       modename   mode-name
       filename   (or buffer-file-name "")
       flag       bs-buffer-show-flag
      ))
    (insert
     (format "%s%s%s "
      (if (eq bs--buffer-coming-from buffer) bs-string-current
        (cond ((null flag)
        bs-string-show-normally)
       ((eq flag 'never)
        bs-string-show-never)
       (t bs-string-show-always)))
      (if modified  "*" " ")
      (if read-only "%" " ")
      ))
    (setq start (point))
    (insert name)
    (if window-system
 (put-text-property start (point) 'mouse-face 'highlight))
    (insert-char ?  (max 0 (- name-entry-length (length name))))
    (insert
     (format "%8d  %-11s "
      size
      modename
      ))
    (setq start (point))
    (insert filename)
    (if window-system
 (put-text-property start (point) 'mouse-face 'highlight))
    ))

(defun bs-mode ()
  "Major-mode for bs-mode."
  (interactive)
  (kill-all-local-variables)
  (use-local-map bs-mode-map)
  (make-local-variable 'bs--name-entry-length)
  (setq major-mode 'bs-mode
 mode-name "Buffer-Selection-Menu"
 buffer-read-only t
 truncate-lines t)
  (run-hooks 'bs-mode-hook))


(defun bs-kill ()
  "Let buffer disappear and resets window-configuration."
  (interactive)
  (bury-buffer (current-buffer))
  (set-window-configuration bs--window-config-coming-from)
  )

(defun bs-abort ()
  "Makes ding and bury buffer."
  (interactive)
  (ding)
  (bs-kill))

(defun bs-set-configuration-and-refresh ()
  "Ask user for a configuration and applies selected configuration."
  (interactive)
  (call-interactively 'bs-set-configuration)
  (bs--redisplay t))


(defun bs--window-for-buffer (buffer-name)
  "Results in window (in current frame) with shown a
buffer with name BUFFER-NAME; results in nil when
there isn't any."
  (let ((window nil))
    (walk-windows (function (lambda (wind)
         (if (string= (buffer-name
         (window-buffer wind))
        buffer-name)
      (setq window wind)))))
    window))

(defun bs--set-window-height ()
  "Corrects the height of window to a suitable height."
  (if (not (one-window-p (selected-window)))
      (shrink-window
       (- (window-height (selected-window))
   ;; window-height in xemacs includes mode-line
   (+ (if bs--running-in-xemacs 3 1)
      (length bs-header-lines)
      (length bs-current-list))))))

(defun bs--current-buffer ()
  "Results in buffer on current position."
  (beginning-of-line)
  (let ((line (+ (- (length bs-header-lines))
   (count-lines 1 (point)))))
    (nth line bs-current-list)))


(defun bs--update-current-line ()
  (let ((buffer (bs--current-buffer))
 (inhibit-read-only t))
    (beginning-of-line)
    (delete-region (point) (save-excursion (end-of-line)(point)))
    (bs--insert-one-entry buffer)
    (beginning-of-line)
    ))


(defun bs-select ()
  "Does action by selection: Restores window-configuration and
selects buffer on current line"
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (set-window-configuration bs--window-config-coming-from)
    (switch-to-buffer buffer)
    ))

(defun bs-mouse-select (event)
  "Selects buffer on current mouzse-click."
  (interactive "e")
  (mouse-set-point event)
  (bs-select))


(defun bs-bury-buffer ()
  "Burys buffer on current line."
  (interactive)
  (bury-buffer (bs--current-buffer))
  (bs--redisplay t))


(defun bs-save ()
  "Saves buffer on current line."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (save-excursion
      (set-buffer buffer)
      (save-buffer))
    (bs--update-current-line)))

(defun bs-toggle-current-to-show ()
  "Toggles current buffer on line for showing always."
  (interactive)
  (let ((buffer (bs--current-buffer))
 res)
    (save-excursion
      (set-buffer buffer)
      (setq res (cond ((null bs-buffer-show-flag)
         'never)
        ((eq bs-buffer-show-flag 'never)
         'always)
        (t nil)))
      (setq bs-buffer-show-flag res))
    ;;(bs--redisplay t)
    (bs--update-current-line)
    (bs--set-window-height)
    (bs--show-config-message res)))

(defun bs-set-current-buffer-to-show-always ()
  "Say current buffer on line to show always."
  (interactive)
  (bs--set-toggle-to-show (bs--current-buffer) 'always))

(defun bs-set-current-buffer-to-show-never ()
  "Say current buffer on line to show always."
  (interactive)
  (bs--set-toggle-to-show (bs--current-buffer) 'never))

(defun bs--set-toggle-to-show (buffer what)
  "Say buffer BUFFER that his flag must be WHAT."
  (save-excursion
    (set-buffer buffer)
    (setq bs-buffer-show-flag what))
  (bs--update-current-line)
  (bs--set-window-height)
  (bs--show-config-message what))

(defun bs--show-config-message (what)
  "Show message when changing mode of showing one buffer."
  (message (cond ((null what)
    "Buffer will be shown normally.")
   ((eq what 'never)
    "Mark buffer to show never.")
   (t "Mark buffer to show always."))))

(defun bs-delete ()
  "Kills buffer on current line."
  (interactive)
  (kill-buffer (bs--current-buffer))
  (bs--redisplay t)
  (bs--set-window-height)
  )

(defun bs-show-sorted ()
  "Show bufferlist sorted by buffer-name."
  (interactive)
  (setq bs--current-sort-function
 (bs-next-config-aux (car bs--current-sort-function)
       bs-sort-functions))
  (bs--redisplay)
  (bs--goto-current-buffer)
  (message "Sorted %s" (car bs--current-sort-function))
  )

(defun bs-apply-sort-faces (&optional sort-description)
  "Set text properties for the sort described by sort-description."
  (let ((sort-description (or sort-description
         bs--current-sort-function)))
    (save-excursion
      (goto-char (point-min))
      (if (and window-system
        (nth 2 sort-description)
        (search-forward-regexp (nth 2 sort-description) nil t))
   (let ((inhibit-read-only t)
  )
     (put-text-property (match-beginning 0)
          (match-end 0)
          'face
          (or (nth 3 sort-description)
       'region)))))))
(defun bs-toggle-show-all ()
  "Toggle show all buffers / show buffers with current configuration."
  (interactive)
  (setq bs--show-all (not bs--show-all))
  (bs--redisplay)
  (bs--goto-current-buffer)
  )

(defun bs-toggle-readonly ()
  "Toggles read-only for buffer on current line.
Uses Function vc-toggle-read-only."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (save-excursion
      (set-buffer buffer)
      (vc-toggle-read-only))
    (bs--update-current-line)))

(defun bs-clear-modified ()
  "Set buffers (on current line) flag for modified to nil."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (save-excursion
      (set-buffer buffer)
      (set-buffer-modified-p nil)))
  (bs--update-current-line))

(defun bs--nth-wrapper (count fun &rest args)
  "Call Function FUN COUNT times with arguments ARGS"
  (setq count (or count 1))
  (while (> count 0)
    (apply fun args)
    (setq count (1- count))))

(defun bs-up (arg)
  "Moves cursor up in bs-mode."
  (interactive "p")
  (bs--nth-wrapper arg 'bs--up))


(defun bs--up ()
  "Moves cursor up in bs-mode."
  (interactive "p")
  (previous-line 1)
  (if (<= (count-lines 1 (point)) (1- (length bs-header-lines)))
      (goto-char (point-max)))
  (beginning-of-line))


(defun bs-down (arg)
  "Moves cursor down in bs-mode."
  (interactive "p")
  (bs--nth-wrapper arg 'bs--down))

(defun bs--down ()
  "Moves cursor down in bs-mode."
  (let ((last (save-excursion (end-of-line) (point))))
    (if (eq last (point-max))
 (goto-line (1+ (length bs-header-lines)))
      (next-line 1))))


(defun bs-visits-non-file (buffer)
  "Predicate:
T:   Buffer BUFFER belongs to no file.
NIL: Buffer BUFFER belongs to a file."
  (not (buffer-file-name buffer)))


(defun bs-sort-buffer-interns-are-last (b1 b2)
  "Sortingpredicate to show intern buffers beginning with *
at the end of all buffers."
  (if (string-match "^\\*" (buffer-name b2))
      t
    nil))

;;; ----------------------------------------------------------------------
;;; Configurations:
;;; ----------------------------------------------------------------------

(defun bs-config-clear()
  (setq bs-dont-show-regexp nil
 bs-must-show-regexp nil
 bs-dont-show-function nil
 bs-must-show-function nil
 bs-buffer-sort-function nil))

(defun bs-config--only-files ()
  (bs-config-clear)
  ;; I want to see *-Buffers at the end
  (setq bs-buffer-sort-function 'bs-sort-buffer-interns-are-last)
  ;; Don't show files, who don't belong to a file
  (setq bs-dont-show-function 'bs-visits-non-file)
  )

(defun bs-config--files-and-scratch ()
  (bs-config-clear)
  ;; I want to see *-Buffers at the end
  (setq bs-buffer-sort-function 'bs-sort-buffer-interns-are-last)
  ;; Don't show files, who don't belong to a file
  (setq bs-dont-show-function 'bs-visits-non-file)
  ;; Show *scratch*-buffer.
  (setq bs-must-show-regexp "^\*scratch\*")
  )

(defun bs-config--all ()
  (bs-config-clear))

(defun bs-config--all-intern-last ()
  (bs-config-clear)
  ;; I want to see *-Buffers at the end
  (setq bs-buffer-sort-function 'bs-sort-buffer-interns-are-last)
  )


(defun bs-set-configuration (name)
  "Ask user for a configuration and applies selected configuration."
  (interactive
   (list
    (completing-read "Use configuration: "
       bs-configurations
       nil
       t )))
  (let ((fun (assoc name bs-configurations)))
    (if fun
 (progn
   (setq bs-current-configuration name)
   (funcall (cdr fun)))
      (ding)
      (message "No bs-configuration named %S." name))))

(defun bs-help ()
  "Help for bs-show."
  (interactive)
  (describe-function 'bs-show))

(defun bs-next-config-aux (start-name liste)
  "Gets the next assoc of LISTE after START-NAME. Will return
the first if START-NAME is at end."
  (let ((assocs liste)
 (length (length liste))
 pos)
    (while (and assocs (not pos))
      (if (string= (car (car assocs)) start-name)
   (setq pos (- length (length assocs))))
      (setq assocs (cdr assocs)))
    (setq pos (1+ pos))
    (if (eq pos length)
 (car liste)
      (nth pos liste))
    ))


(defun bs-next-config (&optional start-name)
  "The next configuration respective the current."
  (bs-next-config-aux (or start-name bs-current-configuration)
        bs-configurations))

(defun bs-select-next-configuration (&optional start-name)
  "Select and appies the available configuration of bufferlisting."
  (interactive)
  (let ((config (bs-next-config start-name)))
    (bs-set-configuration (car config))
    (bs--redisplay t)
    (bs--set-window-height)
    (message "Selected config: %s." (car config)))
  )


;;; ----------------------------------------------------------------------
;;; Mainfunction bs-show
;;; ----------------------------------------------------------------------

;;;###autoload
(defun bs-show ()
  "Generates a new buffer *buffer-selection* for editing a list of
buffers. Move with [up] or [down]. Select a buffer by [space] or [return]
\n\n\\{bs-mode-map}"
  (interactive)
  (if (not (string= "*buffer-selection*" (buffer-name)))
      ;; Only being not in buffer *buffer-selection*
      ;; we have to set the buffer we started the command
      (progn
 (setq bs--buffer-coming-from (current-buffer))
 (setq bs--window-config-coming-from (current-window-configuration))))
  (let ((liste (bs-buffer-list))
 (active-window (bs--window-for-buffer "*buffer-selection*"))
 )
    (if active-window
 (select-window active-window)
      (if (> (window-height (selected-window)) 5)
   (progn
     (split-window-vertically)
     (other-window 1))))
    (bs-show-in-buffer liste)))


;;;###autoload
(global-set-key "\C-x\C-b" 'bs-show)

;;; set default configuration
(bs-set-configuration bs-default-configuration)

;;; Now provide the feature bs
(provide 'bs)

;;; bs.el ends here
