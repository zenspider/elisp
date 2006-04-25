;;; icicles-mac.el --- Macros for Icicles
;; 
;; Filename: icicles-mac.el
;; Description: Macros for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:24:28 2006
;; Version: 22.0
;; Last-Updated: Fri Mar 31 08:50:59 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 46
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-mac.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  This is a helper library for library `icicles.el'.  It defines
;;  macros.  See `icicles.el' for documentation.
;; 
;;  Macros defined here:
;;
;;    `icicle-define-command', `icicle-define-file-command'.
;; 
;;  Standard Emacs function defined here for older Emacs versions:
;;
;;    `select-frame-set-input-focus'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 2006/03/31 dadams
;;     icicle-define(-file)-command: Wrap action function in unwind-protect to select minibuf frame.
;; 2006/03/11 dadams
;;     icicle-define-file-command: Expand file in directory of icicle-last-input.
;; 2006/03/08 dadams
;;     icicle-define(-file)-command: Bug fix (thx to TobyCubitt):
;;       Make sure icicle-candidate-action-fn runs FUNCTION in original buffer and window.
;; 2006/03/07 dadams
;;     icicle-define(-file)-command: Mention in doc string that BINDINGS are not in effect
;;       within icicle-candidate-action-fn.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; Byte-compiling this file, you will likely get some error or warning
;; messages. All of the following are benign.  They are due to
;; differences between different versions of Emacs.
;;
;; Compiling in Emacs 20:
;;
;; the function x-focus-frame is not known to be defined.

(when (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, unless

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;; Macros  ------------------------------------------------

(defmacro icicle-define-command
    (command doc-string function
     prompt table &optional predicate require-match initial-input hist def inherit-input-method
     bindings first-sexp undo-sexp last-sexp)
  ;; Hard-code these in doc string, because \\[...] prefers ASCII
  ;; `C-RET'   instead of `\\[icicle-candidate-action]'
  ;; `C-next'  instead of `\\[icicle-next-prefix-candidate-action]'
  ;; `C-prior' instead of `\\[icicle-previous-prefix-candidate-action]'
  ;; `next'    instead of `\\[icicle-next-apropos-candidate-action]'
  ;; `prior'   instead of `\\[icicle-previous-apropos-candidate-action]'
  "Define COMMAND with DOC-STRING based on FUNCTION.
COMMAND is a symbol.  DOC-STRING is a string.
FUNCTION is a function that takes one argument, read as input.
  (If the argument to FUNCTION is a file name or directory name, then
  use macro `icicle-define-file-command', instead.)

BINDINGS is a list of `let*' bindings added around the command code.
  The following bindings are pre-included - you can refer to them in
  the command body (including in FIRST-SEXP, LAST-SEXP, UNDO-SEXP).

  `orig-buff'   is bound to (current-buffer)
  `orig-window' is bound to (selected-window)

In case of error or user quit, the original buffer is restored.

FIRST-SEXP is a sexp evaluated before the main body of the command.
UNDO-SEXP is a sexp evaluated in case of error or if the user quits.
LAST-SEXP is a sexp evaluated after the main body of the command.

Other arguments are as for `completing-read'.

In order, the created command does this:

 - Uses DOC-STRING, with information about Icicles bindings appended.
 - Binds BINDINGS for the rest of the command.
 - Evaluates FIRST-SEXP.
 - Reads input with `completing-read', using PROMPT, TABLE, PREDICATE,
   REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and INHERIT-INPUT-METHOD.
 - Calls FUNCTION on the input that was read.
 - Evaluates UNDO-SEXP in case of error or if the user quits.
 - Evaluates LAST-SEXP.

The created command also binds `icicle-candidate-action-fn' to a
function that calls FUNCTION on the current completion candidate.
Note that BINDINGS are of course not in effect within
`icicle-candidate-action-fn'."
  `(defun ,command ()
    ,(concat doc-string "\n\nRead input, then "
             (and (symbolp function) (concat "call `" (symbol-name function) "' to "))
             "act on it.

Input-candidate completion and cycling are available.  While cycling,
these keys act on the current candidate:

\\<minibuffer-local-completion-map>\
`C-RET'   - Act on current completion candidate only
`C-next'  - Act, then move to next \
prefix-completion candidate
`C-prior' - Act, then move to previous \
prefix-completion candidate
`next'    - Act, then move to next \
apropos-completion candidate
`prior'   - Act, then move to previous \
apropos-completion candidate
`\\[icicle-all-candidates-action]'     - Act on *all* candidates, successively (careful!)

Use `RET' or `S-RET' to finally choose a candidate, or `C-g' to quit.
This is an Icicles command - see `icicle-mode'.")
    (interactive)
    (let* ((orig-buff (current-buffer))
           (orig-window (selected-window))
           ,@bindings
           (icicle-candidate-action-fn
            (lambda (candidate)
              (unwind-protect
                   (condition-case action-fn-return
                       (progn
                         (condition-case in-action-fn
                             (with-current-buffer orig-buff
                               (save-selected-window
                                 (select-window orig-window)
                                 (funcall ',function candidate)))
                           (error (unless (string= "Cannot switch buffers in minibuffer window"
                                                   (error-message-string in-action-fn))
                                    (error (error-message-string in-action-fn)))
                                  (select-frame-set-input-focus (window-frame orig-window))
                                  (funcall ',function candidate)))
                         (select-frame-set-input-focus (window-frame (minibuffer-window)))
                         nil)           ; Return nil for success.
                     (error (error-message-string action-fn-return))) ; Return error msg.
                (select-frame-set-input-focus (window-frame (minibuffer-window)))))))
      
      ,first-sexp
      (condition-case act-on-choice
          (funcall ',function (completing-read ,prompt ,table ,predicate ,require-match
                                               ,initial-input ,hist ,def ,inherit-input-method))
        (quit (when (buffer-live-p orig-buff) (switch-to-buffer orig-buff)) ,undo-sexp)
        (error (when (buffer-live-p orig-buff) (switch-to-buffer orig-buff)) ,undo-sexp
               (error (error-message-string act-on-choice))))
      ,last-sexp)))

(defmacro icicle-define-file-command
    (command doc-string function
     prompt &optional dir default-filename require-match initial-input predicate
     bindings first-sexp undo-sexp last-sexp)
  ;; Hard-code these in doc string, because \\[...] prefers ASCII
  ;; `C-RET'   instead of `\\[icicle-candidate-action]'
  ;; `C-next'  instead of `\\[icicle-next-prefix-candidate-action]'
  ;; `C-prior' instead of `\\[icicle-previous-prefix-candidate-action]'
  ;; `next'    instead of `\\[icicle-next-apropos-candidate-action]'
  ;; `prior'   instead of `\\[icicle-previous-apropos-candidate-action]'
  "Define COMMAND with DOC-STRING based on FUNCTION.
COMMAND is a symbol.  DOC-STRING is a string.
FUNCTION is a function that takes one file-name or directory-name
argument, read as input.  (Use macro `icicle-define-command' for a
FUNCTION whose argument is not a file or directory name.)

BINDINGS is a list of `let*' bindings added around the command code.
  The following bindings are pre-included - you can refer to them in
  the command body (including in FIRST-SEXP, LAST-SEXP, UNDO-SEXP).

  `orig-buff'   is bound to (current-buffer)
  `orig-window' is bound to (selected-window)

In case of error or user quit, the original buffer is restored.

FIRST-SEXP is a sexp evaluated before the main body of the command.
UNDO-SEXP is a sexp evaluated in case of error or if the user quits.
LAST-SEXP is a sexp evaluated after the main body of the command.

Other arguments are as for `read-file-name'.

In order, the created command does this:

 - Uses DOC-STRING, with information about Icicles bindings appended.
 - Binds BINDINGS for the rest of the command.
 - Evaluates FIRST-SEXP.
 - Reads input with `read-file-name', using PROMPT, DIR,
   DEFAULT-FILENAME, REQUIRE-MATCH, INITIAL-INPUT, and PREDICATE.
 - Calls FUNCTION on the input that was read.
 - Evaluates UNDO-SEXP in case of error or if the user quits.
 - Evaluates LAST-SEXP.

The created command also binds `icicle-candidate-action-fn' to a
function that calls FUNCTION on the current completion candidate.
Note that BINDINGS are of course not in effect within
`icicle-candidate-action-fn'."
  `(defun ,command ()
    ,(concat doc-string "\n\nRead input, then "
             (and (symbolp function) (concat "call `" (symbol-name function) "' to "))
             "act on it.

Input-candidate completion and cycling are available.  While cycling,
these keys act on the current candidate:

\\<minibuffer-local-completion-map>\
`C-RET'   - Act on current completion candidate only
`C-next'  - Act, then move to next \
prefix-completion candidate
`C-prior' - Act, then move to previous \
prefix-completion candidate
`next'    - Act, then move to next \
apropos-completion candidate
`prior'   - Act, then move to previous \
apropos-completion candidate
`\\[icicle-all-candidates-action]'     - Act on *all* candidates, successively (careful!)

Use `RET' or `S-RET' to finally choose a candidate, or `C-g' to quit.
This is an Icicles command - see `icicle-mode'.")
    (interactive)
    (let* ((orig-buff (current-buffer))
           (orig-window (selected-window))
           ,@bindings
           (icicle-candidate-action-fn
            (lambda (candidate)
              (setq candidate (expand-file-name candidate
                                                (file-name-directory icicle-last-input)))
              (unwind-protect
                   (condition-case action-fn-return
                       (progn
                         (condition-case in-action-fn
                             (with-current-buffer orig-buff
                               (save-selected-window
                                 (select-window orig-window)
                                 (funcall ',function candidate)))
                           (error (unless (string= "Cannot switch buffers in minibuffer window"
                                                   (error-message-string in-action-fn))
                                    (error (error-message-string in-action-fn)))
                                  (select-frame-set-input-focus (window-frame orig-window))
                                  (funcall ',function candidate)))
                         (select-frame-set-input-focus (window-frame (minibuffer-window)))
                         nil)           ; Return nil for success.
                     (error (error-message-string action-fn-return))) ; Return error msg.
                (select-frame-set-input-focus (window-frame (minibuffer-window)))))))
      ,first-sexp
      (condition-case act-on-choice
          (funcall
           ',function
           (if (< emacs-major-version 21) ; No predicate arg for Emacs 20.
               (read-file-name ,prompt ,dir ,default-filename ,require-match ,initial-input)
             (read-file-name ,prompt ,dir ,default-filename ,require-match
                             ,initial-input ,predicate)))
        (quit  (when (buffer-live-p orig-buff) (switch-to-buffer orig-buff)) ,undo-sexp)
        (error (when (buffer-live-p orig-buff) (switch-to-buffer orig-buff)) ,undo-sexp
               (error (error-message-string act-on-choice))))
      ,last-sexp)))

(unless (fboundp 'select-frame-set-input-focus) ; Defined in Emacs 22.
  (defun select-frame-set-input-focus (frame)
    "Select FRAME, raise it, and set input focus, if possible."
    (select-frame frame)
    (raise-frame frame)
    ;; Ensure, if possible, that frame gets input focus.
    (cond ((eq window-system 'x) (x-focus-frame frame))
          ((eq window-system 'w32) (w32-focus-frame frame)))
    (cond (focus-follows-mouse (set-mouse-position (selected-frame) (1- (frame-width)) 0)))))

;; Make Emacs-Lisp mode fontify definitions of Icicles commands.
(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(concat "(" (regexp-opt '("icicle-define-command" "icicle-define-file-command") t)
             "\\s-+\\(\\sw\\(\\sw\\|\\s_\\)+\\)")
    (1 font-lock-keyword-face)
    ;; Index (2 or 3) depends on whether or not shy groups are supported.
    ,(list (if (string-match "\\(?:\\)" "") 2 3) font-lock-function-name-face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-mac)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-mac.el ends here
