;;; icicles-fn.el --- Non-interactive functions for Icicles
;; 
;; Filename: icicles-fn.el
;; Description: Non-interactive functions for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:25:53 2006
;; Version: 22.0
;; Last-Updated: Fri Apr 14 23:08:06 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 873
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-fn.el
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
;;  non-interactive functions.  See `icicles.el' for documentation.
;; 
;;  Functions defined here:
;;
;;    `icicle-activate-mark', `icicle-apropos-candidates',
;;    `icicle-bind-completion-keys', `icicle-bind-isearch-keys',
;;    `icicle-call-then-update-Completions',
;;    `icicle-cancel-*Help*-redirection', `icicle-candidate-set-1',
;;    `icicle-choose-completion-string', `icicle-clear-minibuffer',
;;    `icicle-completing-p', `icicle-completing-read',
;;    `icicle-completion-setup-function', `icicle-delete-if',
;;    `icicle-delete-if-not', `icicle-delete-whitespace-from-string',
;;    `icicle-display-Completions',
;;    `icicle-display-candidates-in-Completions',
;;    `icicle-file-directory-p',
;;    `icicle-file-name-apropos-candidates',
;;    `icicle-file-name-directory-w-default',
;;    `icicle-file-name-input-p', `icicle-file-name-nondirectory',
;;    `icicle-file-name-prefix-candidates', `icicle-file-readable-p',
;;    `icicle-file-writable-p', `icicle-files-within',
;;    `icicle-filter-wo-input', `icicle-fix-default-directory',
;;    `icicle-frames-on', `icicle-highlight-complete-input',
;;    `icicle-increment-cand-nb+signal-end',
;;    `icicle-longest-common-match',
;;    `icicle-maybe-sort-and-strip-candidates',
;;    `icicle-minibuffer-contents',
;;    `icicle-minibuffer-contents-from-minibuffer',
;;    `icicle-minibuffer-prompt-end', `icicle-minibuffer-setup',
;;    `icicle-msg-maybe-in-minibuffer', `icicle-next-candidate',
;;    `icicle-place-cursor', `icicle-place-overlay',
;;    `icicle-prefix-candidates', `icicle-read-file-name',
;;    `icicle-read-from-minibuffer', `icicle-read-string',
;;    `icicle-rebind-completion-maps', `icicle-recompute-candidates',
;;    `icicle-redefine-standard-commands',
;;    `icicle-redefine-standard-options',
;;    `icicle-redefine-std-completion-fns', `icicle-remap',
;;    `icicle-remove-dots', `icicle-remove-duplicates',
;;    `icicle-remove-property', `icicle-restore-completion-keys',
;;    `icicle-restore-region-face',
;;    `icicle-restore-standard-commands',
;;    `icicle-restore-standard-options',
;;    `icicle-restore-std-completion-fns',
;;    `icicle-run-icicle-post-command-hook',
;;    `icicle-run-icicle-pre-command-hook',
;;    `icicle-save-or-restore-input',
;;    `icicle-scroll-or-update-Completions', `icicle-set-calling-cmd',
;;    `icicle-set-difference', `icicle-set-intersection',
;;    `icicle-set-union', `icicle-sort-and-strip-ignored',
;;    `icicle-sort-case-insensitively', `icicle-sort-dirs-last',
;;    `icicle-undo-std-completion-faces', `icicle-unmap',
;;    `icicle-unsorted-apropos-candidates',
;;    `icicle-unsorted-file-name-apropos-candidates',
;;    `icicle-unsorted-file-name-prefix-candidates',
;;    `icicle-unsorted-prefix-candidates',
;;    `icicle-update-completions',
;;    `icicle-update-ignored-extensions-regexp'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `completing-read'              - (See below and doc string.)
;;  `read-file-name'               - (See below and doc string.)
;;  `read-from-minibuffer'         - (See below and doc string.)
;;  `read-string'                  - (See below and doc string.)
;;
;;
;;  ***** NOTE: The following functions defined in `simple.el' have
;;              been REDEFINED HERE:
;;
;;  `choose-completion-string' -
;;     Don't exit minibuffer after `lisp-complete-symbol' completion.
;;  `completion-setup-function' - 1. Put faces on inserted string(s).
;;                                2. Help on help.
;;  `repeat-complex-command' - Use `completing-read' to read command.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 2006/04/14 dadams
;;     icicle-call-then-update-Completions: Call icicle-update-input-hook.
;;     Bound icicle-insert-string-from-variable to C-=.  Added to icicle-completion-help-string.
;; 2006/04/09 dadams
;;     icicle-bind-completion-keys, icicle-minibuffer-setup:
;;       Deal with icicle-arrows-respect-completion-type-flag.
;;     icicle-display-candidates-in-Completions: Bug fix: regexp-quote common match when highlighting it.
;;     icicle-clear-minibuffer: Remove interactive spec.
;;     Moved to icicles-cmd.el: icicle-customize-apropos*, icicle-repeat-complex-command.
;; 2006/04/02 dadams
;;     Bound icicle-toggle-regexp-quote.
;; 2006/03/31 dadams
;;     icicle-next-candidate: Apply icicle-place-cursor to icicle-current-regexp-input if regexp-p.
;;     icicle-save-or-restore-input:
;;       Don't set icicle-current-regexp-input if this is a next-candidate action.
;; 2006/03/27 dadams
;;     icicle-place-overlay: Made generic: added args overlay, face, buffer, properties.
;; 2006/03/25 dadams
;;     icicle-call-then-update-Completions: Corrected use of icicle-incremental-completion*.
;; 2006/03/24 dadams
;;     Renamed icicle-expand-input-to-common-match to icicle-longest-common-match.  Rewrote it.
;;     icicle-call-then-update-Completions: Use icicle-incremental-completion-delay and -threshold.
;;     Mapped icicle-delete-char.
;; 2006/03/23 dadams
;;     icicle-expand-input-to-common-match:
;;       Return the longest common match.  Don't set icicle-common-match-string here.
;;     icicle-unsorted-*apropos-candidates: Set icicle-common-match-string here explicitly.
;;     Added: icicle-maybe-sort-and-strip-candidates.  Use in icicle-candidate-set-1.
;; 2006/03/22 dadams
;;     icicle-display-candidates-in-Completions:
;;       Removed root arg (always use icicle-current-input).
;;       Always highlight normal match part.
;;       Highlight common-match part if icicle-expand-input-to-common-match-flag.
;;     icicle-save-or-restore-input: Update regexp even if not icicle-expand-input-to-common-match-flag.
;;     icicle-recompute-candidates: If no candidates, then delete *Completions* window.
;;     icicle-next-candidate: Set default-directory only if icicle-file-name-input-p.
;;     Applied renamings of icicle-match-* faces.
;; 2006/03/21 dadams
;;     icicle-expand-input-to-common-match:
;;       Bug fixes:
;;         If no overlap between first and second candidates, then no common match.
;;         If no match with another candidate, then no common match.
;;         Input must match computed common match.
;;         When checking others, check only the added (pre|suf)fix, and reduce those as needed.
;;     icicle-save-or-restore-input:
;;       Bug fixes:
;;         When icicle-expand-input-to-common-match-flag, expand using directory from the
;;           input, not the default-directory.  Thx to cacher3.ericsson.net for report.
;;         Do test for case-only difference only when case-fold-search.
;;         If input is a directory (with slash), then use it as is.
;;         Save icicle-current-regexp-input if no icicle-common-match-string too.
;;     icicle-display-candidates-in-Completions: Use icicle-common-match-highlight-Completions.
;; 2006/03/20 dadams
;;     icicle-save-or-restore-input: Set icicle-current-regexp-input too.
;;                                   Corrected letter-case test.
;; 2006/03/19 dadams
;;     Added: icicle-expand-input-to-common-match.
;;     icicle-unsorted*-apropos-candidates:
;;       Set icicle-common-match-string if icicle-expand-input-to-common-match-flag.
;;     icicle-save-or-restore-input:
;;       Added regexp-p arg.  Update input to icicle-common-match-string if appropriate.
;;     icicle-next-candidate: Reset icicle-common-match-string.
;; 2006/03/17 dadams
;;     icicle-file-(read|writ)able-p: Put non-empty string condition first.
;;     Added: icicle-delete-whitespace-from-string.
;;     icicle-files-within: Moved here from icicle-cmd.el.
;; 2006/03/14 dadams
;;     Removed: icicle-reset-icicle-completing-p.
;;     icicle-completing-read, icicle-read-file-name: Removed icicle-icicle-completing-p.
;;     icicle-display-*: Added Displaying... message.
;; 2006/03/13 dadams
;;     Added: icicle-file-(read|writ)able-p.  Bound them to C-{ and C-} in minibuffer.
;;     icicle-rebind-completion-maps, icicle-bind-completion-keys: Added the new commands.
;;     icicle-recompute-candidates: Forgot icicle-keep-only-past-inputs in other branch.
;; 2006/03/10 dadams
;;     icicle-save-or-restore-input: Bug fix (thx to Toby Cubitt) - Not relative to default dir.
;;       Use directory-file-name, so don't include /.
;;       Use file-name-nondirectory, not file-relative-name if not cycling into subdirs.
;;     Renamed icicle-minibuffer-contents to icicle-minibuffer-contents-from-minibuffer.
;;     Added new icicle-minibuffer-contents, which can be called outside minibuffer.
;; 2006/03/08 dadams
;;     icicle-place-overlay: Use new face, icicle-current-candidate-highlight.
;; 2006/03/05 dadams
;;     Bound icicle-toggle-incremental-completion to C-^ in minibuffer.
;;     Updated icicle-completion-help-string with C-^ binding.
;;     icicle-display-candidates-in-Completions:
;;       Allow for on-the-fly changes to icicle-incremental-completion-flag.
;; 2006/03/01 dadams
;;     Added: icicle-clear-minibuffer.  Use in icicle-next-candidate.
;; 2006/02/27 dadams 
;;     icicle-call-then-update-Completions: Set last-command to fn arg.
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

(eval-when-compile (require 'cl)) ;; case
                                  ;; plus, for Emacs < 21: dolist, push, pop
                                  ;; plus, for Emacs < 20: when, unless

(eval-when-compile
 (require 'icicles-face)
 (require 'icicles-opt)
 (require 'icicles-var)
 (require 'icicles-mac)
 (require 'icicles-cmd))

;; Byte-compiling this file, you will likely get some error or warning
;; messages. All of the following are benign.  They are due to
;; differences between different versions of Emacs.
;;
;; Compiling in Emacs 22:
;;
;; Warning: `directory-sep-char' is an obsolete variable (as of Emacs 21.1); do not use it.
;; Warning: `make-local-hook' is an obsolete function (as of Emacs 21.1); not necessary any more.
;;
;; Compiling in Emacs 20:
;;
;; The following functions are not known to be defined:
;;     minibufferp, minibuffer-prompt-end, field-string, minibuffer-contents,
;;     display-mouse-p, propertize, dabbrev--reset-global-variables,
;;     dabbrev--abbrev-at-point, dabbrev--minibuffer-origin,
;;     dabbrev--find-all-expansions, dabbrev--substitute-expansion,
;;     face-spec-reset-face, set-face-attribute,
;;     minibuffer-contents-no-properties


;;; Defvars to quiet byte-compilers (Emacs 20 - 22)

(defvar directory-sep-char)
(defvar partial-completion-mode)
(defvar completion-root-regexp)
(defvar minibuffer-prompt-properties)
(defvar minibuffer-local-filename-completion-map)
(defvar dabbrev-case-fold-search)
(defvar dabbrev-upcase-means-case-search)
(defvar dabbrev--last-obarray)
(defvar dabbrev--last-completion-buffer)
(defvar dabbrev--last-abbreviation)
(defvar dabbrev--check-other-buffers)
(defvar dabbrev-case-replace)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Noninteractive Functions -------------------------------


;;; Redefined standard functions............................


;;; REPLACE ORIGINAL `choose-completion-string' in `simple.el', 
;;; saving it for restoration when you toggle `icicle-mode'.
;;; 
;;; Don't exit minibuffer if this is just a `lisp-complete-symbol' completion.
;;; Free variable `completion-reference-buffer' is defined in `simple.el'.
;;;
(or (fboundp 'old-choose-completion-string)
(fset 'old-choose-completion-string (symbol-function 'choose-completion-string)))

;;;###autoload
(when (< emacs-major-version 22)
  (defun icicle-choose-completion-string (choice &optional buffer base-size)
    "Switch to BUFFER and insert the completion choice CHOICE.
BASE-SIZE, if non-nil, says how many characters of BUFFER's text
to keep.  If it is nil, we call `choose-completion-delete-max-match'
to decide what to delete.
If BUFFER is the minibuffer, then exit the minibuffer, unless one of
the following is true:
   - it is reading a file name and CHOICE is a directory
   - `completion-no-auto-exit' is non-nil
   - this is just a `lisp-complete-symbol' completion."
    (let ((buffer (or buffer completion-reference-buffer))) ; In `simple.el'.
      ;; If BUFFER is a minibuffer, barf unless it's currently active.
      (when (and (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))
                 (or (not (active-minibuffer-window))
                     (not (equal buffer (window-buffer (active-minibuffer-window))))))
        (error "Minibuffer is not active for completion"))
      ;; Insert the completion into the buffer where completion was requested.
      (set-buffer buffer)
      (if base-size
          (delete-region (+ base-size (point-min)) (point))
        (choose-completion-delete-max-match choice))
      (insert choice)
      (remove-text-properties (- (point) (length choice)) (point) '(mouse-face nil))
      ;; Update point in the window that BUFFER is showing in.
      (let ((window (get-buffer-window buffer t)))
        (set-window-point window (point)))
      ;; If completing for the minibuffer, exit it with this choice,
      ;; unless this was a `lisp-complete-symbol' completion.
      (and (not completion-no-auto-exit)
           (equal buffer (window-buffer (minibuffer-window)))
           minibuffer-completion-table
           (not (eq 'lisp-complete-symbol icicle-cmd-calling-for-completion))
           ;; If this is reading a file name, and the file name chosen
           ;; is a directory, don't exit the minibuffer.
           (if (and (eq minibuffer-completion-table 'read-file-name-internal)
                    (file-directory-p (buffer-string)))
               (select-window (active-minibuffer-window))
             (exit-minibuffer))))))

;;;###autoload
(when (>= emacs-major-version 22)
  (defun icicle-choose-completion-string (choice &optional buffer base-size)
    "Switch to BUFFER and insert the completion choice CHOICE.
BASE-SIZE, if non-nil, says how many characters of BUFFER's text
to keep.  If it is nil, we call `choose-completion-delete-max-match'
to decide what to delete.
If BUFFER is the minibuffer, then exit the minibuffer, unless one of
the following is true:
   - it is reading a file name and CHOICE is a directory
   - `completion-no-auto-exit' is non-nil
   - this is just a `lisp-complete-symbol' completion."
    (let* ((buffer (or buffer completion-reference-buffer)) ; In `simple.el'.
           (mini-p (minibufferp buffer)))
      ;; If BUFFER is a minibuffer, barf unless it's the currently
      ;; active minibuffer.
    (if (and mini-p
             (or (not (active-minibuffer-window))
                 (not (equal buffer (window-buffer (active-minibuffer-window))))))
        (error "Minibuffer is not active for completion")
      ;; Set buffer so buffer-local choose-completion-string-functions works.
      (set-buffer buffer)
      (unless (run-hook-with-args-until-success 'choose-completion-string-functions
                                                choice buffer mini-p base-size)
      ;; Insert the completion into the buffer where completion was requested.
      (if base-size
          (delete-region (+ base-size (if mini-p (minibuffer-prompt-end) (point-min))) (point))
        (choose-completion-delete-max-match choice))
      (insert choice)
      (remove-text-properties (- (point) (length choice)) (point) '(mouse-face nil))
      ;; Update point in the window that BUFFER is showing in.
      (let ((window (get-buffer-window buffer t)))
        (set-window-point window (point)))
      ;; If completing for the minibuffer, exit it with this choice,
      ;; unless this was a `lisp-complete-symbol' completion.
      (and (not completion-no-auto-exit)
           (equal buffer (window-buffer (minibuffer-window)))
           minibuffer-completion-table
           (not (eq 'lisp-complete-symbol icicle-cmd-calling-for-completion))
           ;; If this is reading a file name, and the file name chosen
           ;; is a directory, don't exit the minibuffer.
           (if (and (eq minibuffer-completion-table 'read-file-name-internal)
                    (file-directory-p (field-string (point-max))))
                 (let ((mini (active-minibuffer-window)))
                   (select-window mini)
                   (when minibuffer-auto-raise (raise-frame (window-frame mini))))
             (exit-minibuffer))))))))



;;; REPLACE ORIGINAL `completion-setup-function' in `simple.el', 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; 1. Put faces on inserted strings.  2. Help on help.
;;;
(or (fboundp 'old-completion-setup-function)
(fset 'old-completion-setup-function (symbol-function 'completion-setup-function)))

;;;###autoload
(when (< emacs-major-version 22)
  (defun icicle-completion-setup-function ()
    "Set up for completion.  This goes in `completion-setup-hook'
so it is called after completion-list buffer text is written.
Put faces on inserted string(s). Provide help on help."
    (save-excursion
      (let* ((mainbuf (current-buffer))
             (instruction1 (if window-system         ; We have a mouse.
                               (substitute-command-keys
                                "Click \\<completion-list-mode-map>\
\\[mouse-choose-completion] on a completion to select it.  ")
                             (substitute-command-keys ; No mouse.
                              "In this buffer, type \\<completion-list-mode-map>\
\\[choose-completion] to select the completion near point.  ")))
             (instruction2
              (and icicle-mode
                   (substitute-command-keys
                    "(\\<minibuffer-local-completion-map>\\[icicle-completion-help]: \
help) "))))
        (set-buffer standard-output)
        (completion-list-mode)
        (make-local-variable 'completion-reference-buffer)
        (setq completion-reference-buffer mainbuf)
        (if (eq minibuffer-completion-table 'read-file-name-internal)
            ;; For file name completion,
            ;; use the number of chars before the start of the
            ;; last file name component.
            (setq completion-base-size (save-excursion
                                         (set-buffer mainbuf)
                                         (goto-char (point-max))
                                         (skip-chars-backward (format "^%c" directory-sep-char))
                                         (- (point) (point-min))))
          ;; Otherwise, in minibuffer, the whole input is being completed.
          (save-match-data
            (if (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name mainbuf))
                (setq completion-base-size 0))))
        (goto-char (point-min))
        (put-text-property 0 (length instruction1) 'face 'icicle-Completions-instruction-1
                           instruction1)
        (when instruction2
          (put-text-property 0 (length instruction2) 'face 'icicle-Completions-instruction-2
                             instruction2))
        (insert (concat instruction1 instruction2 "\n\n"))
        (forward-line 1)
        (while (re-search-forward "[^ \t\n]+\\( [^ \t\n]+\\)*" nil t)
          (let ((beg (match-beginning 0))
                (end (point)))
;;;Emacs20 (when completion-fixup-function (funcall completion-fixup-function))
            (put-text-property beg (point) 'mouse-face 'highlight)
            (goto-char end)))))))

;;;###autoload
(when (>= emacs-major-version 22)
  (defun icicle-completion-setup-function ()
    "Set up for completion.  This goes in `completion-setup-hook'
so it is called after completion-list buffer text is written.
Put faces on inserted string(s). Provide help on help."
    (save-excursion
      (let* ((mainbuf (current-buffer))
             (mbuf-contents (minibuffer-contents))
             (instruction1 (if (display-mouse-p)     ; We have a mouse.
                               (substitute-command-keys
                                "Click \\<completion-list-mode-map>\\[mouse-choose-completion] \
or type \\[choose-completion] on a completion to select it.  ")
                             (substitute-command-keys ; No mouse.
                              "In this buffer, type \\<completion-list-mode-map>\
\\[choose-completion] to select the completion near point.  ")))
             (instruction2
              (and icicle-mode
                   (substitute-command-keys
                    "(\\<minibuffer-local-completion-map>\\[icicle-completion-help]: \
help) "))))
        ;; When reading a file name in the minibuffer,
        ;; set default-directory in the minibuffer
        ;; so it will get copied into the completion list buffer.
        (if minibuffer-completing-file-name
            (with-current-buffer mainbuf
              (setq default-directory (file-name-directory mbuf-contents))))
        ;; If partial-completion-mode is on, point might not be after the
        ;; last character in the minibuffer.
        ;; FIXME: This still doesn't work if the text to be completed
        ;; starts with a `-'.
        (when (and partial-completion-mode (not (eobp)))
          (setq mbuf-contents
                (substring mbuf-contents 0 (- (point) (point-max)))))
        (with-current-buffer standard-output
          (completion-list-mode)
          (make-local-variable 'completion-reference-buffer)
          (setq completion-reference-buffer mainbuf)
          (if minibuffer-completing-file-name
              ;; For file name completion,
              ;; use the number of chars before the start of the
              ;; last file name component.
              (setq completion-base-size
                    (with-current-buffer mainbuf
                      (save-excursion
                        (goto-char (point-max))
                        (skip-chars-backward completion-root-regexp)
                        (- (point) (minibuffer-prompt-end)))))
            ;; Otherwise, in minibuffer, the whole input is being completed.
            (if (minibufferp mainbuf) (setq completion-base-size 0)))
          ;; Put faces on first uncommon characters and common parts.
          (when completion-base-size
            (let* ((common-string-length
                    (- (length mbuf-contents) completion-base-size))
                   (element-start (next-single-property-change (point-min) 'mouse-face))
                   (element-common-end
                    (and element-start (+ (or element-start nil) common-string-length)))
                   (maxp (point-max)))
              (while (and element-start (< element-common-end maxp))
                (when (and (get-char-property element-start 'mouse-face)
                           (get-char-property element-common-end 'mouse-face))
                  (put-text-property element-start element-common-end
                                     'font-lock-face 'completions-common-part)
                  (put-text-property element-common-end (1+ element-common-end)
                                     'font-lock-face 'completions-first-difference))
                (setq element-start (next-single-property-change element-start 'mouse-face))
                (if element-start
                    (setq element-common-end  (+ element-start common-string-length))))))
          ;; Insert help string.
          (goto-char (point-min))
          (put-text-property 0 (length instruction1)
                             'face 'icicle-Completions-instruction-1 instruction1)
          (when instruction2
            (put-text-property 0 (length instruction2)
                               'face 'icicle-Completions-instruction-2 instruction2))
          (insert (concat instruction1 instruction2 "\n\n")))))))



;;; REPLACE ORIGINAL `completing-read' (built-in function), 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Allows for completions that are lists of strings.
;;; Appends `icicle-prompt-suffix' if resulting prompt is not too long.
;;; Removes *Completions* window.
;;;
(or (fboundp 'old-completing-read)
(fset 'old-completing-read (symbol-function 'completing-read)))

;;;###autoload
(defun icicle-completing-read
    (prompt table &optional predicate require-match initial-input hist def inherit-input-method)
  "Read string in minibuffer, with completion and cycling of completions.
Type `\\[exit-minibuffer]' to end your input.

Prefix completion via \\<minibuffer-local-completion-map>\
`\\[icicle-prefix-word-complete]' (word) and `\\[icicle-prefix-complete]' (full).
Apropos (regexp) completion via `\\[icicle-apropos-complete]'.

Prefix cycling of candidate completions via `\\[icicle-previous-prefix-candidate]' and \
`\\[icicle-next-prefix-candidate]'.
Apropos cycling of candidate completions via `\\[icicle-previous-apropos-candidate]' and \
`\\[icicle-next-apropos-candidate]'.

Cycling of past minibuffer inputs via `\\[previous-history-element]' and \
`\\[next-history-element]'.
Searching through input history via `\\[previous-matching-history-element]' \
and `\\[next-matching-history-element]'.

Case is ignored if `completion-ignore-case' is non-nil.  For file-name
  completion, `read-file-name-completion-ignore-case' is used instead.
For file-name completion, cycling into subdirectories is determined by
  `icicle-cycle-into-subdirs-flag'.
Position of the cursor (point) and the mark during completion cycling
  is determined by `icicle-point-position-in-candidate' and
  `icicle-mark-position-in-candidate', respectively.
Highlighting of the matched part of completion candidates during
  cycling is determined by `icicle-match-highlight-minibuffer',
  `icicle-match-highlight-Completions', and
  `icicle-common-match-highlight-Completions'.

Use `\\[icicle-completion-help]' during completion for more information on completion and key
bindings in Icicle mode.

Args: PROMPT, TABLE, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST:

PROMPT is a string to prompt with; normally ends in a colon and space.

TABLE is an alist whose elements' cars are strings, or an obarray.

PREDICATE limits completion to a subset of TABLE.
See `try-completion' and `all-completions' for more details on
completion, TABLE, PREDICATE.

If REQUIRE-MATCH is non-nil, you are not allowed to exit unless the
input is (or completes to) an element of TABLE or is null.  If it is
also not `t', `\\[exit-minibuffer]' doesn't exit if it effects non-null
completion.  If the input is null, `completing-read' returns an empty
string, regardless of the value of REQUIRE-MATCH.

If option `icicle-require-match-flag' is non-nil, it overrides the
value of REQUIRE-MATCH.

If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
If it is (STRING . POSITION), the initial input is STRING, but point
is placed POSITION characters into the string.

HIST, if non-nil, specifies a history list, and optionally the initial
position in the list.  It can be a symbol, which is the history list
variable to use, or it can be a cons cell (HISTVAR . HISTPOS).  In
that case, HISTVAR is the history list variable to use, and HISTPOS is
the initial position (the position in the list which INITIAL-INPUT
corresponds to).  Positions are counted starting from 1 at the
beginning of the list.

DEF, if non-nil, is the default value.

Non-nil `icicle-init-value-flag' means that when DEF is non-nil and
INITIAL-INPUT is nil or \"\", DEF is inserted in the minibuffer as the
INITIAL-INPUT.  The particular non-nil value determines whether or not
the value is preselected and, if preselected, where the cursor is left
\(at the beginning or end of the value).

If INHERIT-INPUT-METHOD is non-nil, the minibuffer inherits the
current input method and the setting of `enable-multibyte-characters'.

Completion ignores case when`completion-ignore-case' is non-nil."
  (unless initial-input (setq initial-input ""))
  (if (consp initial-input)
      (setq icicle-initial-value (car initial-input))
    (setq initial-input        (format "%s" initial-input) ; Convert symbol to string
          icicle-initial-value (or initial-input "")))
  (setq icicle-nb-of-other-cycle-candidates 0)

  ;; Maybe use DEF for INITIAL-INPUT also.
  (when (and icicle-init-value-flag def (stringp initial-input) (string= "" initial-input))
    (setq initial-input def))

  ;; Override REQUIRE-MATCH as needed.
  (setq require-match (case icicle-require-match-flag
                        ((nil) require-match)
                        (no-match-required nil)
                        (partial-match-ok t)
                        (full-match-required 'full-match-required)))
  (let ((minibuffer-completion-table table)
        result)

    ;; Extension: candidate is a list of strings.  Used for multi-completion.
    (when (and (consp table) (consp (car table)) (consp (caar table)))
      (setq minibuffer-completion-table
            (setq table
                  (mapcar
                   (lambda (entry) 
                     (cons (concat (mapconcat #'identity (car entry) icicle-list-join-string)
                                   icicle-list-join-string)
                           (cdr entry)))
                   table))))

    ;; Append suffix if prompt is not too long.
    ;; Use face on suffix if (boundp 'minibuffer-prompt-properties).
    (cond ((not icicle-mode)
           (setq icicle-prompt prompt)  ; No room to add suffix.
           (setq result (old-completing-read icicle-prompt table predicate require-match
                                             initial-input hist def inherit-input-method)))
          ((or icicle-inhibit-reminder-prompt-flag
               (> (length icicle-initial-value)
                  (- (window-width (minibuffer-window)) (length prompt))))
           (setq icicle-prompt prompt)  ; No room to add suffix.
           (setq result (catch 'icicle-read-top
                          (old-completing-read icicle-prompt table predicate require-match
                                               initial-input hist def inherit-input-method))))
          (t                            ; Append suffix to prompt.
           (setq icicle-prompt
                 (if (fboundp 'propertize)
                     (concat (propertize prompt 'face 'minibuffer-prompt)
                             (propertize icicle-prompt-suffix 'face 'icicle-prompt-suffix)
                             "  ")
                   (concat prompt icicle-prompt-suffix "  ")))
           (let ((minibuffer-prompt-properties
                  (and (boundp 'minibuffer-prompt-properties) ; Emacs 21+ only
                       (icicle-remove-property 'face minibuffer-prompt-properties))))
             (setq result
                   (catch 'icicle-read-top
                     (old-completing-read icicle-prompt table predicate require-match
                                          initial-input hist def inherit-input-method))))))
    ;; HACK.  Without this, when REQUIRE-MATCH is non-nil, *Completions* window
    ;; does not disappear.
    (when require-match (icicle-delete-windows-on "*Completions*"))
    result))



;;; REPLACE ORIGINAL `read-file-name' (built-in function), 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Appends `icicle-prompt-suffix' if resulting prompt is not too long.
;;; Removes *Completions* window.
;;;
(or (fboundp 'old-read-file-name)
(fset 'old-read-file-name (symbol-function 'read-file-name)))

;;;###autoload
(defun icicle-read-file-name (prompt &optional dir default-filename
                              require-match initial-input predicate)
  "Read file name, prompting with prompt and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself.
Default the name to DEFAULT-FILENAME if user exits the minibuffer with
the same non-empty string that was inserted by this function.
 (If DEFAULT-FILENAME is omitted, the visited file name is used,
  but if INITIAL-INPUT is specified, that combined with DIR is used.)
If the user exits with an empty minibuffer, this function returns
an empty string.  (This can only happen if the user erased the
pre-inserted contents or if `insert-default-directory' is nil.)
Fourth arg REQUIRE-MATCH non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL-INPUT specifies text to start with.
If optional sixth arg PREDICATE is non-nil, possible completions and
 the resulting file name must satisfy `(funcall predicate NAME)'.
 This argument is only available starting with Emacs 21.
DIR should be an absolute directory name.  It defaults to the value of
`default-directory'.

Non-nil `icicle-init-value-flag' means that when DEFAULT-FILENAME is
non-nil and INITIAL-INPUT is nil or \"\", DEFAULT-FILENAME is inserted
in the minibuffer as the INITIAL-INPUT.  The particular non-nil value
determines whether or not the value is preselected and, if
preselected, where the cursor is left \(at the beginning or end of the
value).

If option `icicle-require-match-flag' is non-nil, it overrides the
value of REQUIRE-MATCH.

If this command was invoked with the mouse, use a file dialog box if
`use-dialog-box' is non-nil, and the window system or X toolkit in use
provides a file dialog box.

Removes *Completions* window when done.

See also `read-file-name-completion-ignore-case'
and `read-file-name-function'."
  (setq icicle-initial-value                  (or initial-input "")
        icicle-nb-of-other-cycle-candidates   0)
  (icicle-fix-default-directory)        ; Make sure there are no backslashes in it.

  ;; Maybe use DEFAULT-FILENAME for INITIAL-INPUT also, after removing the directory part.
  ;; Note that if DEFAULT-FILENAME is null, then we let INITIAL-INPUT remain null too.
  (when (and icicle-init-value-flag default-filename (string= "" icicle-initial-value))
    (setq initial-input (file-name-nondirectory default-filename)))

  ;; Override REQUIRE-MATCH as needed.
  (setq require-match (case icicle-require-match-flag
                        ((nil) require-match)
                        (no-match-required nil)
                        (partial-match-ok t)
                        (full-match-required 'full-match-required)))
  (let (result)
    ;; Append suffix if prompt is not too long.
    ;; Use face on suffix if (boundp 'minibuffer-prompt-properties).
    (cond ((not icicle-mode)
           (setq icicle-prompt prompt)  ; No room to add suffix.
           (condition-case nil          ; If Emacs 22+, use predicate arg.
               (setq result (old-read-file-name icicle-prompt dir default-filename
                                                require-match initial-input predicate))
             (wrong-number-of-arguments
              (setq result (old-read-file-name icicle-prompt dir default-filename
                                               require-match initial-input)))))
          ((or icicle-inhibit-reminder-prompt-flag
               (> (length initial-input)
                  (- (window-width (minibuffer-window)) (length prompt))))
           (setq icicle-prompt prompt)  ; No room to add suffix.
           (condition-case nil          ; If Emacs 22+, use predicate arg.
               (setq result
                     (catch 'icicle-read-top
                       (old-read-file-name icicle-prompt dir default-filename
                                           require-match initial-input predicate)))
             (wrong-number-of-arguments
              (setq result
                    (catch 'icicle-read-top
                      (old-read-file-name icicle-prompt dir default-filename
                                          require-match initial-input))))))
          (t                            ; Append suffix to prompt.
           (setq icicle-prompt
                 (if (fboundp 'propertize)
                     (concat (propertize prompt 'face 'minibuffer-prompt)
                             (propertize icicle-prompt-suffix 'face 'icicle-prompt-suffix)
                             "  ")
                   (concat prompt icicle-prompt-suffix "  ")))
           (let ((minibuffer-prompt-properties ; If Emacs 22+, use pred and suffix face.
                  (and (boundp 'minibuffer-prompt-properties)
                       (icicle-remove-property 'face minibuffer-prompt-properties))))
             (condition-case nil
                 (setq result
                       (catch 'icicle-read-top
                         (old-read-file-name icicle-prompt dir default-filename
                                             require-match initial-input predicate)))
               (wrong-number-of-arguments
                (setq result
                      (catch 'icicle-read-top
                        (old-read-file-name icicle-prompt dir default-filename
                                            require-match initial-input))))))))
    ;; HACK.  Without this, when REQUIRE-MATCH is non-nil, *Completions* window
    ;; does not disappear.
    (when require-match (icicle-delete-windows-on "*Completions*"))
    result))

(defun icicle-fix-default-directory ()
  "Convert backslashes in `default-directory' to slashes."
;; This is a hack.  If you do `C-x 4 f' from a standalone minibuffer
;; frame, `default-directory' on MS Windows has this form:
;; `C:\some-dir/'.  There is a backslash character in the string.  This
;; is not a problem for standard Emacs, but it is a problem for Icicles,
;; because we interpret backslashes using regexp syntax - they are not
;; file separators for Icicles.  So, we call `substitute-in-file-name' to
;; change all backslashes in `default-directory' to slashes.  This
;; shouldn't hurt, because `default-directory' is an absolute directory
;; name - it doesn't contain environment variables.  For example, we
;; convert `C:\some-dir/' to `c:/some-directory/'."
  (setq default-directory (substitute-in-file-name default-directory)))


(defun icicle-remove-property (prop plist)
  "Remove propery PROP from property-list PLIST, non-destructively."
  (let ((cpy plist)
        (result nil))
    (while cpy
      (unless (eq prop (car cpy)) (setq result `(,(cadr cpy) ,(car cpy) ,@result)))
      (setq cpy (cddr cpy)))
    (nreverse result)))



;;; REPLACE ORIGINAL `read-from-minibuffer' (built-in function), 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Respect `icicle-init-value-flag'.
;;;
(or (fboundp 'old-read-from-minibuffer)
(fset 'old-read-from-minibuffer (symbol-function 'read-from-minibuffer)))

;;;###autoload
(defun icicle-read-from-minibuffer (prompt &optional initial-contents keymap read hist
                                    default-value inherit-input-method keep-all)
  "Read a string from the minibuffer, prompting with string PROMPT.
The optional second arg INITIAL-CONTENTS is an alternative to
  DEFAULT-VALUE.  Vanilla Emacs considers it to be obsolete, but
  Icicles does not.  It is discussed in more detail below.
Third arg KEYMAP is a keymap to use while reading;
  if omitted or nil, the default is `minibuffer-local-map'.
If fourth arg READ is non-nil, then interpret the result as a Lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'
Fifth arg HIST, if non-nil, specifies a history list and optionally
  the initial position in the list.  It can be a symbol, which is the
  history list variable to use, or it can be a cons cell
  (HISTVAR . HISTPOS).  In that case, HISTVAR is the history list variable
  to use, and HISTPOS is the initial position for use by the minibuffer
  history commands.  For consistency, you should also specify that
  element of the history as the value of INITIAL-CONTENTS.  Positions
  are counted starting from 1 at the beginning of the list.
Sixth arg DEFAULT-VALUE is the default value.  If non-nil, it is available
  for history commands; but, unless READ is non-nil, `read-from-minibuffer'
  does NOT return DEFAULT-VALUE if the user enters empty input!  It returns
  the empty string.
Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of `enable-multibyte-characters'.
Eighth arg KEEP-ALL, if non-nil, says to put all inputs in the history list,
 even empty or duplicate inputs.  This is available starting with Emacs 22.
If the variable `minibuffer-allow-text-properties' is non-nil,
 then the string which is returned includes whatever text properties
 were present in the minibuffer.  Otherwise the value has no text properties.

Non-nil `icicle-init-value-flag' means that when DEFAULT-VALUE is
non-nil and INITIAL-CONTENTS is nil or \"\", DEFAULT-VALUE is inserted
in the minibuffer as the INITIAL-CONTENTS.  The particular non-nil
value determines whether or not the value is preselected and, if
preselected, where the cursor is left \(at the beginning or end of the
value).

The remainder of this documentation string describes the
INITIAL-CONTENTS argument in more detail.  If non-nil,
INITIAL-CONTENTS is a string to be inserted into the minibuffer before
reading input.  Normally, point is put at the end of that string.
However, if INITIAL-CONTENTS is (STRING . POSITION), the initial input
is STRING, but point is placed at _one-indexed_ position POSITION in
the minibuffer.  Any integer value less than or equal to one puts
point at the beginning of the string.  *Note* that this behavior
differs from the way such arguments are used in `completing-read' and
some related functions, which use zero-indexing for POSITION."
  (unless initial-contents (setq initial-contents "")) 
  ;; Maybe use DEFAULT-VALUE for INITIAL-CONTENTS also.
  (when (and icicle-init-value-flag default-value (stringp initial-contents)
             (string= "" initial-contents))
    (setq initial-contents default-value))
  (if (< emacs-major-version 22)
      (old-read-from-minibuffer prompt initial-contents keymap read hist
                                default-value inherit-input-method)
    (old-read-from-minibuffer prompt initial-contents keymap read hist
                              default-value inherit-input-method keep-all)))



;;; REPLACE ORIGINAL `read-string' (built-in function), 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Respect `icicle-init-value-flag'.
;;;
(or (fboundp 'old-read-string)
(fset 'old-read-string (symbol-function 'read-string)))

;;;###autoload
(defun icicle-read-string (prompt &optional initial-input history
                           default-value inherit-input-method)
  "Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
  Vanilla Emacs considers it to be obsolete, but Icicles does not.  It
  behaves as in `read-from-minibuffer'.  See the documentation string
  of `read-from-minibuffer' for details.
The third arg HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
  See `read-from-minibuffer' for details of HISTORY argument.
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
 for history commands, and as the value to return if the user enters
 the empty string.
Fifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of enable-multibyte-characters."
  (let ((value (read-from-minibuffer prompt initial-input nil nil
                                     history default-value inherit-input-method)))
    (if (and default-value (equal value "")) default-value value)))




;;; Icicle functions - completion display (not cycling).....

(defun icicle-display-candidates-in-Completions (&optional reverse-p no-display-p)
  "Refresh the current set of completion candidates in *Completions*.
REVERSE-P non-nil means display the candidates in reverse order.
NO-DISPLAY-P means do not display the candidates; just recompute them."
  ;; Pred is special if `minibuffer-completion-table' is a function.
  (when (and (not (functionp minibuffer-completion-table))
             (functionp minibuffer-completion-predicate))
    (setq icicle-completion-candidates
          (icicle-delete-if-not
           (lambda (cand)
             (funcall minibuffer-completion-predicate
                      (if (arrayp minibuffer-completion-table) (intern cand) (list cand))))
           icicle-completion-candidates)))
  (case icicle-incremental-completion-flag
    ((t always) (setq icicle-incremental-completion-p 'always))
    ((nil) (setq icicle-incremental-completion-p nil)))
  (unless no-display-p
    (message "Displaying completion candidates...")
    (with-output-to-temp-buffer "*Completions*"
      ;; `condition-case' shouldn't be needed, but it prevents an "End of buffer"
      ;; message from `display-completion-list' on Emacs 22.
      (condition-case nil
          (display-completion-list (if reverse-p
                                       (reverse icicle-completion-candidates)
                                     icicle-completion-candidates))
        (error nil)))
    (save-excursion
      (save-window-excursion
        (set-buffer (get-buffer "*Completions*"))
        (let ((buffer-read-only nil)
              (eob (point-max))
              (case-fold-search completion-ignore-case))
          (goto-char (point-min))
          (forward-line 2)
          (while (not (eobp))
            (let ((beg (goto-char (next-single-property-change (point) 'mouse-face nil eob)))
                  (end (goto-char (next-single-property-change (point) 'mouse-face nil eob))))
              (goto-char beg)

              ;; Highlight whole candidate if it has been used previously.
              (when (and (symbolp minibuffer-history-variable)
                         (consp (symbol-value minibuffer-history-variable))
                         (member (icicle-current-completion-in-Completions)
                                 (symbol-value minibuffer-history-variable)))
                (put-text-property
                 (point) (next-single-property-change (point) 'mouse-face nil end)
                 'face 'icicle-historical-candidate))

              ;; Highlight, inside the candidate, the longest common match.
              (when (and icicle-expand-input-to-common-match-flag (not (string= "" icicle-current-input)))
                (save-excursion
                  (save-restriction
                    (narrow-to-region beg end) ; Search within the completion candidate.
                    (when (re-search-forward
                           (regexp-quote (if (icicle-file-name-input-p)
                                             (icicle-file-name-nondirectory icicle-current-input)
                                           icicle-current-input))
                           nil t)
                      (put-text-property (match-beginning 0) (point)
                                         'face 'icicle-common-match-highlight-Completions)))))
              ;; Highlight, inside the candidate, what the input expression matches.
              (unless (string= "" icicle-current-regexp-input)
                (save-excursion
                  (save-restriction
                    (narrow-to-region beg end) ; Search within the completion candidate.
                    (when (re-search-forward (if (icicle-file-name-input-p)
                                                 (icicle-file-name-nondirectory
                                                  icicle-current-regexp-input)
                                               icicle-current-regexp-input)
                                             nil t)
                      (put-text-property (match-beginning 0) (point)
                                         'face 'icicle-match-highlight-Completions)))))
              (goto-char end))))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)))
    (message nil)))                     ; Clear out any "Looking for..."

(defun icicle-place-cursor (input)
  "Position point and mark with respect to the minibuffer candidate.
Positions are `icicle-point-position-in-candidate' and
`icicle-mark-position-in-candidate', respectively.
INPUT is the current user input, that is, the completion root."
  (let ((case-fold-search completion-ignore-case)
        input-start-position)
    (goto-char (icicle-minibuffer-prompt-end))
    (setq input-start-position (point))
    (when (and (icicle-file-name-input-p) insert-default-directory)
      (search-forward (icicle-file-name-directory-w-default input))
      (setq input-start-position (point))) ; Skip directory.
    ;; Locate completion root within current completion candidate.
    (when (or (memq icicle-point-position-in-candidate '(root-start root-end))
              (memq icicle-mark-position-in-candidate  '(root-start root-end)))
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (point-max)) ; Search within the completion candidate.
          (re-search-forward (if (icicle-file-name-input-p)
                                 (icicle-file-name-nondirectory input)
                               input)
                             nil t))))
    ;; Position point.
    (case icicle-point-position-in-candidate
      (input-start (goto-char input-start-position))
      (input-end (goto-char (point-max)))
      (root-start (goto-char (match-beginning 0)))
      (root-end (goto-char (match-end 0))))
    ;; Position mark.
    (unless (eq icicle-point-position-in-candidate icicle-mark-position-in-candidate)
      (push-mark (case icicle-mark-position-in-candidate
                   (input-start input-start-position)
                   (input-end (point-max))
                   (root-start (match-beginning 0))
                   (root-end (match-end 0)))
                 'nomsg
                 'activate-mark))))

(defun icicle-minibuffer-prompt-end ()
  "Version of `minibuffer-prompt-end' that works for Emacs 20 and later."
  (if (fboundp 'minibuffer-prompt-end) (minibuffer-prompt-end) (point-min)))




;;; Icicles functions - Icicle mode..........................

(defun icicle-rebind-completion-maps (turn-on-p)
  "Rebind minibuffer completion maps to be able to cycle completions.
Also, update the bindings in the minibuffer-completion help variables.

This is called by `icicle-mode'.  When in Icicle mode, all keys that
are globally bound to `next-line' are rebound in the minibuffer to
`icicle-next-prefix-candidate', for minibuffer completion purposes.
Similarly for other keys."
  (cond
    (turn-on-p                          ; TURN IT ON ********************************

     ;; `minibuffer-local-map': default minibuffer map.
     (if (> emacs-major-version 21)
         (define-key minibuffer-local-map [menu-bar minibuf quit]
           (list 'menu-item "Quit" 'icicle-abort-minibuffer-input
                 :help "Abort input and exit minibuffer"))
       (define-key minibuffer-local-map [menu-bar minibuf quit]
         (cons "Quit" 'icicle-abort-minibuffer-input)))
     (define-key minibuffer-local-map [(control ?g)]  'icicle-abort-minibuffer-input)
     (define-key minibuffer-local-map [M-S-backspace] 'icicle-erase-minibuffer)
     (define-key minibuffer-local-map [M-S-delete]    'icicle-erase-minibuffer)
     (define-key minibuffer-local-map [(meta ?.)]     'icicle-insert-string-at-point)
     (define-key minibuffer-local-map [(control ?=)]  'icicle-insert-string-from-variable)

     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     (define-key minibuffer-local-ns-map [(control ?g)]  'icicle-abort-minibuffer-input)
     (define-key minibuffer-local-ns-map [M-S-backspace] 'icicle-erase-minibuffer)
     (define-key minibuffer-local-ns-map [M-S-delete]    'icicle-erase-minibuffer)
     (define-key minibuffer-local-ns-map [(meta ?.)]     'icicle-insert-string-at-point)
     (define-key minibuffer-local-ns-map [(control ?=)]  'icicle-insert-string-from-variable)

     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     (define-key minibuffer-local-isearch-map [(control ?g)]  'icicle-abort-minibuffer-input)
     (define-key minibuffer-local-isearch-map [M-S-backspace] 'icicle-erase-minibuffer)
     (define-key minibuffer-local-isearch-map [M-S-delete]    'icicle-erase-minibuffer)
     (define-key minibuffer-local-isearch-map [(meta ?.)]     'icicle-insert-string-at-point)
     (define-key minibuffer-local-isearch-map [(control ?=)]  'icicle-insert-string-from-variable)

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-bind-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-filename-completion-map': file-name completion map (Emacs 22).
     (when (boundp 'minibuffer-local-filename-completion-map)
       (icicle-bind-completion-keys minibuffer-local-filename-completion-map))

     ;; `minibuffer-local-must-match-map': must-match map.
     (icicle-bind-completion-keys minibuffer-local-must-match-map)
     (define-key minibuffer-local-must-match-map [S-return] 'icicle-apropos-complete-and-exit)

     ;; `completion-list-mode-map': map for *Completions* buffer.
     ;; Abort on `C-g' or `q'.  Switch to minibuffer on [insert].  Do not allow normal input.
     (define-key completion-list-mode-map [(control ?g)]   'icicle-abort-minibuffer-input)
     (define-key completion-list-mode-map "q"              'icicle-abort-minibuffer-input)
     (define-key completion-list-mode-map [insert]         'icicle-switch-to-minibuffer)
     (define-key completion-list-mode-map [down]           'icicle-next-line)
     (define-key completion-list-mode-map [up]             'icicle-previous-line)
     (define-key completion-list-mode-map [S-iso-lefttab]  'icicle-move-to-previous-completion)
     (define-key completion-list-mode-map [S-tab]          'icicle-move-to-previous-completion)
     (define-key completion-list-mode-map [left]           'icicle-move-to-previous-completion)
     (define-key completion-list-mode-map [(control ?i)]   'icicle-move-to-next-completion)
     (define-key completion-list-mode-map [tab]            'icicle-move-to-next-completion)
     (define-key completion-list-mode-map [right]          'icicle-move-to-next-completion)
     (define-key completion-list-mode-map [C-down-mouse-2] 'icicle-mouse-candidate-action)
     (define-key completion-list-mode-map [C-mouse-2]      nil)
     ;; (suppress-keymap completion-list-mode-map) ; Inhibit character self-insertion.
     )


    (t                                  ; TURN IT OFF *******************************

     ;; `minibuffer-local-map': default minibuffer map.
     (if (> emacs-major-version 21)
         (define-key minibuffer-local-map [menu-bar minibuf quit]
           (list 'menu-item "Quit" 'keyboard-escape-quit
                 :help "Abort input and exit minibuffer"))
       (define-key minibuffer-local-map [menu-bar minibuf quit]
         (cons "Quit" 'keyboard-escape-quit)))
     (define-key minibuffer-local-map [(control ?g)]  'abort-recursive-edit)
     (define-key minibuffer-local-map [M-S-backspace] nil)
     (define-key minibuffer-local-map [M-S-delete]    nil)
     (define-key minibuffer-local-map [(meta ?.)]     nil)
     (define-key minibuffer-local-map [(control ?=)]  nil)

     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     (define-key minibuffer-local-ns-map [(control ?g)]  'abort-recursive-edit)
     (define-key minibuffer-local-ns-map [M-S-backspace] nil)
     (define-key minibuffer-local-ns-map [M-S-delete]    nil)
     (define-key minibuffer-local-ns-map [(meta ?.)]     nil)
     (define-key minibuffer-local-ns-map [(control ?=)]  nil)

     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     (define-key minibuffer-local-isearch-map [(control ?g)]  'abort-recursive-edit)
     (define-key minibuffer-local-isearch-map [M-S-backspace] nil)
     (define-key minibuffer-local-isearch-map [M-S-delete]    nil)
     (define-key minibuffer-local-isearch-map [(meta ?.)]     nil)
     (define-key minibuffer-local-isearch-map [(control ?=)]  nil)

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-restore-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-filename-completion-map': file-name completion map.
     (when (boundp 'minibuffer-local-filename-completion-map)
       (icicle-restore-completion-keys minibuffer-local-filename-completion-map))

     ;; `minibuffer-local-must-match-map': must-match map.
     (icicle-restore-completion-keys minibuffer-local-must-match-map)
     (define-key minibuffer-local-must-match-map [S-return] nil)

     ;; `completion-list-mode-map': map for *Completions* buffer.
     (define-key completion-list-mode-map [(control ?g)]    nil)
     (define-key completion-list-mode-map "q"               nil)
     (define-key completion-list-mode-map [insert]          nil)
     (define-key completion-list-mode-map [down]            nil)
     (define-key completion-list-mode-map [up]              nil)
     (define-key completion-list-mode-map [left]            'previous-completion)
     (define-key completion-list-mode-map [right]           'next-completion)
     (define-key completion-list-mode-map [S-iso-lefttab]   nil)
     (define-key completion-list-mode-map [S-tab]           nil)
     (define-key completion-list-mode-map [tab]             nil)
     (define-key completion-list-mode-map [(control ?i)]    nil)
     (define-key completion-list-mode-map [C-mouse-2]       nil)
     (define-key completion-list-mode-map [C-down-mouse-2]  (if (boundp 'facemenu-mouse-menu)
                                                                facemenu-mouse-menu
                                                              facemenu-menu))))

  ;; Update the bindings within the help string.
  (setq icicle-completion-help-string
        (substitute-command-keys
         "\\<minibuffer-local-completion-map>                        \
Minibuffer Completion
                        ---------------------

Minibuffer input can be completed in several ways.
These are the main actions and their key bindings.

 * Display this help.				\\[icicle-completion-help]

 * Complete the current input in the minibuffer.
        Prefix completion:
           A word at a time                     \\[icicle-prefix-word-complete]
           As much as possible                  \\[icicle-prefix-complete]
        Apropos (regexp) completion:            \\[icicle-apropos-complete]
        Match another regexp (progressive):     \\[icicle-narrow-candidates]

 * Choose a completion candidate.
	Cycle among prefix completions:		\\[icicle-next-prefix-candidate], \
\\[icicle-previous-prefix-candidate]
	Cycle among apropos completions:	\\[icicle-next-apropos-candidate], \
\\[icicle-previous-apropos-candidate]

 * Retrieve your last real input (repeat).      \\[icicle-retrieve-last-input]

 * Act on completion candidates (show help, if no action is defined).
	Current candidate:			\\[icicle-candidate-action], \
\\<completion-list-mode-map>\\[icicle-mouse-candidate-action]\\<minibuffer-local-completion-map>
	Next, previous prefix candidate:	\\[icicle-next-prefix-candidate-action], \
\\[icicle-previous-prefix-candidate-action]
	Next, previous apropos candidate:	\\[icicle-next-apropos-candidate-action], \
\\[icicle-previous-apropos-candidate-action]
        All candidates at once                  \\[icicle-all-candidates-action]
	Show help on current candidate:         \\[icicle-help-on-candidate]

 * Perform set operations on candidate sets.
        Set complement:                         \\[icicle-candidate-set-complement]
        Set difference:                         \\[icicle-candidate-set-difference]
        Set union:                              \\[icicle-candidate-set-union]
        Set intersection:                       \\[icicle-candidate-set-intersection]
        Set intersection using another regexp:  \\[icicle-narrow-candidates]
        Save current set:                       \\[icicle-candidate-set-save]
        Retrieve saved set:                     \\[icicle-candidate-set-retrieve]
        Save current set to cache file:         \\[icicle-candidate-set-save-to-cache-file]
        Retrieve saved set from cache file:     \\[icicle-candidate-set-retrieve-from-cache-file]
        Save current set to variable:           \\[icicle-candidate-set-save-to-variable]
        Retrieve saved set from variable:       \\[icicle-candidate-set-retrieve-from-variable]
        Swap current and saved sets:            \\[icicle-candidate-set-swap]
        Define current set by evalling sexpr:   \\[icicle-candidate-set-define]
        Restrict candidates to history items:   \\[icicle-keep-only-past-inputs]

 * Display completions for current input, in buffer *Completions*.
        Show completion candidates:
           Prefix completion			\\[icicle-prefix-complete] (repeat)
           Apropos completion			\\[icicle-apropos-complete]
        Move between minibuffer and list:	\\<completion-list-mode-map>\
\\[icicle-switch-to-minibuffer]
        Move among completion candidates:	\\[next-line], \\[previous-line], \
\\[icicle-move-to-next-completion], \\[icicle-move-to-previous-completion]
        Choose a completion candidate:		\\[choose-completion], \
\\[mouse-choose-completion]

 * Toggle some user options on the fly.
        Toggle ignoring certain file extensions \\<minibuffer-local-completion-map>\
\\[icicle-toggle-ignored-extensions]
        Toggle sorting completion candidates    \\[icicle-toggle-sorting]
        Toggle incremental completion           \\[icicle-toggle-incremental-completion]
        Toggle escaping of regexp special chars \\[icicle-toggle-regexp-quote]

 * Choose a previous input from the minibuffer history.
        Apropos-complete against history items: \\[icicle-history], \
\\[icicle-keep-only-past-inputs]
        Restrict candidates to history items:   \\[icicle-keep-only-past-inputs]
	Cycle among minibuffer history items:	\\[next-history-element], \
\\[previous-history-element]
	Search among minibuffer history items:	\
\\[next-matching-history-element], \\[previous-matching-history-element]

 * Manipulate your input.  You can modify it, before committing it.
        Erase (clear) input:			\\[icicle-erase-minibuffer]
        Yank thing at point into minibuffer:    \\[icicle-insert-string-at-point]
        Insert text (regexp) from a variable:   \\[icicle-insert-string-from-variable]
        Abandon input:				\\[icicle-abort-minibuffer-input]
        Send input to Emacs:			\\[exit-minibuffer]

Remember: You can always input any character that is bound to a
          command (e.g. \\[icicle-prefix-complete]) \
by preceding it with \\<global-map>\\[quoted-insert].

User options controlling minibuffer completion and cycling:

 * `completion-ignore-case', `read-file-name-completion-ignore-case'
                                          - Case sensitivity?
 * `icicle-buffer-*'                      - `icicle-buffer' options
 * `icicle-change-region-background-flag' - Change region color?
 * `icicle-color-themes'                  - For `icicle-color-theme'
 * `icicle-completion-nospace-flag'       - Ignore space at start?
 * `icicle-Completions-frame-at-right-flag'- *Completions* at right?
 * `icicle-cycle-into-subdirs-flag'       - Explore subdirectories?
 * `icicle-default-thing-insertion'       - Control behavior of \
\\<minibuffer-local-completion-map>\\[icicle-insert-string-at-point]
 * `icicle-incremental-completion-flag'   - *Completions* icompletion?
 * `icicle-inhibit-reminder-prompt-flag'  - Show reminder in prompt?
 * `icicle-init-value-flag'               - Use default as init value?
 * `icicle-list-join-string'              - Multi-completion join
 * `icicle-mark-position-in-candidate'    - Mark position in cycling
 * `icicle-minibuffer-setup-hook'         - Functions run after setup
 * `icicle-point-position-in-candidate'   - Cursor position in cycling
 * `icicle-redefine-standard-commands-flag'- Redefine std commands?
 * `icicle-region-background'             - Background for region
 * `icicle-require-match-flag'            - Override REQUIRE-MATCH?
 * `icicle-search-ring-max', `icicle-regexp-search-ring-max'
                                          - Search ring sizes
 * `icicle-show-Completions-initially-flag'- Show *Completions* first?
 * `icicle-sort-function'                 - Sort completion candidates
 * `icicle-thing-at-point-functions'      - Functions to yank things 
 * `icicle-word-completion-key'           - Keys for word completion

Some of the Icicles faces you can customize:

 * `icicle-match-highlight-minibuffer'   - Highlight match in minibuf
 * `icicle-match-highlight-Completions'  - Same, but in *Completions*
 * `icicle-common-match-highlight-Completions'  - Part common to all
 * `icicle-complete-input'               - Highlight input if complete

----------------------------------------------------------------------

These are all of the minibuffer bindings during completion:

\\{minibuffer-local-completion-map}---------------------------------------\
-------------------------------
"))

  (setq icicle-prompt-suffix
        (substitute-command-keys
         " (\\<minibuffer-local-completion-map>\\[icicle-apropos-complete], \
\\[icicle-prefix-complete]: list, \\[icicle-completion-help]: help) "))
  (when (and (interactive-p) turn-on-p)
    (message (substitute-command-keys
              "Use `\\<minibuffer-local-completion-map>\
\\[icicle-completion-help]' in minibuffer for help."))))

(defsubst icicle-remap (from to map)
  "Bind command TO in MAP to all keys currently bound globally to FROM."
  (substitute-key-definition from to map global-map))

(defsubst icicle-unmap (from map)
  "In MAP, unbind any keys that are globally bound to FROM."
  (substitute-key-definition from nil map global-map))

(defun icicle-bind-completion-keys (map)
  "Bind keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."

  ;; Menu-bar Minibuf menu.
  (define-key map [menu-bar minibuf ?\?] nil)
  (define-key map [menu-bar minibuf space] nil)
  (define-key map [menu-bar minibuf tab] nil)
  (define-key map [menu-bar minibuf separator-last] '("--"))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf action-all]
        (list 'menu-item "Act On All Completions - Careful!" 'icicle-all-candidates-action
              :enable '(and icicle-mode icicle-candidate-action-fn)
              :help "Apply the command action to *each* of the possible completions"))
    (define-key map [menu-bar minibuf action-all]
      (cons "Act On All Completions - Careful!" 'icicle-all-candidates-action))
    (put 'icicle-all-candidates-action 'menu-enable
         '(and icicle-mode icicle-candidate-action-fn)))
  (define-key map [menu-bar minibuf separator-actions] '("--"))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-define]
        (list 'menu-item "Define Completions Set by Lisp Sexpr" 'icicle-candidate-set-define
              :help "Define the set of current completion candidates by evalating a sexpr"))
    (define-key map [menu-bar minibuf set-define]
      (cons "Define Completions Set by Lisp Sexpr" 'icicle-candidate-set-define)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-swap]
        (list 'menu-item "Swap Saved and Current Completions Sets" 'icicle-candidate-set-swap
              :help "Swap the saved and current sets of completion candidates"))
    (define-key map [menu-bar minibuf set-swap]
      (cons "Swap Saved and Current Completions Sets" 'icicle-candidate-set-swap)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-retrieve-from-variable]
        (list 'menu-item "Retrieve Saved Completions Set From Variable"
              'icicle-candidate-set-retrieve-from-variable
              :help "Retrieve saved completion candidates from variable, making them current"))
    (define-key map [menu-bar minibuf set-retrieve-from-variable]
      (cons "Retrieve Saved Completions Set From Variable"
            'icicle-candidate-set-retrieve-from-variable)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-save-to-variable]
        (list 'menu-item "Save Completions Set To Variable"
              'icicle-candidate-set-save-to-variable
              :help "Save current completion candidates to a variable, for later recall"))
    (define-key map [menu-bar minibuf set-save-to-variable]
      (cons "Save Completions Set To Variable" 'icicle-candidate-set-save-to-variable)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-retrieve-from-cache-file]
        (list 'menu-item "Retrieve Saved Completions Set From Cache File"
              'icicle-candidate-set-retrieve-from-cache-file
              :help "Retrieve saved completion candidates from cache file, making them current"))
    (define-key map [menu-bar minibuf set-retrieve-from-cache-file]
      (cons "Retrieve Saved Completions Set From Cache File"
            'icicle-candidate-set-retrieve-from-cache-file)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-save-to-cache-file]
        (list 'menu-item "Save Completions Set To Cache File"
              'icicle-candidate-set-save-to-cache-file
              :help "Save current completion candidates to your cache file, for later recall"))
    (define-key map [menu-bar minibuf set-save-to-cache-file]
      (cons "Save Completions Set To Cache File" 'icicle-candidate-set-save-to-cache-file)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-retrieve]
        (list 'menu-item "Retrieve Saved Completions Set" 'icicle-candidate-set-retrieve
              :help "Retrieve the saved set of completion candidates, making it current"))
    (define-key map [menu-bar minibuf set-retrieve]
      (cons "Retrieve Saved Completions Set" 'icicle-candidate-set-retrieve)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-save]
        (list 'menu-item "Save Completions Set" 'icicle-candidate-set-save
              :help "Save the set of current completion candidates, for later recall"))
    (define-key map [menu-bar minibuf set-save]
      (cons "Save Completions Set" 'icicle-candidate-set-save)))
  (define-key map [menu-bar minibuf separator-set2] '("--"))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-intersection]
        (list 'menu-item "Intersect Saved Completions Set" 'icicle-candidate-set-intersection
              :help "Set intersection between the current and saved candidates"))
    (define-key map [menu-bar minibuf set-intersection]
      (cons "Intersect Saved Completions Set" 'icicle-candidate-set-intersection)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-union]
        (list 'menu-item "Add Saved Completions Set" 'icicle-candidate-set-union
              :help "Set difference between the current and saved completion candidates"))
    (define-key map [menu-bar minibuf set-union]
      (cons "Add Saved Completions Set" 'icicle-candidate-set-union)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-difference]
        (list 'menu-item "Subtract Saved Completions Set" 'icicle-candidate-set-difference
              :help "Set difference between the current and saved completion candidates"))
    (define-key map [menu-bar minibuf set-difference]
      (cons "Subtract Saved Completions Set" 'icicle-candidate-set-difference)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-complement]
        (list 'menu-item "Complement Completions Set" 'icicle-candidate-set-complement
              :help "Complement the set of current completion candidates"))
    (define-key map [menu-bar minibuf set-complement]
      (cons "Complement Completions Set" 'icicle-candidate-set-complement)))
  (define-key map [menu-bar minibuf separator-set1] '("--"))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf word-complete]
        (list 'menu-item "Word-Complete" 'icicle-prefix-word-complete
              :help "Complete at most one word of prefix"))
    (define-key map [menu-bar minibuf word-complete]
      (cons "Word-Complete" 'icicle-prefix-word-complete)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf prefix-complete]
        (list 'menu-item "Prefix-Complete" 'icicle-prefix-complete
              :help "Complete prefix as far as possible"))
    (define-key map [menu-bar minibuf prefix-complete]
      (cons "Prefix-Complete" 'icicle-prefix-complete)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf apropos-complete]
        (list 'menu-item "Apropos-Complete" 'icicle-apropos-complete
              :help "Complete regular expression as far as possible and list completions"))
    (define-key map [menu-bar minibuf apropos-complete]
      (cons "Apropos-Complete" 'icicle-apropos-complete)))
  (if (> emacs-major-version 21)
      (define-key-after map [menu-bar minibuf C-h]
        (list 'menu-item "Help" 'icicle-completion-help
              :help "Display help on minibuffer completion") [menu-bar minibuf ?\?])
    ;; Emacs 20: Cannot use `define-key-after' with multi-event key.
    (define-key map [menu-bar minibuf C-h] (cons "Help" 'icicle-completion-help)))

  ;; Remap some commands bound globally.
  (if (fboundp 'command-remapping)
      (define-key map [remap self-insert-command] 'icicle-self-insert)
    (substitute-key-definition 'self-insert-command 'icicle-self-insert map global-map))
  (icicle-remap 'backward-delete-char-untabify 'icicle-backward-delete-char-untabify map)
  (icicle-remap 'delete-backward-char          'icicle-delete-backward-char map)
  (icicle-remap 'delete-char                   'icicle-delete-char map)
  (icicle-remap 'backward-kill-word            'icicle-backward-kill-word map)
  (icicle-remap 'kill-word                     'icicle-kill-word map)
  (icicle-remap 'backward-kill-sexp            'icicle-backward-kill-sexp map)
  (icicle-remap 'kill-sexp                     'icicle-kill-sexp map)
  (icicle-remap 'backward-kill-sentence        'icicle-backward-kill-sentence map)
  (icicle-remap 'kill-sentence                 'icicle-kill-sentence map)
  (icicle-remap 'backward-kill-paragraph       'icicle-backward-kill-paragraph map)
  (icicle-remap 'kill-paragraph                'icicle-kill-paragraph map)
  (icicle-remap 'kill-line                     'icicle-kill-line map)
  (icicle-remap 'kill-region                   'icicle-kill-region map)
  (icicle-remap 'kill-region-wimpy             'icicle-kill-region-wimpy map)
  (icicle-remap 'transpose-chars               'icicle-transpose-chars map)
  (icicle-remap 'transpose-words               'icicle-transpose-words map)
  (icicle-remap 'transpose-sexps               'icicle-transpose-sexps map)
  (icicle-remap 'transpose-yank                'icicle-transpose-yank map)
  (icicle-remap 'transpose-yank-pop            'icicle-transpose-yank-pop  map)
  (icicle-remap 'help-command                  'icicle-completion-help map)
  (unless icicle-arrows-respect-completion-type-flag
    (icicle-remap 'previous-line               'icicle-previous-prefix-candidate map)
    (icicle-remap 'next-line                   'icicle-next-prefix-candidate map))
  (icicle-remap 'scroll-up                     'icicle-next-apropos-candidate map)
  (icicle-remap 'scroll-down                   'icicle-previous-apropos-candidate map)
  (icicle-remap 'backward-paragraph            'icicle-previous-prefix-candidate-action map)
  (icicle-remap 'forward-paragraph             'icicle-next-prefix-candidate-action map)
  (icicle-remap 'scroll-right                  'icicle-previous-apropos-candidate-action map)
  (icicle-remap 'scroll-left                   'icicle-next-apropos-candidate-action map)

  ;; Bind some additional keys.
  (define-key map icicle-word-completion-key 'icicle-prefix-word-complete)
  (define-key map [M-S-backspace]            'icicle-erase-minibuffer)
  (define-key map [M-S-delete]               'icicle-erase-minibuffer)
  (define-key map [(meta ?h)]                'icicle-history)
  (define-key map [(meta pause)]             'icicle-keep-only-past-inputs)
  (define-key map [(control help)]           'icicle-help-on-candidate)
  (define-key map [(control f1)]             'icicle-help-on-candidate)
  (define-key map [(control return)]         'icicle-candidate-action)
  (define-key map [(control ?o)]             'icicle-candidate-action)
  (define-key map [(control ?!)]             'icicle-all-candidates-action)
  (define-key map [S-iso-lefttab]            'icicle-apropos-complete)
  (define-key map [S-tab]                    'icicle-apropos-complete)
  (define-key map [S-M-C-iso-lefttab]        'icicle-apropos-complete-no-display)
  (define-key map [S-M-C-tab]                'icicle-apropos-complete-no-display)
  (define-key map [(control ?i)]             'icicle-prefix-complete)
  (define-key map [tab]                      'icicle-prefix-complete)
  (define-key map [(meta control ?/)]        'icicle-prefix-complete) ; For `dabbrev.el'.
  (define-key map [(meta control tab)]       'icicle-prefix-complete-no-display)
  (define-key map [insert]                   'icicle-switch-to-Completions-buf)
  ;; `minibuffer-completion-help' got wiped out by remap for self-insert.
  (define-key map "?"                        'icicle-self-insert)
  (define-key map [(control ?g)]             'icicle-abort-minibuffer-input)
  (define-key map [(control ?l)]             'icicle-retrieve-last-input)
  (define-key map " "                        'icicle-self-insert)
  (define-key map [(control ?~)]             'icicle-candidate-set-complement)
  (define-key map [(control ?-)]             'icicle-candidate-set-difference)
  (define-key map [(control ?+)]             'icicle-candidate-set-union)
  (define-key map [(control ?*)]             'icicle-candidate-set-intersection)
  (define-key map [(control ?>)]             'icicle-candidate-set-save)
  (define-key map [(control ?<)]             'icicle-candidate-set-retrieve)
  (define-key map [(meta control ?})]        'icicle-candidate-set-save-to-variable)
  (define-key map [(meta control ?{)]        'icicle-candidate-set-retrieve-from-variable)
  (define-key map [(control ?})]             'icicle-candidate-set-save-to-cache-file)
  (define-key map [(control ?{)]             'icicle-candidate-set-retrieve-from-cache-file)
  (define-key map [(control ?%)]             'icicle-candidate-set-swap)
  (define-key map [(control ?:)]             'icicle-candidate-set-define)
  (define-key map [(control ?=)]             'icicle-insert-string-from-variable)
  (define-key map [(control ?,)]             'icicle-toggle-sorting)
  (define-key map [(control ?.)]             'icicle-toggle-ignored-extensions)
  (define-key map [(control ?^)]             'icicle-toggle-incremental-completion)
  (define-key map [(control ?`)]             'icicle-toggle-regexp-quote)
  (define-key map [(meta ?.)]                'icicle-insert-string-at-point)
  (define-key map [(meta ?*)]                'icicle-narrow-candidates))

(defun icicle-restore-completion-keys (map)
  "Restore standard keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."

  ;; Menu-bar Minibuf menu.
  (define-key map [menu-bar minibuf separator-last]    nil)
  (define-key map [menu-bar minibuf action-all]        nil)
  (define-key map [menu-bar minibuf separator-actions] nil)
  (define-key map [menu-bar minibuf set-define]        nil)
  (define-key map [menu-bar minibuf set-swap]          nil)
  (define-key map [menu-bar minibuf set-retrieve]      nil)
  (define-key map [menu-bar minibuf set-save]          nil)
  (define-key map [menu-bar minibuf separator-set2]    nil)
  (define-key map [menu-bar minibuf set-intersection]  nil)
  (define-key map [menu-bar minibuf set-union]         nil)
  (define-key map [menu-bar minibuf set-difference]    nil)
  (define-key map [menu-bar minibuf set-complement]    nil)
  (define-key map [menu-bar minibuf separator-set1]    nil)
  (define-key map [menu-bar minibuf word-complete]     nil)
  (define-key map [menu-bar minibuf prefix-complete]   nil)
  (define-key map [menu-bar minibuf apropos-complete]  nil)
  (define-key map [menu-bar minibuf C-h]               nil)
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf ?\?]
        (list 'menu-item "List Completions" 'minibuffer-completion-help
              :help "Display all possible completions"))
    (define-key map [menu-bar minibuf ?\?]
      (cons "List Completions" 'minibuffer-completion-help)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf space]
        (list 'menu-item "Complete Word" 'minibuffer-complete-word
              :help "Complete at most one word"))
    (define-key map [menu-bar minibuf space]
      (cons "Complete Word" 'minibuffer-complete-word)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf tab]
        (list 'menu-item "Complete" 'minibuffer-complete :help "Complete as far as possible"))
    (define-key map [menu-bar minibuf tab] (cons "Complete" 'minibuffer-complete)))

  ;; Restore remapped commands.
  (if (fboundp 'command-remapping)
      (define-key map [remap self-insert-command] nil)
    (substitute-key-definition 'self-insert-command nil map global-map))
  (icicle-unmap 'backward-delete-char-untabify map)
  (icicle-unmap 'delete-backward-char          map)
  (icicle-unmap 'delete-char                   map)
  (icicle-unmap 'backward-kill-word            map)
  (icicle-unmap 'kill-word                     map)
  (icicle-unmap 'backward-kill-sexp            map)
  (icicle-unmap 'kill-sexp                     map)
  (icicle-unmap 'backward-kill-sentence        map)
  (icicle-unmap 'kill-sentence                 map)
  (icicle-unmap 'backward-kill-paragraph       map)
  (icicle-unmap 'kill-paragraph                map)
  (icicle-unmap 'kill-line                     map)
  (icicle-unmap 'kill-region                   map)
  (icicle-unmap 'kill-region-wimpy             map)
  (icicle-unmap 'transpose-chars               map)
  (icicle-unmap 'transpose-words               map)
  (icicle-unmap 'transpose-sexps               map)
  (icicle-unmap 'transpose-yank                map)
  (icicle-unmap 'transpose-yank-pop            map)
  (icicle-unmap 'help-command                  map)
  (icicle-unmap 'previous-line                 map)
  (icicle-unmap 'next-line                     map)
  (icicle-unmap 'scroll-up                     map)
  (icicle-unmap 'scroll-down                   map)
  (icicle-unmap 'backward-paragraph            map)
  (icicle-unmap 'forward-paragraph             map)
  (icicle-unmap 'scroll-right                  map)
  (icicle-unmap 'scroll-left                   map)

  ;; Restore additional bindings.
  (define-key map icicle-word-completion-key nil) ; Do first, so can be rebound, as needed.
  (define-key map [M-S-backspace]            nil)
  (define-key map [M-S-delete]               nil)
  (define-key map [(meta ?h)]                nil)
  (define-key map [(meta pause)]             nil)
  (define-key map [(control help)]           nil)
  (define-key map [(control f1)]             nil)
  (define-key map [(control ?l)]             nil)
  (define-key map [(control return)]         nil)
  (define-key map [(control ?o)]             nil)
  (define-key map [(control ?!)]             nil)
  (define-key map [S-iso-lefttab]            nil)
  (define-key map [S-tab]                    nil)
  (define-key map [insert]                   nil)
  (define-key map [(control ?~)]             nil)
  (define-key map [(control ?-)]             nil)
  (define-key map [(control ?+)]             nil)
  (define-key map [(control ?*)]             nil)
  (define-key map [(control ?>)]             nil)
  (define-key map [(control ?<)]             nil)
  (define-key map [(shift control ?})]       nil)
  (define-key map [(shift control ?{)]       nil)
  (define-key map [(control ?})]             nil)
  (define-key map [(control ?{)]             nil)
  (define-key map [(control ?%)]             nil)
  (define-key map [(control ?:)]             nil)
  (define-key map [(control ?=)]             nil)
  (define-key map [(control ?,)]             nil)
  (define-key map [(control ?.)]             nil)
  (define-key map [(control ?^)]             nil)
  (define-key map [(control ?`)]             nil)
  (define-key map [(meta ?.)]                nil)
  (define-key map [(meta ?*)]                nil)
  (define-key map "?"                        'minibuffer-completion-help)
  (define-key map [(control ?i)]             'minibuffer-complete)
  (define-key map [tab]                      'minibuffer-complete)
  (define-key map [(meta control tab)]       nil)
  (define-key map [(meta control ?/)]        nil)
  (define-key map [(control ?g)]             'abort-recursive-edit)
  (define-key map " "                        'minibuffer-complete-word)
  (define-key map [(meta ?n)]                'next-history-element)
  (define-key map [down]                     'next-history-element)
  (define-key map [next]                     'next-history-element)
  (define-key map [(meta ?p)]                'previous-history-element)
  (define-key map [up]                       'previous-history-element)
  (define-key map [prior]                    'switch-to-completions)
  (define-key map [(meta ?v)]                'switch-to-completions))

;; Inspired from `icomplete-minibuffer-setup'.
;; We change the region background here dynamically.
;; It would be better to just use a buffer-local face, but those don't yet exist.
;;
(defun icicle-minibuffer-setup ()
  "Run in minibuffer on activation, to enable completion cycling.
Usually run by inclusion in `minibuffer-setup-hook'."
  (cond ((and icicle-mode (window-minibuffer-p (selected-window)) (not executing-kbd-macro))
         ;; The pre- and post-command hooks are local to the
         ;; minibuffer, so they are added here, not in `icicle-mode'.
         ;; They are removed in `icicle-mode' when mode is exited.
         (unless (fboundp 'define-minor-mode) (make-local-hook 'pre-command-hook))
         (add-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook nil t)
         (unless (fboundp 'define-minor-mode) (make-local-hook 'post-command-hook))
         (add-hook 'post-command-hook        'icicle-run-icicle-post-command-hook nil t)
         (when (= 1 (recursion-depth))
           (setq icicle-saved-region-background (face-background 'region)))
         (when icicle-change-region-background-flag
           (set-face-background 'region icicle-region-background))
         ;; Reset prompt, because some commands (e.g. `find-file') don't use `read-file-name'
         ;; or `completing-read'.  Reset other stuff too.
         (setq icicle-prompt                         ""
               icicle-default-directory              default-directory
               icicle-initial-value                  nil
               icicle-last-completion-command        nil
               icicle-last-input                     ""
               icicle-completion-candidates          nil
               icicle-candidate-nb                   nil
               icicle-incremental-completion-p       icicle-incremental-completion-flag)
         (when icicle-arrows-respect-completion-type-flag
           (dolist (map (if (boundp 'minibuffer-local-filename-completion-map)
                            (list minibuffer-local-completion-map minibuffer-local-filename-completion-map
                                  minibuffer-local-must-match-map)
                          (list minibuffer-local-completion-map minibuffer-local-must-match-map)))
             (define-key map [up]   'previous-history-element)
             (define-key map [down] 'next-history-element)))
         (icicle-update-ignored-extensions-regexp)
         (when (memq icicle-init-value-flag '(preselect-start preselect-end))
           (icicle-select-minibuffer-contents))
         (when (and icicle-show-Completions-initially-flag (icicle-completing-p))
           (icicle-display-Completions))
         (run-hooks 'icicle-minibuffer-setup-hook))))

(defun icicle-cancel-*Help*-redirection ()
  "Cancel redirection of focus from *Help* buffer to minibuffer.
Focus was redirected during `icicle-help-on-candidate'."
  (let* ((help-window (get-buffer-window "*Help*" t))
         (help-frame (and help-window (window-frame help-window))))
    (when help-frame (redirect-frame-focus help-frame))))

(defun icicle-run-icicle-pre-command-hook ()
  "Run `icicle-pre-command-hook' functions.
Used in `pre-command-hook'."
  (run-hooks 'icicle-pre-command-hook))

(defun icicle-run-icicle-post-command-hook ()
  "Run `icicle-post-command-hook' functions.
Used in `post-command-hook'."
  (run-hooks 'icicle-post-command-hook))

(defun icicle-set-calling-cmd ()
  "Remember last command that called for completion.
Used in `completion-setup-hook'."
  (setq icicle-cmd-calling-for-completion this-command))

(defun icicle-update-ignored-extensions-regexp ()
  "Update ignored extensions if `completion-ignored-extensions' changed."
  (when (and (icicle-file-name-input-p)
             (not (equal icicle-ignored-extensions completion-ignored-extensions)))
    (setq icicle-ignored-extensions-regexp ; Make regexp for ignored file extensions.
          (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "\\|") "\\)\\'"))
    ;; Flag to prevent updating `icicle-ignored-extensions-regexp' unless
    ;; `completion-ignored-extensions' changes.
    (setq icicle-ignored-extensions completion-ignored-extensions)))

(defun icicle-completing-p ()
  "Non-nil if reading minibuffer input with completion."
  (not (null minibuffer-completion-table)))

;; We change the region background here dynamically.
;; It would be better to just use a buffer-local face, but those don't yet exist.
(defun icicle-restore-region-face ()
  "Restore region face.  It was changed during minibuffer activity
if `icicle-change-region-background-flag' is non-nil."
  (when icicle-change-region-background-flag
    (set-face-background 'region icicle-saved-region-background)))

(defun icicle-activate-mark ()
  "Prevent region from being deactivated.  Use in `icicle-post-command-hook'."
  (when (and (window-minibuffer-p (selected-window)) (not executing-kbd-macro))
    (setq deactivate-mark nil)))




;;; Icicles functions - Icicle mode helper functions . . . .

;; This is only used in Emacs 22+, but we define it always anyway.
(defun icicle-undo-std-completion-faces ()
  "Get rid of standard completion-root highlighting in *Completions*."
  ;; Do this because the standard Emacs 22 highlighting can interfere with
  ;; apropos-completion highlighting.
  (when (fboundp 'face-spec-reset-face)
    (when (facep 'completions-common-part)
      (face-spec-reset-face 'completions-common-part)
      (set-face-attribute 'completions-common-part nil :inherit nil))
    (when (facep 'completions-first-difference)
      (face-spec-reset-face 'completions-first-difference)
      (set-face-attribute 'completions-first-difference nil :inherit nil))))

(defun icicle-redefine-std-completion-fns ()
  "Replace standard completion functions with versions for Icicle mode."
  (when (fboundp 'icicle-completing-read)
    (defalias 'exit-minibuffer              (symbol-function 'icicle-exit-minibuffer))
    (defalias 'minibuffer-complete-and-exit (symbol-function 'icicle-minibuffer-complete-and-exit))
    (defalias 'switch-to-completions        (symbol-function 'icicle-switch-to-completions))
    (defalias 'choose-completion-string     (symbol-function 'icicle-choose-completion-string))
    (defalias 'mouse-choose-completion      (symbol-function 'icicle-mouse-choose-completion))
    (defalias 'completion-setup-function    (symbol-function 'icicle-completion-setup-function))
    (defalias 'completing-read              (symbol-function 'icicle-completing-read))
    (defalias 'read-file-name               (symbol-function 'icicle-read-file-name))))

(defun icicle-restore-std-completion-fns ()
  "Restore standard completion functions replaced in Icicle mode."
  (when (fboundp 'old-completing-read)
    (defalias 'exit-minibuffer              (symbol-function 'old-exit-minibuffer))
    (defalias 'minibuffer-complete-and-exit (symbol-function 'old-minibuffer-complete-and-exit))
    (defalias 'switch-to-completions        (symbol-function 'old-switch-to-completions))
    (defalias 'choose-completion-string     (symbol-function 'old-choose-completion-string))
    (defalias 'mouse-choose-completion      (symbol-function 'old-mouse-choose-completion))
    (defalias 'completion-setup-function    (symbol-function 'old-completion-setup-function))
    (defalias 'completing-read              (symbol-function 'old-completing-read))
    (defalias 'read-file-name               (symbol-function 'old-read-file-name))))

(defun icicle-redefine-standard-commands ()
  "Replace certain standard Emacs commands with Icicles versions."
  (when (and (fboundp 'icicle-completing-read) icicle-redefine-standard-commands-flag)
    (defalias 'dabbrev-completion           (symbol-function 'icicle-dabbrev-completion))
    (defalias 'lisp-complete-symbol         (symbol-function 'icicle-lisp-complete-symbol))
    (defalias 'repeat-complex-command       (symbol-function 'icicle-repeat-complex-command))
    (defalias 'customize-apropos            (symbol-function 'icicle-customize-apropos))
    (defalias 'customize-apropos-faces      (symbol-function 'icicle-customize-apropos-faces))
    (defalias 'customize-apropos-groups     (symbol-function 'icicle-customize-apropos-groups))
    (defalias 'customize-apropos-options    (symbol-function 'icicle-customize-apropos-options))
    (defalias 'read-from-minibuffer         (symbol-function 'icicle-read-from-minibuffer))
    (defalias 'read-string                  (symbol-function 'icicle-read-string))))

(defun icicle-restore-standard-commands ()
  "Restore standard Emacs commands replaced in Icicle mode."
  (when (and (fboundp 'old-completing-read) icicle-redefine-standard-commands-flag)
    (defalias 'dabbrev-completion           (symbol-function 'old-dabbrev-completion))
    (defalias 'lisp-complete-symbol         (symbol-function 'old-lisp-complete-symbol))
    (defalias 'repeat-complex-command       (symbol-function 'old-repeat-complex-command))
    (defalias 'customize-apropos            (symbol-function 'old-customize-apropos))
    (defalias 'customize-apropos-faces      (symbol-function 'old-customize-apropos-faces))
    (defalias 'customize-apropos-groups     (symbol-function 'old-customize-apropos-groups))
    (defalias 'customize-apropos-options    (symbol-function 'old-customize-apropos-options))
    (defalias 'read-from-minibuffer         (symbol-function 'old-read-from-minibuffer))
    (defalias 'read-string                  (symbol-function 'old-read-string))))

(defun icicle-redefine-standard-options ()
  "Replace certain standard Emacs options with Icicles versions."
  (when (boundp 'icicle-search-ring-max)
    (setq icicle-saved-search-ring-max        search-ring-max ; Save it.
          search-ring-max                     icicle-search-ring-max)
    (setq icicle-saved-regexp-search-ring-max regexp-search-ring-max ; Save it.
          regexp-search-ring-max              icicle-regexp-search-ring-max)))

(defun icicle-restore-standard-options ()
  "Restore standard Emacs options replaced in Icicle mode."
  (when (boundp 'icicle-saved-search-ring-max)
    (setq search-ring-max        icicle-saved-search-ring-max)
    (setq regexp-search-ring-max icicle-saved-regexp-search-ring-max)))

(defun icicle-bind-isearch-keys ()
  "Bind `S-TAB' in Isearch maps.  Use in `isearch-mode-hook'."
  (define-key isearch-mode-map [S-tab] 'icicle-isearch-complete)
  (define-key minibuffer-local-isearch-map [S-tab] 'isearch-complete-edit))




;;; Icicles functions - prefix completion cycling...........

(defun icicle-prefix-candidates (input)
  "List of candidate prefix completions for the current partial INPUT.
INPUT is a string.  Each candidate is a string."
  (if icicle-sort-function
      (sort (icicle-unsorted-prefix-candidates input) icicle-sort-function)
    (icicle-unsorted-prefix-candidates input)))

(defun icicle-unsorted-prefix-candidates (input)
  "Unsorted list of prefix completions for the current partial INPUT."
  (append icicle-extra-candidates
          (icicle-delete-if-not
           (lambda (cand) (let ((case-fold-search completion-ignore-case))
                            (icicle-filter-wo-input cand)))
           (all-completions input minibuffer-completion-table minibuffer-completion-predicate
                            icicle-completion-nospace-flag))))

(defun icicle-file-name-prefix-candidates (input)
  "List of prefix completions for partial file name INPUT.
INPUT is a string.
Candidates can be directories.  Each candidate is a string."
  (let ((default-directory (icicle-file-name-directory-w-default input)))
    (icicle-sort-and-strip-ignored
     (icicle-unsorted-file-name-prefix-candidates
      (or (icicle-file-name-nondirectory input) "")))))

(defun icicle-unsorted-file-name-prefix-candidates (input)
  "Unsorted list of prefix completions for the current file-name INPUT."
  (let ((slashed-p (and (> (length input) 0) (eq ?/ (aref input 0)))))
    (when slashed-p (setq input (substring input 1)))
    (append icicle-extra-candidates
            (icicle-delete-if-not
             (lambda (cand) (let ((case-fold-search completion-ignore-case))
                              (if (member cand '("../" "./"))
                                  (member input '(".." ".")) ; Prevent "" from matching "../"
                                (and (string-match (concat "^" (regexp-quote input)) cand)
                                     (icicle-filter-wo-input cand)))))
             (all-completions input minibuffer-completion-table
                              (if slashed-p "/" default-directory)
                              icicle-completion-nospace-flag)))))




;;; Icicles functions - apropos completion cycling..........

(defun icicle-apropos-candidates (input)
  "List of candidate apropos completions for the current partial INPUT.
INPUT is a string.  Each candidate is a string."
  (if icicle-sort-function
      (sort (icicle-unsorted-apropos-candidates input) icicle-sort-function)
    (icicle-unsorted-apropos-candidates input)))

(defun icicle-unsorted-apropos-candidates (input)
  "Unsorted list of apropos completions for the current partial INPUT.
When `icicle-expand-input-to-common-match-flag' is non-nil, this also
sets `icicle-common-match-string' to the longest common match of input
over all candidates."
  (when icicle-regexp-quote-flag (setq input (regexp-quote input)))
  (let ((candidates (append icicle-extra-candidates
                            (icicle-delete-if-not
                             (lambda (cand) (let ((case-fold-search completion-ignore-case))
                                              (and (string-match input cand)
                                                   (icicle-filter-wo-input cand))))
                             (all-completions "" minibuffer-completion-table
                                              minibuffer-completion-predicate
                                              icicle-completion-nospace-flag)))))
    (when (and icicle-expand-input-to-common-match-flag (consp candidates))
      (setq icicle-common-match-string (icicle-longest-common-match input candidates)))
    candidates))                        ; Return candidates.

(defun icicle-file-name-apropos-candidates (input)
  "List of apropos completions for partial file-name INPUT.
INPUT is a string.
Candidates can be directories.  Each candidate is a string."
  (let ((default-directory (icicle-file-name-directory-w-default input)))
    (icicle-sort-and-strip-ignored
     (icicle-unsorted-file-name-apropos-candidates
      (or (icicle-file-name-nondirectory input) "")))))

(defun icicle-unsorted-file-name-apropos-candidates (input)
  "Unsorted list of apropos completions for the partial file-name INPUT.
When `icicle-expand-input-to-common-match-flag' is non-nil, this also
sets `icicle-common-match-string' to the longest common match of input
over all candidates."
  (when icicle-regexp-quote-flag (setq input (regexp-quote input)))
  (let ((slashed-p (and (> (length input) 0) (eq ?/ (aref input 0)))))
    (when slashed-p (setq input (substring input 1)))
    (let ((candidates (append icicle-extra-candidates
                              (icicle-delete-if-not
                               (lambda (cand)
                                 (let ((case-fold-search completion-ignore-case))
                                   (if (member cand '("../" "./")) ; Prevent "" from matching "../"
                                       (member input '(".." "."))
                                     (and (string-match input cand)
                                          (icicle-filter-wo-input cand)))))
                               (all-completions "" minibuffer-completion-table
                                                (if slashed-p "/" default-directory)
                                                icicle-completion-nospace-flag)))))
      (when (and icicle-expand-input-to-common-match-flag (consp candidates))
        (setq icicle-common-match-string (icicle-longest-common-match input candidates)))
      candidates)))                     ; Return candidates.

(defun icicle-longest-common-match (input candidates)
  "Return the longest common match for INPUT among all CANDIDATES.
This assumes that INPUT matches each string in list CANDIDATES.
Return nil if there is no common match.  This actually returns
`regexp-quote' applied to the longest common match, so that special
characters in the match don't throw off regexp matching."
  (let ((case-fold-search completion-ignore-case)
        (first (car candidates)))
    (string-match input first)
    (let* ((len-first (length first))
           (beg 0)
           (end len-first)
           (orig-match-beg (match-beginning 0))
           (lcm first)                  ; "lcm" for "longest common match".
           (rest (cdr candidates)))
      ;; Compare with the rest of the candidates, reducing as needed.
      (while (and rest lcm)
        ;; Remove any prefix that doesn't match some other candidate.
        (while (and (< beg orig-match-beg)
                    (not (string-match (regexp-quote (substring lcm 0 (1+ (- orig-match-beg beg))))
                                       (car rest)))) ; Use 1+ so we include first character of input.
          ;; Take a character off of the left.
          (setq lcm (substring lcm 1)
                beg (1+ beg)))
        ;; Remove any suffix that doesn't match some other candidate.
        (while (and (> end 0) (not (string-match (regexp-quote lcm) (car rest))))
          ;; Take a character off of the right.
          (setq lcm (substring lcm 0 (1- (length lcm)))
                end (1- end)))
        (unless (and (string-match (regexp-quote lcm) (car rest))
                     (string-match input lcm))
          (setq lcm nil))               ; No possible expansion
        (pop rest))
      lcm)))




;;; Icicles functions - common helper functions.............

;; Main cycling function - used by `icicle-next-prefix-candidate' and `icicle-next-apropos-candidate'.
(defun icicle-next-candidate (nth candidates-fn &optional regexp-p)
  "Replace input by NTH next or previous completion for an input.
Default value of NTH is 1, meaning use the next completion.
Negative NTH means use a previous, not subsequent, completion.

CANDIDATES-FN is a function that returns the list of candidate
completions for its argument, the current partial input (a string).

Optional arg REGEXP-P non-nil means that CANDIDATES-FN uses regexp
matching. This is used to highlight the appropriate matching root."
  (unless (stringp icicle-last-completion-candidate)
    (setq icicle-last-completion-candidate icicle-initial-value))
  (setq nth (or nth 1))
  (setq icicle-current-input (icicle-minibuffer-contents-from-minibuffer))
  (setq icicle-common-match-string nil) ; Don't use old one in `icicle-save-or-restore-input'
  (icicle-save-or-restore-input regexp-p)
  (when (and (icicle-file-name-input-p) (icicle-file-directory-p icicle-current-input))
    (setq icicle-default-directory icicle-current-input))
  (icicle-recompute-candidates nth candidates-fn)
  (cond ((null icicle-completion-candidates)
         (save-selected-window (icicle-delete-windows-on "*Completions*"))
         (minibuffer-message "  [No completion]"))
        (t
         (icicle-clear-minibuffer)
         (let ((nb-cands (length icicle-completion-candidates))
               (unit (if (wholenump nth) 1 -1))
               next)
           ;; So `icomplete+' can append the number of other candidates to the minibuffer.
           (when icicle-completion-candidates
             (setq icicle-nb-of-other-cycle-candidates (1- nb-cands)))
           (icicle-increment-cand-nb+signal-end nth nb-cands)
           (setq next (elt icicle-completion-candidates icicle-candidate-nb))
           (while (null next)           ; Skip null candidates.
             (icicle-increment-cand-nb+signal-end unit nb-cands)
             (setq next (elt icicle-completion-candidates icicle-candidate-nb)))
           ;; Filter with predicate
           (when (and (not (icicle-file-name-input-p)) ; Pred is special for files.
                      minibuffer-completion-predicate)
             (while (not (condition-case nil
                             (funcall minibuffer-completion-predicate
                                      (if (arrayp minibuffer-completion-table)
                                          (intern next) ; obarray of symbols.
                                        (list next))) ; List of strings (sym names).
                           (error nil)))
               (icicle-increment-cand-nb+signal-end unit nb-cands)
               (setq next (elt icicle-completion-candidates icicle-candidate-nb))
               (while (null next)       ; Skip null candidates.
                 (icicle-increment-cand-nb+signal-end unit nb-cands)
                 (setq next (elt icicle-completion-candidates icicle-candidate-nb)))))
           ;; Reset last candidate.  Need a copy, because we change its text properties.
           (setq icicle-last-completion-candidate (copy-sequence next))

           ;; Underline the root that was completed, in the minibuffer.
           (let ((case-fold-search completion-ignore-case)
                 (inp (if (icicle-file-name-input-p)
                          (icicle-file-name-nondirectory icicle-current-input)
                        icicle-current-input))
                 indx)
             (unless regexp-p (setq inp (regexp-quote inp)))
             (setq indx (string-match inp icicle-last-completion-candidate))
             (when indx
               (put-text-property indx (match-end 0) 'face 'icicle-match-highlight-minibuffer
                                  icicle-last-completion-candidate)))
           (insert (if (and (icicle-file-name-input-p) insert-default-directory)
                       (icicle-file-name-directory-w-default icicle-current-input)
                     "")
                   icicle-last-completion-candidate)
           (icicle-place-cursor (if regexp-p icicle-current-regexp-input icicle-current-input))

           ;; Highlight current completion candidate, if *Completions* is displayed.
           (when (get-buffer-window "*Completions*" t)

             ;; Refresh *Completions*, updating it to reflect the current candidates.
             (unless (or (and (memq this-command '(icicle-next-apropos-candidate
                                                   icicle-previous-apropos-candidate
                                                   icicle-next-apropos-candidate-action
                                                   icicle-previous-apropos-candidate-action))
                              (memq last-command '(icicle-next-apropos-candidate
                                                   icicle-previous-apropos-candidate
                                                   icicle-next-apropos-candidate-action
                                                   icicle-previous-apropos-candidate-action
                                                   icicle-candidate-action)))
                         (and (memq this-command '(icicle-next-prefix-candidate
                                                   icicle-previous-prefix-candidate
                                                   icicle-next-prefix-candidate-action
                                                   icicle-previous-prefix-candidate-action))
                              (memq last-command '(icicle-next-prefix-candidate
                                                   icicle-previous-prefix-candidate
                                                   icicle-next-prefix-candidate-action
                                                   icicle-previous-prefix-candidate-action
                                                   icicle-candidate-action))))
               (icicle-display-candidates-in-Completions (not (wholenump nth))))
             ;; Highlight current candidate in *Completions*.
             (let ((compl-win (get-buffer-window "*Completions*" t))
                   curr-candidate-pos)
               (save-window-excursion
                 (select-window compl-win)
                 (let ((case-fold-search completion-ignore-case))
                   (goto-char (point-min))
                   (forward-line 3)
                   (icicle-move-to-next-completion icicle-candidate-nb t)
                   (set-buffer-modified-p nil)
                   (setq curr-candidate-pos (point))))
               (set-window-point compl-win curr-candidate-pos)))))))

(defun icicle-save-or-restore-input (&optional regexp-p)
  "Save current minibuffer input or restore last input.
If value in minibuffer now is `icicle-last-completion-candidate',
then it is probably not a real input, so restore last real input.
Otherwise, save current value as last input.

This also updates `icicle-current-regexp-input'.

If optional arg REGEXP-P and `icicle-common-match-string' are non-nil,
then also update the current input to be `icicle-common-match-string'."
  (cond ((and (not (string= "" icicle-last-input)) ; Don't restore empty input.
              (not (string= "" icicle-last-completion-candidate))
              ;; Current input = last candidate?
              (string= (if (icicle-file-name-input-p)
                           (directory-file-name (icicle-remove-dots
                                                 icicle-last-completion-candidate))
                         icicle-last-completion-candidate)
                       (if (icicle-file-name-input-p)
                           (if icicle-cycle-into-subdirs-flag
                               (icicle-file-name-nondirectory icicle-current-input)
                             (file-name-nondirectory
                              (directory-file-name (icicle-remove-dots icicle-current-input))))
                         icicle-current-input))
              (not (string= icicle-current-input icicle-initial-value)))
         (setq icicle-current-input icicle-last-input)) ; Restore last real input.
        (t
         (cond ((and icicle-expand-input-to-common-match-flag regexp-p icicle-common-match-string)
                (let ((common (if (and (icicle-file-name-input-p) insert-default-directory)
                                  (directory-file-name
                                   (expand-file-name icicle-common-match-string
                                                     (file-name-directory icicle-current-input)))
                                icicle-common-match-string)))
                  
                  ;; This test ensures:
                  ;; 1) We don't just change letter case (e.g. MS Windows file names).
                  ;; 2) We don't do this more than once (the saved regexp input would get overwritten).
                  (unless (or (and case-fold-search
                                   (string= (upcase icicle-current-input) (upcase common))
                                   (not (string= icicle-current-input common)))
                              (memq last-command (list this-command 'handle-switch-frame)))
                    (setq icicle-current-regexp-input icicle-current-input) ; Save it for `C-l'.
                    ;; Use common as current input, unless input is a directory.
                    (unless (and (icicle-file-name-input-p) (file-directory-p icicle-current-input))
                      (setq icicle-current-input common)))))
               ((and regexp-p
                     (not (memq last-command (list this-command 'handle-switch-frame)))
                     (not (memq this-command ; Cycled candidates are not input regexps.
                                '(icicle-next-apropos-candidate icicle-previous-apropos-candidate
                                  icicle-next-apropos-candidate-action
                                  icicle-previous-apropos-candidate-action
                                  icicle-next-prefix-candidate         icicle-previous-prefix-candidate
                                  icicle-next-prefix-candidate-action
                                  icicle-previous-prefix-candidate-action))))
                (setq icicle-current-regexp-input icicle-current-input))) ; Save it for `C-l'.
         
         ;; Update to use current input.
         (setq icicle-last-completion-candidate icicle-current-input
               icicle-last-input icicle-current-input))))

(defun icicle-remove-dots (filename)
  "Strip leading string through last ../ or ./ from FILENAME."
  (let ((newname filename))
    (while
        (or (string-match "\\.\\./" newname)
            (string-match "\\./" newname)
            ;; Emacs 21+ `file-relative-name' returns ".." and "." (no slash) for "" first arg
            (string-match "^\\.\\.$" newname) 
            (string-match "^\\.$" newname))
      (setq newname (substring newname (match-end 0))))
    newname))

(defun icicle-recompute-candidates (nth candidates-fn)
  "Recompute `icicle-completion-candidates', if needed.
If buffer *Completions* is already displayed, it is updated.
This does nothing, unless the user changed the minibuffer input or the
completion type has changed (from apropos to prefix or vice versa).
Argument NTH is passed to `icicle-display-candidates-in-Completions'.
Argument CANDIDATES-FN is a function that recomputes the candidates."
  (unless (and icicle-last-completion-command
               (string= icicle-current-input icicle-last-input) ; No change in user input.
               ;; No change in completion type: apropos vs prefix.
               (or (and (memq icicle-last-completion-command
                              '(icicle-apropos-complete icicle-candidate-set-complement
                                icicle-keep-only-past-inputs))
                        (memq this-command '(icicle-apropos-complete
                                             icicle-next-apropos-candidate
                                             icicle-next-apropos-candidate-action
                                             icicle-previous-apropos-candidate
                                             icicle-previous-apropos-candidate-action)))
                   (and (memq icicle-last-completion-command
                              '(icicle-prefix-complete icicle-candidate-set-complement
                                icicle-keep-only-past-inputs))
                        (memq this-command '(icicle-prefix-complete
                                             icicle-next-prefix-candidate
                                             icicle-next-prefix-candidate-action
                                             icicle-previous-prefix-candidate
                                             icicle-previous-prefix-candidate-action)))))
    ;; Set `icicle-last-completion-command', to record new completion type.
    (case this-command
      ((icicle-next-prefix-candidate
        icicle-previous-prefix-candidate
        icicle-next-prefix-candidate-action
        icicle-previous-prefix-candidate-action)
       (setq icicle-last-completion-command 'icicle-prefix-complete))
      ((icicle-next-apropos-candidate
        icicle-previous-apropos-candidate
        icicle-next-apropos-candidate-action
        icicle-previous-apropos-candidate-action)
       (setq icicle-last-completion-command 'icicle-apropos-complete)))
    ;; Recompute and redisplay completion candidates.  Reset candidate number.
    (setq icicle-completion-candidates (funcall candidates-fn icicle-current-input)
          icicle-candidate-nb          nil)
    (when (get-buffer-window "*Completions*" 0)
      (if icicle-completion-candidates
          (icicle-display-candidates-in-Completions (not (wholenump nth)))
        (save-selected-window (icicle-delete-windows-on "*Completions*"))))))

(defun icicle-increment-cand-nb+signal-end (incr max)
  "Increment candidate number by INCR modulo MAX, and signal end of cycle."
  (if icicle-candidate-nb
      (setq icicle-candidate-nb (+ incr icicle-candidate-nb))
    (setq icicle-candidate-nb 0))       ; Reset.
  (setq icicle-candidate-nb (mod icicle-candidate-nb max))
  (when (and (= 0 icicle-candidate-nb)  ; Signal end of cycle.
             (eq last-command this-command))
    (let ((visible-bell t)) (ding) (setq visible-bell nil) (ding))))

(defun icicle-place-overlay (start end overlay face buffer &rest properties)
  "Put OVERLAY with FACE between START and END in BUFFER.
OVERLAY is a symbol whose value is the overlay.  If nil, the overlay
  is created.  If non-nil, it is simply moved.
PROPERTIES are additional overlay properties to add: pairs of a
property and a value."
  (if (symbol-value overlay)            ; Overlay exists, just move it.
      (move-overlay (symbol-value overlay) start end buffer)
    (set overlay (make-overlay start end buffer))
    (overlay-put (symbol-value overlay) 'face face)))

(defun icicle-sort-and-strip-ignored (candidates)
  "Remove file names with ignored extensions, and \".\".  Sort CANDIDATES.
If `icicle-sort-function' is nil, then do not sort."
  (let* ((pred1 (lambda (cand) (or (string-match icicle-ignored-extensions-regexp cand)
                                   (string= "./" cand))))
         (pred2 (lambda (cand) (string= "./" cand)))
         (new-candidates (icicle-delete-if (if icicle-ignored-extensions-regexp pred1 pred2)
                                           candidates)))
    ;; If the only candidates have ignored extensions, then use them.
    (unless new-candidates (setq new-candidates (icicle-delete-if pred2 candidates)))
    (if icicle-sort-function
        (sort new-candidates icicle-sort-function)
      new-candidates)))

(defun icicle-file-name-directory-w-default (file)
  "Like `file-name-directory', but return `default-directory', not nil.
Does not treat backslash as a directory separator, even on MS Windows."
  (let ((escaped-file (subst-char-in-string ?\\ ?\a file)))
    (or (file-name-directory escaped-file) default-directory)))

(defun icicle-file-name-nondirectory (file)
  "Like `file-name-nondirectory', but does not treat backslash specially.
That is, backslash is never treated as a directory separator."
  (let ((escaped-file (subst-char-in-string ?\\ ?\a file)))
    (subst-char-in-string ?\a ?\\ (file-name-nondirectory escaped-file))))

(defun icicle-file-name-input-p ()
  "Return non-nil if expected input is a file name.
This is used, instead of variable `minibuffer-completing-file-name',
because we sometimes complete against an explicit alist of file names,
even in the overall context of file-name input.  In that case, we do
not want to use file-name completion.  An example of this is
completing against a history list of file names, using
`icicle-history'."
  ;;
  ;; Note that some Emacs 20 code uses this as the equivalent of `minibuffer-completing-file-name':
  ;; (memq minibuffer-completion-table '(read-file-name-internal read-directory-name-internal))
  ;;
  (and (symbolp minibuffer-completion-table) (stringp minibuffer-completion-predicate)))

(defun icicle-sort-dirs-last (name1 name2)
  "Non-nil if NAME1 is a file and NAME2 is a dir, or `string-lessp'.
This can be used as the value for `icicle-sort-function'.
It is especially useful when `icicle-cycle-into-subdirs-flag' is
non-nil.  Otherwise, cycling into subdirectories is depth-first, not
breadth-first."
  (if (icicle-file-name-input-p)
      (let ((name1-dir-p (icicle-file-directory-p name1))
            (name2-dir-p (icicle-file-directory-p name2)))
        (if (or (and name1-dir-p name2-dir-p) ; Both or neither are directories.
                (not (or name1-dir-p name2-dir-p)))
            (string-lessp name1 name2)  ; Compare equals.
          name2-dir-p))                 ; Files come before directories.
    (string-lessp name1 name2)))

(defun icicle-sort-case-insensitively (string1 string2)
  "Like `string-lessp', but case is ignored, so `A' = `a' , and so on."
  (string-lessp (upcase string1) (upcase string2)))

(defun icicle-file-directory-p (file)
  "Local, faster replacement for `file-directory-p'.
This does not do all of the file-handler processing that 
`file-directory-p' does, so it is not a general replacement."
  (and (stringp file) (string= file (icicle-file-name-directory-w-default file))))

(defun icicle-minibuffer-contents ()
  "Return the user minibuffer input as a string, without text-properties."
  (save-selected-window
    (select-window (minibuffer-window))
    (icicle-minibuffer-contents-from-minibuffer)))

(defun icicle-minibuffer-contents-from-minibuffer ()
  "Return the user minibuffer input as a string, without text-properties.
The current buffer must be a minibuffer."
  (let ((input (if (fboundp 'minibuffer-contents-no-properties)
                   (minibuffer-contents-no-properties) ; e.g. Emacs 22
                 (buffer-substring-no-properties (point-min) (point-max))))) ; e.g. Emacs 20
    (when (and (icicle-file-name-input-p)
               (not (string= "" input))) ; Do nothing if user deleted everything in minibuffer.
      (let ((last-char ""))
        (when (string= "$" (substring input (1- (length input)) (length input)))
          (setq last-char "$"
                input (substring input 0 (1- (length input)))))
        (setq input
              (save-match-data
                (concat (subst-char-in-string ?\a ?\\
                                              (condition-case nil
                                                  (substitute-in-file-name
                                                   (subst-char-in-string ?\\ ?\a input 'in-place))
                                                (error input))
                                              'in-place)
                        last-char)))))
    input))

(defun icicle-filter-wo-input (candidate)
  "Filter completion CANDIDATE using regexps and predicate.
This filtering is in addition to matching user input."
  (and (or (not icicle-must-match-regexp)
           (string-match icicle-must-match-regexp candidate))
       (or (not icicle-must-not-match-regexp)
           (not (string-match icicle-must-not-match-regexp candidate)))
       (or (not icicle-must-pass-predicate)
           (funcall icicle-must-pass-predicate candidate))))

(defun icicle-update-completions ()
  "Update completions list.  Update display too, if already shown."
  (setq icicle-completion-candidates
        (funcall (case icicle-last-completion-command
                   (icicle-prefix-complete (if (icicle-file-name-input-p)
                                               #'icicle-file-name-prefix-candidates
                                             #'icicle-prefix-candidates))
                   (t (if (icicle-file-name-input-p)
                          #'icicle-file-name-apropos-candidates
                        #'icicle-apropos-candidates)))
                 icicle-current-input))
  (when (get-buffer-window "*Completions*" 0)
    (icicle-display-candidates-in-Completions)))

(defun icicle-msg-maybe-in-minibuffer (format-string &rest args)
  "Display FORMAT-STRING as a message.
If called with the minibuffer active, this is done using `message'.
Otherwise, it is done using `minibuffer-message'."
  (if (active-minibuffer-window)
      (minibuffer-message (apply #'format (concat "  [" format-string "]") args))
    (apply #'message format-string args)))

(defun icicle-delete-if (pred inlist)
  "A copy of list INLIST with no elements that satisfy predicate PRED."
  (let ((outlist nil))
    (dolist (o inlist) (unless (funcall pred o) (push o outlist)))
    (nreverse outlist)))

(defun icicle-delete-if-not (pred inlist)
  "A copy of list INLIST with only elements that satisfy predicate PRED."
  (let ((outlist nil))
    (dolist (o inlist) (when (funcall pred o) (push o outlist)))
    (nreverse outlist)))

(defun icicle-frames-on (buffer &optional frame) ; From `frames-on' in `frame-fns.el'.
  "List of all live frames showing BUFFER (a buffer or its name).
The optional FRAME argument is as for function `get-buffer-window'."
  (filtered-frame-list (function (lambda (fr) (get-buffer-window buffer fr)))))

(defun icicle-candidate-set-1 (set-fn msg)
  "Helper function for defining Icicle set commands.
SET-FN is the function to apply to the current and saved candidates.
MESSAGE is the confirmation message to display in the minibuffer."
  (setq icicle-completion-candidates
        (funcall set-fn icicle-completion-candidates icicle-saved-completion-candidates))
  (if (null icicle-completion-candidates)
      (minibuffer-message "  [EMPTY SET]")
    (icicle-maybe-sort-and-strip-candidates)
    (icicle-scroll-or-update-Completions msg)))

(defun icicle-maybe-sort-and-strip-candidates ()
  "Sort `icicle-completion-candidates'.  Strip ignored file names too."
  (if (icicle-file-name-input-p)
      (setq icicle-completion-candidates
            (icicle-sort-and-strip-ignored icicle-completion-candidates))
    (if icicle-sort-function
        (setq icicle-completion-candidates
              (sort icicle-completion-candidates icicle-sort-function)))))

(defun icicle-scroll-or-update-Completions (msg)
  "Scroll *Completions* if this command was repeated; else update it."
  (if (get-buffer-window "*Completions*" 0)
      (if (eq last-command this-command)
          ;; User repeated the command.  Scroll window around.
          (save-selected-window
            (select-window (get-buffer-window "*Completions*" 0))
            (condition-case nil
                (scroll-up nil)
              (end-of-buffer (goto-char (point-min)) (forward-line 3))))
        ;; User did something else (e.g. changed input).  Update the display.
        (icicle-display-candidates-in-Completions)
        (minibuffer-message msg))
    ;; No window yet.  Show window.
    (icicle-display-candidates-in-Completions)
    (minibuffer-message msg)))

(defun icicle-display-Completions ()
  "Display *Completions* buffer."
  (let ((completions (all-completions "" minibuffer-completion-table
                                      minibuffer-completion-predicate
                                      icicle-completion-nospace-flag)))
    (message "Displaying completion candidates...")
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list
       (if icicle-sort-function (sort completions icicle-sort-function) completions)))))

;; From `cl-seq.el', function `union', without keyword treatment.
;; Same as `simple-set-union' in `misc-fns.el'.
(defun icicle-set-union (list1 list2)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or
LIST2.  This is a non-destructive function; it copies the data if
necessary."
  (cond ((null list1) list2)
        ((null list2) list1)
        ((equal list1 list2) list1)
        (t
         (or (>= (length list1) (length list2))
             (setq list1 (prog1 list2 (setq list2 list1)))) ; Swap them.
         (while list2
           (unless (member (car list2) list1)
               (setq list1 (cons (car list2) list1)))
           (setq list2 (cdr list2)))
         list1)))

;; From `cl-seq.el', function `intersection', without keyword treatment.
;; Same as `simple-set-intersection' in `misc-fns.el'.
(defun icicle-set-intersection (list1 list2)
  "Set intersection of lists LIST1 and LIST2.
This is a non-destructive operation: it copies the data if necessary."
  (and list1 list2
       (if (equal list1 list2)
           list1
         (let ((result nil))
           (unless (>= (length list1) (length list2))
             (setq list1 (prog1 list2 (setq list2 list1)))) ; Swap them.
           (while list2
             (when (member (car list2) list1)
               (setq result (cons (car list2) result)))
             (setq list2 (cdr list2)))
           result))))

;; From `cl-seq.el', function `set-difference', without keyword treatment.
;; Same as `simple-set-difference' in `misc-fns.el'.
(defun icicle-set-difference (list1 list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-difference operation.
The result list contains all items that appear in LIST1 but not LIST2.
This is non-destructive; it makes a copy of the data if necessary, to
avoid corrupting the original LIST1 and LIST2."
  (if (or (null list1) (null list2)) list1    (let ((result nil))
      (while list1
        (unless (member (car list1) list2) (setq result (cons (car list1) result)))
        (setq list1 (cdr list1)))
      result)))

(defun icicle-highlight-complete-input ()
  "Highlight minibuffer input, showing that it is a sole completion.
Overlay `icicle-complete-input-overlay' is created with `match' face,
unless it exists."
  (let ((case-fold-search completion-ignore-case)
        input-start-position)
    (save-excursion
      (goto-char (icicle-minibuffer-prompt-end))
      (setq input-start-position (point))
      (when (and (icicle-file-name-input-p) insert-default-directory)
        (search-forward (icicle-file-name-directory-w-default
                         (icicle-minibuffer-contents-from-minibuffer)))
        (setq input-start-position (point))) ; Skip directory.
      (if icicle-complete-input-overlay ; Don't recreate if exists.
          (move-overlay icicle-complete-input-overlay
                        input-start-position (point-max) (current-buffer))
        (setq icicle-complete-input-overlay (make-overlay input-start-position (point-max)))
        (overlay-put icicle-complete-input-overlay 'face 'icicle-complete-input)))))

(defun icicle-call-then-update-Completions (fn &rest args)
  "Call FN with ARGS, then update *Completions* with input matches."
  (apply fn args)
  (setq icicle-current-input (icicle-minibuffer-contents-from-minibuffer))
  (when (and icicle-incremental-completion-p
             (or (get-buffer-window "*Completions*" 0) ; Already displayed.
                 (not (eq t icicle-incremental-completion-p))) ; Display anyway.
             (or (> icicle-incremental-completion-threshold (length icicle-completion-candidates))
                 (sit-for icicle-incremental-completion-delay))) ; Wait if there are lots of candidates.
    (let ((icicle-icompleting-p t))
      (setq this-command (if (eq 'icicle-prefix-complete icicle-last-completion-command)
                             'icicle-prefix-complete
                           'icicle-apropos-complete))
      (setq last-command fn)
      (funcall this-command)
      (run-hooks 'icicle-update-input-hook))))

(defun icicle-clear-minibuffer ()
  "Delete all user input in the minibuffer."
  (if (fboundp 'delete-minibuffer-contents) (delete-minibuffer-contents) (erase-buffer)))

;; Borrowed from `ps-print.el'
(defun icicle-remove-duplicates (list)
  "Copy of LIST with duplicate elements removed.  Tested with `equal'."
  (let ((tail list)
        new)
    (while tail
      (unless (member (car tail) new) (push (car tail) new))
      (pop tail))
    (nreverse new)))

(defun icicle-file-readable-p (file)
  "Return non-nil if FILE (a string) names a readable file."
  (and (not (string= "" file)) (file-readable-p file) (not (file-directory-p file))))

(defun icicle-file-writable-p (file)
  "Return non-nil if FILE (a string) names a writable file."
  (and (not (string= "" file)) (file-writable-p file) (not (file-directory-p file))))

(defun icicle-files-within (file-list accum)
  "List of all files in FILE-LIST.
Directories in FILE-LIST are processed recursively to include their
files and the files in their subdirectories.  The list of files is
accumulated in ACCUM, which is used for recursive calls."
  (let ((res accum))
    (while file-list
      (if (file-directory-p (car file-list))
          (setq res (icicle-files-within (directory-files (car file-list) 'full icicle-re-no-dot)
                                         res))
        (setq res (cons (car file-list) res)))
      (pop file-list))
    res))

(defun icicle-delete-whitespace-from-string (string)
  "Remove whitespace from STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let (char)
      (while (not (eobp))
        (setq char (char-after))
        (if (memq char '(?\  ?\t ?\n)) (delete-char 1) (forward-char 1)))
      (buffer-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-fn.el ends here
