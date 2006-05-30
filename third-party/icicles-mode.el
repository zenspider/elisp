;;; icicles-mode.el --- Icicle Mode definition for Icicles
;;
;; Filename: icicles-mode.el
;; Description: Icicle Mode definition for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 10:21:10 2006
;; Version: 22.0
;; Last-Updated: Fri May 19 22:34:28 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 121
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-mode.el
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
;;  This is a helper library for library `icicles.el'.  It defines the
;;  command `icicle-mode'.  See `icicles.el' for documentation.
;;
;;  User options defined here (in Custom group `icicles'):
;;
;;    `icicle-mode', `icicle-mode-hook'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-mode-map'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/05/19 dadams
;;     icicle-mode: (add-hook 'kill-emacs-hook 'icicle-control-reminder-prompt).
;; 2006/05/18 dadams
;;     Change :init-value to nil, per new Emacs convention.
;; 2006/05/13 dadams
;;     icicle-mode: Updated doc string.
;; 2006/05/10 dadams
;;     icicle-define-icicle-mode-map: Added menu item Send Bug Report.
;; 2006/04/03 dadams
;;     icicle-define-icicle-mode-map: Added icicle-toggle-(regexp-quote|incremental-completion).
;; 2006/03/16 dadams
;;     icicle-mode: Turn on minibuffer-indicate-depth-mode (Emacs 22 only).
;;     Added soft require of minibuf-depth.el for Emacs 22.
;; 2006/03/14 dadams
;;     Do not use icicle-reset-icicle-completing-p as minibuffer-exit-hook.
;; 2006/03/07 dadams
;;     Corrected menu items for icicle-doc (no name regexp input, just doc regexp).
;; 2006/03/05 dadams
;;     Moved here from icicle-opt.el: icicle-mode, icicle-mode-hook.
;;     Moved here from icicle-fn.el: icicle-mode-map.
;;     Added: icicle-define-icicle-mode-map.
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

(when (fboundp 'define-minor-mode) (require 'minibuf-depth nil t)) ; Emacs 22

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; User Options (alphabetical)

;; Emacs 20 only
(unless (fboundp 'define-minor-mode)
  (defcustom icicle-mode nil
    "*Toggle minibuffer input completion and cycling.
Setting this variable directly does not take effect;
use either \\[customize] or command `icy-mode' (aka `icicle-mode')."
    :set (lambda (symbol value) (icicle-mode (if value 1 -1)))
    :initialize 'custom-initialize-default
    :type 'boolean
    :group 'icicles
    :require 'icicles))

;;;###autoload
(defcustom icicle-mode-hook nil
  "*Functions run after entering and exiting Icicle mode."
  :type 'hook :group 'icicles)


;;; Internal variables (alphabetical) ----------------------

(defvar icicle-mode-map nil
  "Keymap for Icicle mode.  These are top-level key bindings.
See also `icicle-rebind-completion-maps' for minibuffer bindings.")

(defvar icicle-touche-pas-aux-menus-flag) ; Quiet the byte-compiler


;;; Icicle mode command ------------------------------------

(defalias 'icy-mode 'icicle-mode)

;; Main command.  Inspired from `icomplete-mode'.
;;;###autoload
(if (fboundp 'define-minor-mode)
    ;; Emacs 21+ ------------
    (eval '(define-minor-mode icicle-mode
            "Icicle mode: Toggle minibuffer input completion and cycling.
Non-nil prefix ARG turns mode on if ARG > 0, else turns it off.
Icicle mode binds several keys in the minibuffer.

The following top-level commands are also available in Icicle mode:

`icicle-bookmark'                      - Jump to bookmark(s)
`icicle-buffer'(`-other-window')       - Switch to buffer(s)
`icicle-buffer-config'                 - Pick `icicle-buffer' options
`icicle-clear-option'                  - Set binary option(s) to nil
`icicle-color-theme'                   - Change color theme
`icicle-compilation-search'            - `icicle-search' and show hits
`icicle-complete-thesaurus-entry'      - Complete word using thesaurus
`icicle-completion-help'               - Give bindings for completion
`icicle-customize-icicles-group'       - Customize options and faces
`icicle-delete-file'                   - Delete file(s)/directory(s)
`icicle-doc'                           - Show function/variable doc(s)
`icicle-execute-extended-command'      - `execute-extended-command' +
`icicle-find-file'(`-other-window')    - Visit file(s)/directory(s)
`icicle-font'                          - Change font of frame
`icicle-frame-bg'                      - Change background of frame
`icicle-frame-fg'                      - Change foreground of frame
`icicle-fundoc'                        - Show function description(s)
`icicle-imenu'                         - Navigate among Imenu entries
`icicle-insert-thesaurus-entry'        - Insert thesaurus entry(s)
`icicle-locate-file'(`-other-window')  - Visit file(s) in a directory
`icicle-map'                           - Apply function to alist items
`icy-mode' or `icicle-mode'            - Toggle Icicle mode
`icicle-occur'                         - `occur' + apropos icompletion
`icicle-recent-file'(`-other-window')  - Open recently used file(s)
`icicle-reset-option-to-nil'           - Set binary option(s) to nil
`icicle-save-string-to-variable'       - Save text for use with `C-='
`icicle-search'                        - Search with regexps & cycling
`icicle-send-bug-report'               - Send Icicles bug report
`icicle-set-option-to-t'               - Set binary option(s) to t
`icicle-toggle-ignored-extensions'     - Toggle ignoring file suffixes
`icicle-toggle-incremental-completion' - Toggle apropos icompletion
`icicle-toggle-option'                 - Toggle binary user option(s)
`icicle-toggle-sorting'                - Toggle sorting of completions
`icicle-vardoc'                        - Show variable description(s)

For more information, use `\\<minibuffer-local-completion-map>\\[icicle-completion-help]' \
when the minibuffer is active."
            :global t :group 'icicles :lighter " Icy" :init-value nil
            (cond (icicle-mode
                   (unless icicle-mode-map (icicle-define-icicle-mode-map))
                   (add-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
                   (add-hook 'minibuffer-exit-hook     'icicle-cancel-*Help*-redirection)
                   (add-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
                   (add-hook 'icicle-post-command-hook 'icicle-activate-mark 'append)
                   ;; The pre- and post-command hooks are local to the minibuffer,
                   ;; So they are added in `icicle-minibuffer-setup', not here.
                   ;; Nevertheless, they are removed here when Icicle mode is exited.
                   (add-hook 'isearch-mode-hook        'icicle-bind-isearch-keys)
                   (add-hook 'completion-setup-hook    'icicle-set-calling-cmd 'append)
                   (add-hook 'kill-emacs-hook          'icicle-control-reminder-prompt)
                   (icicle-undo-std-completion-faces)
                   (icicle-redefine-std-completion-fns)
                   (icicle-redefine-standard-commands)
                   (icicle-redefine-standard-options)
                   (when (fboundp 'minibuffer-indicate-depth-mode)
                     (minibuffer-indicate-depth-mode 99)))
                  (t
                   (remove-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
                   (remove-hook 'minibuffer-exit-hook     'icicle-cancel-*Help*-redirection)
                   (remove-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
                   (remove-hook 'icicle-post-command-hook 'icicle-activate-mark)
                   (remove-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook nil)
                   (remove-hook 'post-command-hook        'icicle-run-icicle-post-command-hook nil)
                   (remove-hook 'isearch-mode-hook        'icicle-bind-isearch-keys)
                   (remove-hook 'completion-setup-hook    'icicle-set-calling-cmd)
                   (remove-hook 'kill-emacs-hook          'icicle-control-reminder-prompt)
                   ;; $$$ Should restore standard completion faces here.
                   (icicle-restore-std-completion-fns)
                   (icicle-restore-standard-commands)
                   (icicle-restore-standard-options)
                   (when (fboundp 'minibuffer-indicate-depth-mode)
                     (minibuffer-indicate-depth-mode -99))))
            (message "Turning %s Icicle mode..." (if icicle-mode "ON" "OFF"))
            (icicle-rebind-completion-maps icicle-mode)
            (message "Turning %s Icicle mode...done" (if icicle-mode "ON" "OFF"))))

  ;; Emacs 20 ------------
  (defun icicle-mode (&optional arg)
    "Icicle mode: Toggle minibuffer input completion and cycling.
Non-nil prefix ARG turns mode on if ARG > 0, else turns it off.
Icicle mode binds several keys in the minibuffer.

The following top-level commands are also available in Icicle mode:

`icicle-bookmark'                      - Jump to bookmark(s)
`icicle-buffer'(`-other-window')       - Switch to buffer(s)
`icicle-buffer-config'                 - Pick `icicle-buffer' options
`icicle-clear-option'                  - Set binary option(s) to nil
`icicle-color-theme'                   - Change color theme
`icicle-compilation-search'            - `icicle-search' and show hits
`icicle-complete-thesaurus-entry'      - Complete word using thesaurus
`icicle-completion-help'               - Give bindings for completion
`icicle-customize-icicles-group'       - Customize options and faces
`icicle-delete-file'                   - Delete file(s)/directory(s)
`icicle-doc'                           - Show function/variable doc(s)
`icicle-execute-extended-command'      - `execute-extended-command' +
`icicle-find-file'(`-other-window')    - Visit file(s)/directory(s)
`icicle-font'                          - Change font of frame
`icicle-frame-bg'                      - Change background of frame
`icicle-frame-fg'                      - Change foreground of frame
`icicle-fundoc'                        - Show function description(s)
`icicle-imenu'                         - Navigate among Imenu entries
`icicle-insert-thesaurus-entry'        - Insert thesaurus entry(s)
`icicle-locate-file'(`-other-window')  - Visit file(s) in a directory
`icicle-map'                           - Apply function to alist items
`icy-mode' or `icicle-mode'            - Toggle Icicle mode
`icicle-occur'                         - `occur' + apropos icompletion
`icicle-recent-file'(`-other-window')  - Open recently used file(s)
`icicle-reset-option-to-nil'           - Set binary option(s) to nil
`icicle-save-string-to-variable'       - Save text for use with `C-='
`icicle-search'                        - Search with regexps & cycling
`icicle-send-bug-report'               - Send Icicles bug report
`icicle-set-option-to-t'               - Set binary option(s) to t
`icicle-toggle-ignored-extensions'     - Toggle ignoring file suffixes
`icicle-toggle-incremental-completion' - Toggle apropos icompletion
`icicle-toggle-option'                 - Toggle binary user option(s)
`icicle-toggle-sorting'                - Toggle sorting of completions
`icicle-vardoc'                        - Show variable description(s)

For more information, use `\\<minibuffer-local-completion-map>\\[icicle-completion-help]' \
when the minibuffer is active."
    (interactive "P")
    (setq icicle-mode (if arg (> (prefix-numeric-value arg) 0) (not icicle-mode)))
    (icicle-rebind-completion-maps icicle-mode)
    (cond (icicle-mode
           (unless icicle-mode-map (icicle-define-icicle-mode-map))
           ;; This is not really necessary after the first time - no great loss.
           (add-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
           (add-hook 'minibuffer-exit-hook     'icicle-cancel-*Help*-redirection)
           (add-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
           (add-hook 'icicle-post-command-hook 'icicle-activate-mark 'append)
           ;; The pre- and post-command hooks are local to the minibuffer,
           ;; So they are added in `icicle-minibuffer-setup', not here.
           ;; Nevertheless, they are removed here when Icicle mode is exited.
           (add-hook 'isearch-mode-hook        'icicle-bind-isearch-keys)
           (add-hook 'completion-setup-hook    'icicle-set-calling-cmd 'append)
           (add-hook 'kill-emacs-hook          'icicle-control-reminder-prompt)
           (icicle-redefine-std-completion-fns)
           (icicle-redefine-standard-commands)
           (icicle-redefine-standard-options)
           (run-hooks 'icicle-mode-hook)
           (message "Icicle mode is now ON"))
          (t
           (remove-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
           (remove-hook 'minibuffer-exit-hook     'icicle-cancel-*Help*-redirection)
           (remove-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
           (remove-hook 'icicle-post-command-hook 'icicle-activate-mark)
           (remove-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook nil)
           (remove-hook 'post-command-hook        'icicle-run-icicle-post-command-hook nil)
           (remove-hook 'isearch-mode-hook        'icicle-bind-isearch-keys)
           (remove-hook 'completion-setup-hook    'icicle-set-calling-cmd)
           (remove-hook 'kill-emacs-hook          'icicle-control-reminder-prompt)
           (icicle-restore-std-completion-fns)
           (icicle-restore-standard-commands)
           (icicle-restore-standard-options)
           (run-hooks 'icicle-mode-hook)
           (message "Icicle mode is now OFF"))))
  (add-to-list 'minor-mode-alist '(icicle-mode " Icy")))

(defun icicle-define-icicle-mode-map ()
  "Define `icicle-mode-map'."
  (let ((map (make-sparse-keymap "Icicles")))
    (define-key map [menu-bar] (make-sparse-keymap))
    (define-key map [menu-bar icicles] (cons "Icicles" map))
    (define-key map [icicle-mode] '("Turn Off Icicle Mode" . icicle-mode))
    (define-key map [icicle-report-bug] '("Send Bug Report" . icicle-send-bug-report))
    (define-key map [icicle-help] '("Help" . icicle-completion-help))
    (define-key map [icicle-separator-last] '("--"))
    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-frames-menu)) ; Defined in `menu-bar+.el'.
           (define-key menu-bar-frames-menu [icicle-separator-frame] '("--"))
           (define-key menu-bar-frames-menu [icicle-font] '("[Icy] Change Font" . icicle-font))
           (define-key menu-bar-frames-menu [icicle-frame-fg]
             '("[Icy] Change Foreground..." . icicle-frame-fg))
           (define-key menu-bar-frames-menu [icicle-frame-bg]
             '("[Icy] Change Background..." . icicle-frame-bg)))
          (t
           (define-key map [icicle-font] '("Change Font of Frame..." . icicle-font))
           (define-key map [icicle-frame-fg]
             '("Change Foreground of Frame..." . icicle-frame-fg))
           (define-key map [icicle-frame-bg]
             '("Change Background of Frame..." . icicle-frame-bg))
           (define-key map [icicle-separator-frame] '("--"))))
    (put 'icicle-font 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (put 'icicle-frame-bg 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (put 'icicle-frame-fg 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-describe-menu)) ; Use Describe menu, if available.
           (define-key menu-bar-describe-menu [icicle-separator-doc] '("--"))
           (define-key menu-bar-describe-menu [icicle-doc]
             '("[Icy] Function or Var with Doc..." . icicle-doc))
           (define-key menu-bar-describe-menu [icicle-fundoc]
             '("[Icy] Function with Name, Doc..." . icicle-fundoc))
           (define-key menu-bar-describe-menu [icicle-vardoc]
             '("[Icy] Variable with Name, Doc..." . icicle-vardoc)))
          (t
           (define-key map [icicle-doc]
             '("Describe Function or Var with Doc..." . icicle-doc))
           (define-key map [icicle-fundoc]
             '("Describe Function with Name, Doc..." . icicle-fundoc))
           (define-key map [icicle-vardoc]
             '("Describe Variable with Name, Doc..." . icicle-vardoc))
           (define-key map [icicle-separator-doc] '("--"))))
    (define-key map [icicle-color-theme] '("Choose Color Theme..." . icicle-color-theme))
    (put 'icicle-color-theme 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (define-key map [icicle-complete-thesaurus-entry]
      '("Complete with Thesaurus..." . icicle-complete-thesaurus-entry))
    (put 'icicle-complete-thesaurus-entry 'menu-enable
         '(and icicle-mode (not buffer-read-only) (boundp 'synonyms-obarray)))
    (define-key map [icicle-separator-misc] '("--"))
    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-apropos-menu)) ; Use Apropos menu, if available.
           (define-key menu-bar-apropos-menu [icicle-separator-apropos] '("--"))
           (define-key menu-bar-apropos-menu [icicle-apropos-zippy]
             '("[Icy] Zippy..." . icicle-apropos-zippy))
           (cond ((fboundp 'apropos-option)
                  (define-key menu-bar-apropos-menu [icicle-apropos]
                    '("[Icy] Symbols..." . icicle-apropos))
                  (define-key menu-bar-apropos-menu [icicle-apropos-function]
                    '("[Icy] Functions..." . icicle-apropos-function))
                  (define-key menu-bar-apropos-menu [icicle-apropos-variable]
                    '("[Icy] Variables..." . icicle-apropos-variable))
                  (define-key menu-bar-apropos-menu [icicle-apropos-option]
                    '("[Icy] Options..." . icicle-apropos-option))
                  (define-key menu-bar-apropos-menu [icicle-apropos-command]
                    '("[Icy] Commands..." . icicle-apropos-command)))
                 (t
                  (define-key menu-bar-apropos-menu [icicle-apropos-variable]
                    '("[Icy] Variables..." . icicle-apropos-variable))))
           (define-key menu-bar-apropos-menu [icicle-apropos-command]
             '("[Icy] Commands..." . icicle-apropos-command)))
          (t
           (define-key map [icicle-apropos-zippy] '("Apropos Zippy..." . icicle-apropos-zippy))
           (cond ((fboundp 'apropos-option)
                  (define-key map [icicle-apropos]
                    '("Apropos..." . icicle-apropos))
                  (define-key map [icicle-apropos-function]
                    '("Apropos Functions..." . icicle-apropos-function))
                  (define-key map [icicle-apropos-variable]
                    '("Apropos Variables..." . icicle-apropos-variable))
                  (define-key map [icicle-apropos-option]
                    '("Apropos Options..." . icicle-apropos-option))
                  (define-key map [icicle-apropos-command]
                    '("Apropos Commands..." . icicle-apropos-command)))
                 (t
                  (define-key map [icicle-apropos-variable]
                    '("Apropos Variables..." . icicle-apropos-variable))
                  (define-key map [icicle-apropos-command]
                    '("Apropos Commands..." . icicle-apropos-command))))
           (define-key map [icicle-separator-apropos] '("--"))))
    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-options-menu)) ; Use Options menu, if available.
           (define-key menu-bar-options-menu [icicle-separator-toggle] '("--"))
           (define-key menu-bar-options-menu [icicle-set-option-to-t]
             '("[Icy] Turn On Option..." . icicle-set-option-to-t))
           (define-key menu-bar-options-menu [icicle-reset-option-to-nil]
             '("[Icy] Turn Off Option..." . icicle-reset-option-to-nil))
           (define-key menu-bar-options-menu [icicle-toggle-option]
             '("[Icy] Toggle Option..." . icicle-toggle-option))
           (define-key menu-bar-options-menu [icicle-toggle-regexp-quote]
             '("[Icy] Toggle Escaping Special Chars" . icicle-toggle-regexp-quote))
           (define-key menu-bar-options-menu [icicle-toggle-incremental-completion]
             '("[Icy] Toggle Incremental Completion" . icicle-incremental-completion))
           (define-key menu-bar-options-menu [icicle-toggle-sorting]
             '("[Icy] Toggle Completion Sorting" . icicle-toggle-sorting))
           (define-key menu-bar-options-menu [icicle-toggle-ignore]
             '("[Icy] Toggle Ignored File Extensions" . icicle-toggle-ignored-extensions)))
          (t
           (define-key map [icicle-set-option-to-t]
             '("Turn On Option..." . icicle-set-option-to-t))
           (define-key map [icicle-reset-option-to-nil]
             '("Turn Off Option..." . icicle-reset-option-to-nil))
           (define-key map [icicle-toggle-option] '("Toggle Option..." . icicle-toggle-option))
           (define-key map [icicle-toggle-sorting]
             '("Toggle Completion Sorting" . icicle-toggle-sorting))
           (define-key map [icicle-toggle-ignore]
             '("Toggle Ignored File Extensions" . icicle-toggle-ignored-extensions))
           (define-key map [icicle-separator-toggle] '("--"))))
    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-file-menu)) ; Use File menu, if available.
           (define-key menu-bar-file-menu [icicle-file-separator] '("--"))
           (define-key menu-bar-file-menu [icicle-delete-file]
             '("[Icy] Delete File..." . icicle-delete-file))
           (when (condition-case nil (require 'recentf) (error nil))
             (define-key menu-bar-file-menu [icicle-recent-file-other-window]
               '("[Icy] Open Recent File Other Window..." . icicle-recent-file-other-window))
             (define-key menu-bar-file-menu [icicle-recent-file]
               '("[Icy] Open Recent File..." . icicle-recent-file)))
           (define-key menu-bar-file-menu [icicle-find-file-other-window]
             '("[Icy] Open File or Directory Other Window..." . icicle-find-file-other-window))
           (define-key menu-bar-file-menu [icicle-find-file]
             '("[Icy] Open File or Directory..." . icicle-find-file)))
          (t
           (define-key map [icicle-delete-file] '("Delete File..." . icicle-delete-file))
           (when (condition-case nil (require 'recentf) (error nil))
             (define-key map [icicle-recent-file-other-window]
               '("Open Recent File Other Window..." . icicle-recent-file-other-window))
             (define-key map [icicle-recent-file]
               '("Open Recent File..." . icicle-recent-file)))
           (define-key map [icicle-find-file-other-window]
             '("Open File or Directory Other Window..." . icicle-find-file-other-window))
           (define-key map [icicle-find-file]
             '("Open File or Directory ..." . icicle-find-file))))
    (put 'icicle-delete-file 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (put 'icicle-find-file 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (put 'icicle-find-file-other-window 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (when (condition-case nil (require 'recentf) (error nil))
      (put 'icicle-recent-file 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
      (put 'icicle-recent-file-other-window 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
    (define-key map [icicle-add-buffer-config]
      '("New Buffer Configuration..." . icicle-add-buffer-config))
    (define-key map [icicle-buffer-config]
      '("Choose Buffer Configuration..." . icicle-buffer-config))
    (put 'icicle-buffer-config 'menu-enable '(and icicle-mode icicle-buffer-configs))
    (define-key map [icicle-remove-buffer-candidate]
      '("Don't Always Include Buffer..." . icicle-remove-buffer-candidate))
    (put 'icicle-remove-buffer-candidate 'menu-enable '(and icicle-mode icicle-buffer-extras))
    (define-key map [icicle-add-buffer-candidate]
      '("Always Include Buffer..." . icicle-add-buffer-candidate))
    (define-key map [icicle-buffer-other-window]
      '("Switch To Buffer Other Window..." . icicle-buffer-other-window))
    (put 'icicle-buffer-other-window 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (define-key map [icicle-buffer] '("Switch To Buffer..." . icicle-buffer))
    (put 'icicle-buffer 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-bookmark-map)) ; Use Bookmarks menu, if available.
           (require 'bookmark)          ; `bookmark-buffer-name' is not autoloaded.
           (define-key menu-bar-bookmark-map [icicle-bookmark]
             '("[Icy] Jump to Bookmark Using Icicles..." . icicle-bookmark)))
          (t
           (define-key map [icicle-bookmark] '("Jump To Bookmark..." . icicle-bookmark))
           (define-key map [icicle-separator-bookmark-buffer] '("--"))))
    (put 'icicle-bookmark 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-search-menu)) ; Use Search menu, if available.
           (define-key menu-bar-search-menu [icicle-separator-search] '("--"))
           (define-key menu-bar-search-menu [icicle-compilation-search]
             '("[Icy] Search Compilation/Grep Hits (Regexp)..." . icicle-compilation-search))
           (define-key menu-bar-search-menu [icicle-search]
             '("[Icy] Search (Regexp)..." . icicle-search)))
          (t
           (define-key map [icicle-compilation-search]
             '("Search Compilation/Grep Hits (Regexp)..." . icicle-compilation-search))
           (define-key map [icicle-search] '("Search (Regexp)..." . icicle-search))))
    (put 'icicle-compilation-search 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
           (condition-case nil (eq (current-buffer) (compilation-find-buffer)) (error nil))))
    (put 'icicle-search 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (push (cons 'icicle-mode map) minor-mode-map-alist)
    (setq icicle-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-mode.el ends here
