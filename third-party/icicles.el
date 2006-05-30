;;; icicles.el --- Minibuffer input completion and cycling.
;;
;; Filename: icicles.el
;; Description: Minibuffer completion and cycling.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2006, Drew Adams, all rights reserved.
;; Created: Tue Aug  1 14:21:16 1995
;; Version: 22.0
;; Last-Updated: Fri May 26 18:43:30 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 17986
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `cl', `color-theme', `cus-face',
;;   `easymenu', `hexrgb', `icicles-cmd', `icicles-face',
;;   `icicles-fn', `icicles-keys', `icicles-mac', `icicles-menu',
;;   `icicles-mode', `icicles-opt', `icicles-var', `misc-fns',
;;   `subr-21', `thingatpt', `thingatpt+', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Minibuffer input completion and cycling of completion candidates.
;;
;;  Input completion takes as input a string and returns a name that
;;  contains the input string.  This library enables minibuffer
;;  cycling of completion candidates, and provides additional support
;;  for input completion.
;;
;;  Two kinds of completion are offered here, which are distinguished
;;  by how the input string is matched against the completed name:
;;
;;   - Prefix completion - The input string is a prefix of the
;;                         completed name.  This is the usual Emacs
;;                         completion.
;;
;;   - Apropos completion - The input string is a regular expression
;;                          that matches somewhere (anywhere) within
;;                          the completed name.  You can think of the
;;                          name as having been returned by `apropos'
;;                          (except it also works for file and buffer
;;                          names).
;;
;;  See also: Library `icicles-menu.el', which lets you execute menu
;;            commands, cycling and completing them.
;;
;;
;;  To use this library:
;;
;;    Add this to your initialization file (~/.emacs or ~/_emacs):
;;
;;      (require 'icicles) ; Load this library.
;;      (icicle-mode 1)    ; Turn on Icicle mode.
;;
;;    It is best to add this code *after* any code that creates or
;;    changes key bindings, so Icicles can pick up all or your key
;;    definitions (bindings).
;;
;;    You will need all of these libraries (loaded by `icicles.el'):
;;
;;      `icicles-cmd.el'
;;      `icicles-face.el'
;;      `icicles-fn.el'
;;      `icicles-mac.el'
;;      `icicles-mode.el'
;;      `icicles-opt.el'
;;      `icicles-var.el'
;;
;;    If you want Icicles to bind some top-level Icicles commands for
;;    you, then you will also need library `icicles-keys.el'.  If you
;;    do not want this, then you will need to set (e.g. customize)
;;    user option `icicle-bind-top-level-commands-flag' to nil.
;;
;;    After startup, you can turn Icicle mode on or off at any time
;;    interactively, using command `icy-mode' (aka `icicle-mode' -
;;    prefix `icy' is unique to this command, so it is easier to
;;    complete).
;;
;;    Note: If you turn on Icicle mode in your init file, it's best to
;;          do so as late as possible - after you or any libraries
;;          that you load do any key binding.  This is because Icicles
;;          uses the current global key bindings to determine which
;;          keys to bind for minibuffer completion and cycling.  To
;;          pick up the latest bindings at any time, you can of course
;;          enter Icicle mode interactively using command `icy-mode'
;;          (if necessary, exit, then re-enter).

;;
;;
;;  Things Defined in Icicles
;;  -------------------------
;;
;;  Key bindings defined in Icicles: see "Key Bindings", below.
;;
;;  Macros defined in Icicles:
;;
;;    `icicle-define-command', `icicle-define-file-command'.
;; 
;;  Commands defined in Icicles -
;;
;;   Commands to be used mainly at top level:
;;
;;    `icicle-add-buffer-candidate', `icicle-add-buffer-config',
;;    `icicle-add/update-saved-completion-set', `icicle-apropos',
;;    `icicle-apropos-command', `icicle-apropos-function',
;;    `icicle-apropos-option', `icicle-apropos-variable',
;;    `icicle-apropos-zippy', `icicle-bookmark', `icicle-buffer',
;;    `icicle-buffer-config', `icicle-buffer-list',
;;    `icicle-buffer-other-window', `icicle-color-theme',
;;    `icicle-compilation-search', `icicle-complete-thesaurus-entry',
;;    `icicle-clear-option', `icicle-customize-icicles-group',
;;    `icicle-dabbrev-completion', `icicle-delete-file',
;;    `icicle-dired-saved-file-candidates',
;;    `icicle-dired-saved-file-candidates-other-window', `icicle-doc',
;;    `icicle-execute-extended-command', `icicle-find-file',
;;    `icicle-find-file-other-window', `icicle-font',
;;    `icicle-frame-bg', `icicle-frame-fg', `icicle-fundoc',
;;    `icicle-imenu', `icicle-insert-string-from-variable',
;;    `icicle-insert-thesaurus-entry', `icicle-kill-buffer',
;;    `icicle-lisp-complete-symbol', `icicle-locate-file',
;;    `icicle-locate-file-other-window', `icicle-map' `icicle-mode',
;;    `icy-mode', `icicle-occur', `icicle-recent-file',
;;    `icicle-recent-file-other-window',
;;    `icicle-remove-buffer-candidate', `icicle-remove-buffer-config',
;;    `icicle-remove-saved-completion-set',
;;    `icicle-repeat-complex-command', `icicle-reset-option-to-nil',
;;    `icicle-save-string-to-variable', `icicle-search',
;;    `icicle-search-highlight-cleanup', `icicle-set-option-to-t',
;;    `icicle-toggle-ignored-extensions',
;;    `icicle-toggle-ignored-space-prefix',
;;    `icicle-toggle-incremental-completion',
;;    `icicle-toggle-regexp-quote', `icicle-toggle-sorting',
;;    `icicle-vardoc', `toggle-icicle-ignored-extensions',
;;    `toggle-icicle-ignored-space-prefix',
;;    `toggle-icicle-incremental-completion',
;;    `toggle-icicle-regexp-quote', `toggle-icicle-sorting'.
;;
;;   Commands to be used mainly in the minibuffer or *Completions*:
;; 
;;    `icicle-abort-minibuffer-input', `icicle-apropos-complete',
;;    `icicle-apropos-complete-no-display',
;;    `icicle-apropos-complete-and-exit',
;;    `icicle-backward-delete-char-untabify',
;;    `icicle-backward-kill-paragraph',
;;    `icicle-backward-kill-sentence', `icicle-backward-kill-sexp',
;;    `icicle-backward-kill-word', `icicle-candidate-action',
;;    `icicle-candidate-set-complement',
;;    `icicle-candidate-set-define',
;;    `icicle-candidate-set-difference',
;;    `icicle-candidate-set-intersection',
;;    `icicle-candidate-set-retrieve',
;;    `icicle-candidate-set-retrieve-from-cache-file',
;;    `icicle-candidate-set-retrieve-from-variable',
;;    `icicle-candidate-set-save',
;;    `icicle-candidate-set-save-to-cache-file',
;;    `icicle-candidate-set-save-to-variable',
;;    `icicle-candidate-set-swap', `icicle-candidate-set-union',
;;    `icicle-choose-completion-string', `icicle-completion-help',
;;    `icicle-customize-apropos', `icicle-customize-apropos-faces',
;;    `icicle-customize-apropos-groups',
;;    `icicle-customize-apropos-options',
;;    `icicle-delete-backward-char', `icicle-delete-char',
;;    `icicle-delete-windows-on', `icicle-erase-minibuffer',
;;    `icicle-erase-minibuffer-or-history-element',
;;    `icicle-exit-minibuffer', `icicle-help-on-candidate',
;;    `icicle-history', `icicle-insert-string-at-point',
;;    `icicle-insert-string-from-variable', `icicle-isearch-complete',
;;    `icicle-keep-only-past-inputs', `icicle-kill-line',
;;    `icicle-kill-paragraph', `icicle-kill-region',
;;    `icicle-kill-region-wimpy', `icicle-kill-sentence',
;;    `icicle-kill-sexp', `icicle-kill-word',
;;    `icicle-minibuffer-complete-and-exit',
;;    `icicle-mouse-candidate-action',
;;    `icicle-mouse-choose-completion',
;;    `icicle-move-to-next-completion',
;;    `icicle-move-to-previous-completion',
;;    `icicle-narrow-candidates', `icicle-next-apropos-candidate',
;;    `icicle-next-apropos-candidate-action', `icicle-next-line',
;;    `icicle-next-prefix-candidate',
;;    `icicle-next-prefix-candidate-action', `icicle-prefix-complete',
;;    `icicle-prefix-complete-no-display',
;;    `icicle-previous-apropos-candidate',
;;    `icicle-previous-apropos-candidate-action',
;;    `icicle-previous-line', `icicle-previous-prefix-candidate',
;;    `icicle-previous-prefix-candidate-action',
;;    `icicle-retrieve-candidates-from-set',
;;    `icicle-retrieve-last-input', `icicle-self-insert',
;;    `icicle-send-bug-report', `icicle-switch-to-Completions-buf',
;;    `icicle-switch-to-completions', `icicle-switch-to-minibuffer',
;;    `icicle-toggle-ignored-extensions',
;;    `icicle-toggle-ignored-space-prefix',
;;    `icicle-toggle-incremental-completion',
;;    `icicle-toggle-regexp-quote', `icicle-toggle-sorting',
;;    `icicle-transpose-chars', `icicle-transpose-sexps',
;;    `icicle-transpose-words', `icicle-yank', `icicle-yank-pop',
;;    `old-completing-read', `old-choose-completion-string',
;;    `old-completion-setup-function', `old-exit-minibuffer',
;;    `old-minibuffer-complete-and-exit', `old-read-file-name',
;;    `old-switch-to-completions', `toggle-icicle-ignored-extensions',
;;    `toggle-icicle-ignored-space-prefix',
;;    `toggle-icicle-incremental-completion',
;;    `toggle-icicle-regexp-quote', `toggle-icicle-sorting'.
;;
;;  Faces defined in Icicles (in Custom group `icicles'):
;;
;;    `icicle-common-match-highlight-Completions',
;;    `icicle-complete-input', `icicle-current-candidate-highlight',
;;    `icicle-historical-candidate',
;;    `icicle-match-highlight-Completions',
;;    `icicle-match-highlight-minibuffer', `icicle-prompt-suffix',
;;    `icicle-search-current-input',
;;    `icicle-search-main-regexp-current',
;;    `icicle-search-main-regexp-others'.
;;
;;  User options defined in Icicles (in Custom group `icicles'):
;;
;;    `icicle-arrows-respect-completion-type-flag',
;;    `icicle-bind-top-level-commands-flag', `icicle-buffer-configs',
;;    `icicle-buffer-extras',
;;    `icicle-buffer-ignore-space-prefix-flag',
;;    `icicle-buffer-match-regexp', `icicle-buffer-no-match-regexp',
;;    `icicle-buffer-predicate', `icicle-buffer-require-match-flag'
;;    `icicle-buffer-sort', `icicle-change-region-background-flag',
;;    `icicle-color-themes', `icicle-Completions-frame-at-right-flag',
;;    `icicle-cycle-into-subdirs-flag',
;;    `icicle-default-thing-insertion',
;;    `icicle-expand-input-to-common-match-flag',
;;    `icicle-ignore-space-prefix-flag',
;;    `icicle-incremental-completion-delay',
;;    `icicle-incremental-completion-flag',
;;    `icicle-incremental-completion-threshold',
;;    `icicle-init-value-flag', `icicle-list-join-string',
;;    `icicle-mark-position-in-candidate',
;;    `icicle-minibuffer-setup-hook', `icicle-mode',
;;    `icicle-mode-hook', `icicle-point-position-in-candidate',
;;    `icicle-redefine-standard-commands-flag',
;;    `icicle-regexp-quote-flag', `icicle-regexp-search-ring-max',
;;    `icicle-region-background', `icicle-reminder-prompt-flag',
;;    `icicle-require-match-flag', `icicle-saved-completion-sets',
;;    `icicle-search-highlight-all-flag',
;;    `icicle-search-cleanup-flag', `icicle-search-hook',
;;    `icicle-search-ring-max',
;;    `icicle-show-Completions-initially-flag',
;;    `icicle-sort-function', `icicle-thing-at-point-functions',
;;    `icicle-touche-pas-aux-menus-flag', `icicle-update-input-hook'
;;    `icicle-word-completion-key'.
;;
;;  Non-interactive functions in Icicles:
;;
;;    `icicle-activate-mark', `icicle-apropos-candidates',
;;    `icicle-apropos-complete-1',
;;    `icicle-barf-if-outside-minibuffer',
;;    `icicle-barf-if-outside-Completions', `icicle-binary-option-p',
;;    `icicle-bind-completion-keys', `icicle-bind-isearch-keys',
;;    `icicle-buffer-sort-*...*-last', `icicle-candidate-set-1',
;;    `icicle-cancel-*Help*-redirection', `icicle-clear-minibuffer',
;;    `icicle-completing-p', `icicle-completing-read',
;;    `icicle-completion-setup-function',
;;    `icicle-current-completion-in-Completions',
;;    `icicle-define-icicle-mode-map',
;;    `icicle-delete-file-or-directory', `icicle-delete-if',
;;    `icicle-delete-if-not', `icicle-delete-whitespace-from-string',
;;    `icicle-display-Completions',
;;    `icicle-display-candidates-in-Completions',
;;    `icicle-execute-extended-command-1', `icicle-file-directory-p',
;;    `icicle-file-name-apropos-candidates',
;;    `icicle-file-name-directory-w-default',
;;    `icicle-file-name-input-p', `icicle-file-name-nondirectory',
;;    `icicle-file-name-prefix-candidates', `icicle-file-readable-p',
;;    `icicle-file-writable-p', `icicle-files-within',
;;    `icicle-filter-alist', `icicle-filter-wo-input',
;;    `icicle-find-file-w-wildcards',
;;    `icicle-find-file-other-window-w-wildcards', `icicle-frames-on',
;;    `icicle-highlight-complete-input', `icicle-imenu-in-buffer-p',
;;    `icicle-increment-cand-nb+signal-end',
;;    `icicle-increment-color-hue', `icicle-increment-color-value',
;;    `icicle-insert-input', `icicle-insert-thesaurus-entry-cand-fn',
;;    `icicle-insert-thing', `icicle-isearch-resume',
;;    `icicle-kill-a-buffer', `icicle-map-action',
;;    `icicle-maybe-sort-and-strip-candidates',
;;    `icicle-minibuffer-contents',
;;    `icicle-minibuffer-contents-from-minibuffer',
;;    `icicle-minibuffer-prompt-end', `icicle-minibuffer-setup',
;;    `icicle-msg-maybe-in-minibuffer',
;;    `icicle-nb-of-candidate-in-Completions',
;;    `icicle-next-candidate', `icicle-non-whitespace-string-p',
;;    `icicle-place-cursor', `icicle-place-overlay',
;;    `icicle-prefix-candidates', `icicle-prefix-complete-1',
;;    `icicle-read-file-name', `icicle-read-from-minibuffer',
;;    `icicle-read-from-minibuf-nil-default', `icicle-read-string',
;;    `icicle-rebind-completion-maps', `icicle-recompute-candidates',
;;    `icicle-redefine-standard-commands',
;;    `icicle-redefine-std-completion-fns',
;;    `icicle-redefine-standard-options', `icicle-remap',
;;    `icicle-remove-dots', `icicle-remove-duplicates',
;;    `icicle-remove-property', `icicle-restore-completion-keys',
;;    `icicle-restore-region-face',
;;    `icicle-restore-standard-commands',
;;    `icicle-restore-std-completion-fns',
;;    `icicle-restore-standard-options',
;;    `icicle-run-icicle-post-command-hook',
;;    `icicle-run-icicle-pre-command-hook',
;;    `icicle-save-or-restore-input',
;;    `icicle-scroll-or-update-Completions', `icicle-search-action',
;;    `icicle-search-highlight-all-input-matches',
;;    `icicle-select-minibuffer-contents' `icicle-set-calling-cmd',
;;    `icicle-set-difference', `icicle-set-intersection',
;;    `icicle-set-union', `icicle-signum',
;;    `icicle-sort-and-strip-ignored',
;;    `icicle-sort-case-insensitively', `icicle-sort-dirs-last',
;;    `icicle-undo-std-completion-faces', `icicle-unmap',
;;    `icicle-unsorted-apropos-candidates',
;;    `icicle-unsorted-file-name-apropos-candidates',
;;    `icicle-unsorted-file-name-prefix-candidates',
;;    `icicle-unsorted-prefix-candidates',
;;    `icicle-update-completions',
;;    `icicle-update-ignored-extensions-regexp'.
;;
;;  Internal variables defined in Icicles:
;;
;;    `icicle-candidate-nb', `icicle-cmd-calling-for-completion',
;;    `icicle-common-match-string', `icicle-complete-input-overlay',
;;    `icicle-completion-candidates' `icicle-completion-help-string',
;;    `icicle-current-completion-candidate-overlay',
;;    `icicle-current-input', `icicle-current-regexp-input',
;;    `icicle-default-directory',
;;    `icicle-default-thing-insertion-flipped-p',
;;    `icicle-extra-candidates', `icicle-icompleting-p',
;;    `icicle-ignored-extensions', `icicle-ignored-extensions-regexp',
;;    `icicle-incremental-completion-p', `icicle-initial-value',
;;    `icicle-insert-string-at-pt-end',
;;    `icicle-insert-string-at-pt-start',
;;    `icicle-last-completion-candidate',
;;    `icicle-last-completion-command', `icicle-last-input',
;;    `icicle-last-sort-function', `icicle-menu-items-alist',
;;    `icicle-mode-map', `icicle-must-match-regexp',
;;    `icicle-must-not-match-regexp', `icicle-must-pass-predicate',
;;    `icicle-nb-of-other-cycle-candidates',
;;    `icicle-post-command-hook', `icicle-pre-command-hook',
;;    `icicle-prompt', `icicle-prompt-suffix', `icicle-re-no-dot',
;;    `icicle-saved-candidates-variables-obarray',
;;    `icicle-saved-completion-candidates',
;;    `icicle-saved-ignored-extensions',
;;    `icicle-saved-regexp-search-ring-max',
;;    `icicle-saved-region-background',
;;    `icicle-saved-search-ring-max', `icicle-search-candidates',
;;    `icicle-search-current-overlay', `icicle-search-overlays',
;;    `icicle-search-refined-overlays',
;;    `icicle-successive-grab-count',
;;    `icicle-thing-at-pt-fns-pointer'.
;;
;;  Emacs functions defined in Icicles for older Emacs versions.
;;
;;    `select-frame-set-input-focus'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED in Icicles:
;;
;;  `completing-read'              - (See below and doc string.)
;;  `exit-minibuffer'              - Remove *Completion* window.
;;  `minibuffer-complete-and-exit' - Remove *Completion* window.
;;  `read-file-name'               - (See below and doc string.)
;;  `read-from-minibuffer'         - (See below and doc string.)
;;  `read-string'                  - (See below and doc string.)
;;
;;
;;  ***** NOTE: The following functions defined in `dabbrev.el' have
;;              been REDEFINED in Icicles:
;;
;;  `dabbrev-completion' - Use Icicles completion when you repeat
;;                         (`M-C-/').
;;
;;
;;  ***** NOTE: The following functions defined in `lisp.el' have
;;              been REDEFINED in Icicles:
;;
;;  `lisp-complete-symbol' - Selects *Completions* window even if on
;;                           another frame.
;;
;;
;;  ***** NOTE: The following functions defined in `mouse.el' have
;;              been REDEFINED in Icicles:
;;
;;  `mouse-choose-completion' - Return the number of the completion.
;;
;;
;;  ***** NOTE: The following functions defined in `simple.el' have
;;              been REDEFINED in Icicles:
;;
;;  `choose-completion-string' -
;;     Don't exit minibuffer after `lisp-complete-symbol' completion.
;;  `completion-setup-function' - 1. Put faces on inserted string(s).
;;                                2. Help on help.
;;  `switch-to-completions' - Always selects *Completions* window.
;;
;;  `next-history-element' (advised only) -
;;     Depending on `icicle-init-value-flag', select minibuffer
;;     contents.
;;
;;  `repeat-complex-command' - Use `completing-read' to read command.

;;
;;
;;  Nutshell View
;;  -------------
;;
;;  Load this library, turn on Icicle mode, and you're good to go.
;;  You can turn Icicle mode off or on at any time with command
;;  `icy-mode'.
;;
;;  Here's a sample of what you can do in Icicle mode.
;;
;;  ** Cycle Completion Candidates **
;;
;;   M-x tool `next'
;;
;;  That is, hit the `next' key, which is often labeled "Page Down".
;;  Each time you hit `next', another match for your input (`tool')
;;  replaces it in the minibuffer.
;;
;;   M-x ediff-toggle-use-toolbar `next'
;;   M-x scroll-bar-toolkit-scroll `next'
;;   M-x tool-bar-mode `next'
;;   M-x tooltip-mode `next'
;;   M-x ediff-toggle-use-toolbar ; Back to the beginning
;;
;;  Keys `next' and `prior' ("Page Up") cycle among all of the
;;  commands that contain (match) the minibuffer input - `tool', in
;;  this case.  Just hit `RET' (Return) when you get to the command
;;  you want.
;;
;;  You can use a regular expression, to narrow the field of matching
;;  inputs:
;;
;;   M-x ise.+char `next'
;;   M-x isearch-*-char `next'
;;   M-x isearch-delete-char `next'
;;   ...
;;
;;  ** Display Completion Candidates **
;;
;;  You can display all of the matches for the current minibuffer
;;  input, in the *Completions* buffer, with `S-TAB' (Shift TAB).  So,
;;  for instance, `S-TAB' with `M-x ise.+char' in the minibuffer
;;  displays all commands whose names contain `ise' followed
;;  (somewhere) by `char'.
;;
;;  ** Prefix Completion and Apropos Completion **
;;
;;  You can get the standard Emacs "prefix" completion, instead of
;;  this "apropos completion", by using `TAB' instead of `S-TAB'.  You
;;  can cycle prefix-completion candidates by using the `up' and
;;  `down' arrow keys instead of `next' and `prior'.
;;
;;  ** Chains of Simple Match Patterns **
;;
;;  To see which functions contain `char', `delete', and `back' in
;;  their names, in any order:
;;
;;   C-h f char S-TAB - display all function names that contain
;;   `char'.
;;
;;   M-* delete - narrow that set of names to those that also contain
;;   `delete'.
;;
;;   M-* back - narrow the set of matching names further, to those
;;   that also contain `back'.
;;
;;  This displays a list of functions like this in buffer
;;  `*Completions*' (your list might be somewhat different):
;;
;;    `backward-delete-char', `backward-delete-char-untabify',
;;    `delete-backward-char', `icicle-backward-delete-char-untabify',
;;    `icicle-delete-backward-char',
;;    `quail-conversion-backward-delete-char'
;;
;;  Since you are completing input to `C-h f', you can then cycle to a
;;  name using `next' and hit `RET', or click `mouse-2', to see the
;;  doc for that function. If, instead, you were completing input to
;;  `M-x', you could choose a command to execute. And so on.
;;
;;  The thing to notice here is that you can use `M-*' to input chains
;;  of multiple simple regexps, to narrow down the set of completion
;;  candidates progressively. This is analogous to piping the result
;;  of `grep' to another `grep', and piping that result to another
;;  `grep'...
;;
;;  See "Progressive Completion", below, for more on progressive
;;  completion with `M-*'.
;;
;;  ** Available for Almost Any Input **
;;
;;  All of this works not only for the input of commands, with `M-x',
;;  but for the input of nearly anything.  For instance, you can use
;;  `C-x b' (`switch-to-buffer') and cycle among buffer names.  Or use
;;  `C-h v' (`describe-variable') and cycle among variable names.  It
;;  works whenever a command reads input with completion.
;;
;;  Whenever you're in Icicle mode, you see "Icy" in the mode-line.
;;
;;  ** Component Icicles Libraries **
;;
;;  Icicles is composed of the following libraries.  When you load the
;;  driver library, `icicles.el', the others are all loaded
;;  automatically .
;;
;;    `icicles.el'      - driver library; contains the doc (this!)
;;    `icicles-cmd.el'  - commands (interactive functions)
;;    `icicles-face.el' - faces
;;    `icicles-fn.el'   - non-interactive functions
;;    `icicles-mac.el'  - macros
;;    `icicles-mode.el' - Icicle mode definition
;;    `icicles-opt.el'  - user options (variables)
;;    `icicles-var.el'  - internal variables
;;
;;  Library `icicles-menu.el' is a separate Icicles library.  It does
;;  not require any of the other libraries, and they don't require
;;  `icicles-menu.el', but `icicles-menu.el' is especially useful when
;;  used with the other libraries.
;;
;;  For more (and there is a lot more), read on...

;;
;;
;;  Inserting Text Found Near the Cursor
;;  ------------------------------------
;;
;;  Most of Icicles is about completing text that you type in the
;;  minibuffer against some set of possible completion candidates.
;;  This feature is not.  It is related only in the sense that it is
;;  also about inputting text that is already available elsewhere.
;;
;;  Some Emacs commands provide, as the default value for minibuffer
;;  input, a word or other text at the cursor position (aka "point").
;;  You can insert this default value in the minibuffer with `M-n'.
;;  Icicles option `icicle-init-value-flag' can be used to
;;  automatically insert the default value into the minibuffer as an
;;  initial value, if you prefer that behavior (I do; many people do
;;  not).
;;
;;  Sometimes you would like to use the text at the cursor, but the
;;  command asking for input does not let you retrieve that text as
;;  the default value.  For example, if the text at point is a file
;;  name, you might like to use it with `C-x f' to open that file.
;;  Or, if the text is a URL, you might want to visit it using a Web
;;  browser.
;;
;;  Some Emacs-Lisp libraries, such as `ffap.el', have as their
;;  specific purpose to help you do this.  "Ffap" stands for
;;  `find-file-at-point', the main command in that library.  It tries
;;  to interpret the text at point and "do the right thing" with it:
;;  visit a file, open a URL in a Web browser, and so on.
;;
;;  If you like, you can use library `ffap.el' with Icicles.  All
;;  Icicles features are then available during file-name and URL
;;  completion.  And if you like `ffap.el', you might also like to try
;;  my extension library `ffap-.el'.  However, I personally don't like
;;  some of the ffap behavior.  I like to control which buffer text I
;;  use as minibuffer input and how that text should be interpreted
;;  (file name, URL, and so on).
;;
;;  Icicles provides a simpler way to do this: Just use `M-.' (command
;;  `icicle-insert-string-at-point') in the minibuffer.  It grabs text
;;  at or near the cursor and yanks it into the minibuffer.
;;  Successive uses of `M-.' grab and insert either alternative bits
;;  of text (different text "things") or (by default) successive bits
;;  of text.
;;
;;  For example, suppose that the cursor is at the beginning of the
;;  word "simpler" in the previous paragraph. Then, during minibuffer
;;  input, you use `M-. M-. M-.'. Each time you hit `M-.'  another
;;  word is inserted in the minibuffer:
;;
;;    simpler
;;    simpler way
;;    simpler way to
;;
;;  Option `icicle-thing-at-point-functions' controls which text at or
;;  near the cursor `M-.' inserts into the minibuffer.  It is a cons
;;  cell, that is, an ordered pair:
;;
;;  * The car (first part)is a list of functions that grab different
;;    kinds of strings at or near point.  Any number of functions can
;;    be used.  They are used in sequence by `M-.'.  By default, there
;;    are three functions, which alternatively grab:
;;
;;    1) the symbol or file name at point
;;    2) the word at point
;;    3) the URL at point
;;
;;  * The cdr (second part) is a function that advances point one text
;;    thing.  Each time command `M-.' is used successively, this is
;;    called to grab more things of text (of the same kind).  The
;;    default function grabs successive words.
;;
;;  If either the car or cdr is empty, then the other alone determines
;;  the behavior of `M-.'.  Otherwise, option
;;  `icicle-default-thing-insertion' determines whether the car or the
;;  cdr is used by `icicle-insert-string-at-point'.
;;
;;  For example, if the value of `icicle-default-thing-insertion' is
;;  `more-of-the-same' (the default), then repeated use of `M-.'
;;  inserts successive words into the minibuffer, as shown above.  If
;;  you set `icicle-default-thing-insertion' to `alternatives', then
;;  repeated use of `M-.' inserts a different kind of thing at point:
;;  file name, word, or URL.
;;
;;  You can use `C-u M-.' at any time to temporarily override the
;;  value of `icicle-default-thing-insertion'.  If you use a numeric
;;  prefix argument N (not just plain `C-u'), then it is the same as
;;  using `M-.' N times with `more-of-the-same' as the value of
;;  `icicle-default-thing-insertion'.
;;
;;  And, if the numeric argument is negative, then text is grabbed to
;;  the left of the cursor, instead of to the right.  In the example
;;  above, if you used `M--1 M-. M-. M-.', then the successive
;;  insertions would be as follows:
;;
;;    a
;;    provides a
;;    Icicles provides a
;;
;;  In the case of `alternatives', there are only three possibilities,
;;  by default.  The first function in the list grabs text that has
;;  the syntax of an Emacs-Lisp symbol name, which in practice can
;;  also be a file name or a URL - it can include characters such as
;;  -, /, +, ., :, @, and _.  The second function grabs a word, which
;;  includes letters, ' and -.  The third function grabs a URL, adding
;;  prefix "http://" if needed.  These are the functions used by
;;  default, but you can add to them or replace them.  If you use my
;;  library `thingatpt+.el', then the cursor need not be exactly on
;;  the text - the nearest symbol or word is grabbed.

;;
;;
;;  Background on Input Completion
;;  ------------------------------
;;
;;  This section reviews standard Emacs behavior regarding input
;;  completion. It does not describe any Icicles completion features.
;;
;;  When you are prompted in the minibuffer to enter something, you
;;  are sometimes presented with a default value.  This might be
;;  automatically inserted after the prompt, initially.  If not, you
;;  can retrieve the default value yourself, using `M-n' (Emacs 21 or
;;  later).
;;
;;  Often, there is more than one reasonable default value that might
;;  make sense.  Depending on what you're being asked to enter, these
;;  "candidate default values" might be command names, buffer names,
;;  existing file names, variable names, and so on.
;;
;;  For most Emacs functions that prompt you for input, the person who
;;  wrote the function decided on the reasonable set of default
;;  values, and passed these to an "input-completing function" such as
;;  `completing-read' or `read-file-name', which prompts you and reads
;;  your input.  The programmer also decided whether you will be
;;  *required* to pick one of the default values or you will be free
;;  to enter something else.  The programmer might also have told the
;;  input-completing function to require that your input pass some
;;  special test (predicate).
;;
;;  Be aware that standard Emacs terminology does not refer to such a
;;  set of default values as "default values"; they are called
;;  "completions". By "default value", standard Emacs terminology
;;  means only the single value that you can access via `M-n'. Icicles
;;  refers to all such potential inputs indifferently as "default
;;  values", "completions", "completion candidates", and
;;  "candidates". Whenever completion is not requiring you to pick one
;;  of the available candidates, they are effectively only default
;;  choices.
;;
;;  So, how do you get access to the default values that the
;;  programmer has made available to you, in order to choose one?  You
;;  hit certain keys to complete the current contents of the
;;  minibuffer (excluding the prompt).  This current, partial input is
;;  considered as a prefix of one of the default values, and it is
;;  completed in the minibuffer to the entire default value
;;  (completion).
;;
;;  Keys `TAB', `RET' (Return), and `SPC' (Space) perform different
;;  degrees of this "prefix completion" in standard Emacs.  If you
;;  type a prefix of one of the available default values, you can
;;  complete the value this way in the minibuffer, and then enter
;;  (commit) it, using `RET'.
;;
;;  But if your partial input matches the prefix of more than one
;;  default value, then completion pops up the list of all matching
;;  completions for you to choose from (in buffer *Completions*).  You
;;  choose a candidate by clicking it with `mouse-2' or placing the
;;  cursor on it and hitting `RET'.
;;
;;  Because this is the way you access the default values supplied to
;;  an input-completing function, I call those values
;;  "prefix-completion candidates".  If there is no partial input yet
;;  (empty minibuffer), then the entire list of default values
;;  supplied to the input-completing function appears in the pop-up
;;  *Completions* buffer.  See the Emacs manual (`C-h i') for more on
;;  this general mechanism of prefix completion (called simply
;;  "completion" there).
;;
;;  Calls to `completing-read' and `read-file-name' are not the only
;;  places where input completion is used.  When you use `M-x'
;;  (command `execute-extended-command'), completion is also
;;  available.

;;
;;
;;  Cycling Completions
;;  -------------------
;;
;;  Icicles lets you use the `up' and `down' arrow keys or `C-p' and
;;  `C-n' to cycle through the list of candidate prefix completions
;;  that match whatever input is present in the minibuffer (or all
;;  candidate completions, if there is no input in the minibuffer).
;;  In the minibuffer, each candidate replaces your partial input, in
;;  turn, when you cycle.  The prefix (root) that was completed is
;;  underlined in the minibuffer completion candidate.
;;
;;  Suppose you use `C-x b' (command `switch-to-buffer').  You can
;;  then use `C-n' until the right buffer name appears in the
;;  minibuffer, then hit `RET'.  Or you can type some text that begins
;;  one or more of the buffer names, and then use `C-n' to cycle among
;;  those names that match that prefix.  If there are many candidates,
;;  typing part of the name to narrow the field can save time.
;;
;;  Another example: Suppose you use `C-h v' (`describe-variable') and
;;  type `cal'.  Use `C-n' to cycle among all variables that start
;;  with `cal', until you find the one you want (then hit `RET').
;;
;;  In other words, the current partial input in the minibuffer
;;  determines a matching set of default values, and those are the
;;  values that you can cycle through.  You can at any time erase or
;;  change the partial input - the list of matching candidates
;;  automatically reflects the change.
;;
;;  This also means that it's good to have a quick way to clear the
;;  minibuffer of any input, so Icicles also provides minibuffer key
;;  binding `M-k' to do that.
;;
;;  A visible and audible signal lets you know when you have reached
;;  one end of the list of completion candidates, but you can of
;;  course continue to cycle, wrapping around.
;;
;;  If the completion candidates are already displayed in buffer
;;  *Completions* when you try to cycle among them (because you hit
;;  `TAB'), then the current candidate is highlighted in *Completions*
;;  as you access it in the minibuffer with the `up' and `down' arrow
;;  keys.  If you change the minibuffer input, then the *Completions*
;;  list is updated accordingly, to reflect the new set of matching
;;  candidates.  The root that was completed (the minibuffer input) is
;;  highlighted in each candidate of the *Completions* display.  The
;;  *Completions* window is automatically scrolled as needed, to show
;;  the current candidate.
;;
;;  Don't become a cycling drone!  Input some text to narrow the set
;;  of candidates, before cycling among them to choose one.  This is a
;;  good habit to adopt, generally, in Icicles.  Most of the power of
;;  Icicles comes in your ability to filter a set of candidates.  This
;;  is especially true when it comes to regexp filtering (see "Apropos
;;  Completions", below).
;;
;;  Cycling and filtering work hand in hand. If the set of candidates
;;  is small to begin with, then just cycling might be quick enough -
;;  that is the case if you move among a small set of buffers, for
;;  instance. But with Icicles you can profitably use cycling on even
;;  a very large set of candidates - by filtering the set first. The
;;  reason this is not very practical with vanilla Emacs is that
;;  filtering by a prefix only is not very potent.
;;
;;  Tip: Whenever you type or delete text in the minibuffer, your
;;  partial input is remembered.  When you cycle completion
;;  candidates, your input is replaced by each candidate, but you can
;;  at any time refresh the minibuffer to retrieve what you last
;;  typed.  You do this with `C-l', which is bound in the minibuffer
;;  to command `icicle-retrieve-last-input'.  Editing a completion
;;  candidate that you have cycled into the minibuffer counts as
;;  input.  Editing tells Icicles to remember what is in the
;;  minibuffer as your last real input.  If you want to replace the
;;  candidate and go back to editing the input you had already typed
;;  before cycling, then use `C-l' - don't just delete characters from
;;  the candidate.
;;
;;  If you are an Emacs-Lisp programmer, then you can use
;;  `completing-read' and `read-file-name' to define your own
;;  commands, enabling them to take advantage of Icicles completion
;;  and cycling.  The definition of command `icicle-recent-file' is a
;;  good model to follow.  Emacs has a `recentf-mode' that lets you
;;  open recently accessed files.  But this mode makes you open a file
;;  using a menu interface.  Command `icicle-recent-file' lets you use
;;  the usual `find-file' minibuffer interface, with completion and
;;  cycling among your recent files.  See sections "Defining Icicles
;;  Commands" and "Note to Programmers", below, for more on defining
;;  your own commands with `completing-read' and `read-file-name'.
;;
;;  You can change the keys that are bound to completion-candidate
;;  cycling (`up', `down', `C-p', and `C-n') - see "Customizing Key
;;  Bindings", below.

;;
;;
;;  Traversing Minibuffer Histories
;;  -------------------------------
;;
;;  Perhaps you are already used to accessing past inputs using the
;;  `down' and `up' arrow keys, `C-n' and `C-p', `M-n' and `M-p', or
;;  `next' and `prior'.  If not, try it.  You can go backward and
;;  forward in the minibuffer histories (there are different history
;;  lists for different kinds of input).  You can't really cycle them
;;  (with wraparound), but when you get to one end you can reverse the
;;  direction.
;;
;;  Anyway, the input-cycling behavior that Icicles offers is in
;;  addition to this standard traversal of histories.  Since there
;;  are, by default, several extra pairs of keys used for history
;;  traversal, rebinding some of them to use for Icicles completion is
;;  no real loss.
;;
;;  By default, Icicles rebinds all of the key sequences that you
;;  normally use for commands `next-line' and `previous-line', so that
;;  they perform prefix-completion cycling in the minibuffer.  In
;;  vanilla Emacs, this means keys `down', `up', `C-n', and `C-p'.
;;
;;  Icicles also rebinds `next' and `prior' for apropos-completion
;;  cycling (see below).  You still have `M-n' and `M-p' available to
;;  access past inputs (history).  And the rebindings are only for
;;  minibuffer input; global bindings are not affected.
;;
;;  You can at any time switch back and forth between input-history
;;  traversal (`M-n', `M-p') and prefix completion (`C-n', `C-p' or
;;  `down', `up').
;;
;;  See also (below):
;;
;;  * "History Enhancements" for new ways to use the standard history
;;    lists with Icicles
;;
;;  * "Customizing Key Bindings" for how to change the default Icicles
;;    key bindings

;;
;;
;;  Apropos Completions
;;  -------------------
;;
;;  Icicles offers a new way to complete your partial input in the
;;  minibuffer.  Instead of considering the string of input characters
;;  to be the prefix of various complete names, you can look for names
;;  that match that string anywhere.  This is the single most
;;  important feature that Icicles offers.
;;
;;  This is similar in effect to using command `apropos' to find
;;  "apropos completions" of a string (except it works also for file
;;  and buffer names), so that's the term I use for this.  The more
;;  correct characterization of this is that of the previous
;;  paragraph, however: names that match the given string.
;;
;;  Just as with prefix completion, Icicles lets you cycle among the
;;  apropos candidates.  To do this, you use keys `next' and `prior'
;;  (or `C-v' and `M-v').  The root that was completed is underlined
;;  in the minibuffer completion candidate.
;;
;;  For example, suppose you use `M-x' to enter a command.  You don't
;;  remember the exact command name, but it has something to do with
;;  lines, so you type `M-x line', then hit `next' repeatedly, until
;;  you see the right "line" command - `transpose-lines', perhaps.
;;  Prefix completion cannot find this command, because "line" is not
;;  a prefix of "transpose-lines".
;;
;;  Because `M-x' expects a command name, only command names are
;;  inserted into the minibuffer as the apropos-completion candidates
;;  for `M-x'.  Likewise, in other contexts, where names of other
;;  kinds of object are expected, apropos completion inserts only
;;  names of objects of the appropriate type.  Prefix completion works
;;  the same way.
;;
;;  For example, using `next' and `prior' with `C-x b at' lets you
;;  cycle through all buffers (such as `*scratch*') that have "at" in
;;  their name - only buffer names appear as candidates.
;;
;;  Another example: Suppose you are in Info, reading the Emacs-Lisp
;;  manual, and you want to go to a node (manual section) that
;;  discusses regular expressions (regexps). You could search through
;;  the Table of Contents, or you could search through the Index or
;;  index topics (`i'), or you could search through the manual text
;;  from the beginning (`s').  Or, you could type `g' to use command
;;  `Info-goto-node', type the word `regexp' in the minibuffer, and
;;  then use `next' or `prior' to cycle among all Info nodes with
;;  `regexp' in their name.
;;
;;  Apropos completion uses a regular expression (regexp) as its input
;;  string.  You can type `M-x \bes', for instance, to find commands
;;  with "es" at the start of a word within the command name (`\b'
;;  matches the start of a word).  It will find `eshell-test' and
;;  `color-theme-blue-eshell', but not `count-lines' - "es" does not
;;  start a word in `count-lines'.  Similarly, for file names, buffer
;;  names, and so on.
;;
;;  Prefix completion is actually a special case of apropos
;;  completion, where the regexp starts with "^".  (That is not how it
;;  is implemented, however.)
;;
;;  What if you want to see the list of all completion candidates that
;;  match the minibuffer input? Instead of cycling candidates blindly,
;;  just hit `S-TAB' (Shift TAB) at any time to display the matching
;;  candidates in pop-up buffer *Completions*.  This is analogous to
;;  `TAB' for prefix completion.
;;
;;  Everything said above about the *Completions* buffer for prefix
;;  completion is also true for apropos completion.  It is updated to
;;  reflect the current set of matching candidates, and the current
;;  completion is highlighted.  The root that was completed is
;;  highlighted within each candidate (first occurrence only).  Root
;;  highlighting is more important in the case of apropos completion,
;;  because the match position is different in different candidates.
;;  In the case of apropos completion, the root is not the input
;;  string, taken literally, but the part of a candidate that the
;;  input matches.  See "*Completions* Display", below, for additional
;;  ways to use the minibuffer with `*Completions*'.
;;
;;  Regexp matching is perhaps the most powerful feature of Icicles.
;;  Enjoy!  Explore!  You can at any time switch back and forth
;;  between prefix completion (`down', `up'), apropos completion
;;  (`next', `prior'), and history traversal (`M-n', `M-p').

;;
;;
;;  Longest-Common-Match Completion
;;  -------------------------------
;;
;;  Apropos (regexp) matching and prefix completion each match a
;;  pattern against a completion candidate.  This operation concerns
;;  only a single candidate; it does not take into account the fact
;;  that there are others.  Since the matching operation is repeated
;;  anyway for each candidate, however, we can also find the longest
;;  string that includes the same match (apropos or prefix) for all
;;  candidates.
;;
;;  For prefix completion, Emacs already completes your input to the
;;  longest common prefix match.  Icicles extends this notion to
;;  apropos completion.
;;
;;  For example, if you enter `M-x minib' and hit `TAB', Emacs
;;  completes your input to `minibuffer', which is the longest prefix
;;  match for `minib' among all command names.  The actual string that
;;  matches prefix `minib' among all candidates is, itself, `minib'.
;;
;;  If you hit `S-TAB', then each matching candidate contains a
;;  substring that matches your regexp input `minib'.  In this case,
;;  that substring is `minib', just as in the prefix-matching case.
;;  And, as in the prefix case, each matching candidate also includes
;;  a longer substring, `minibuffer', which includes what your input
;;  matches for each candidate.
;;
;;  Icicles replaces your regexp input in the minibuffer by the
;;  longest such string.  Icicles highlights this longest common match
;;  in buffer *Completions* using face
;;  `icicle-common-match-highlight-Completions' (magenta, by default).
;;  What your input matches directly is highlighted in *Completions*
;;  using face `icicle-match-highlight-Completions' (red, by default).
;;
;;  It is of course possible that a given regexp match different
;;  candidates differently, so that there is no common match.  In that
;;  case, only the individual matches are highlighted in *Completions*
;;  - you will see only red, no magenta, highlighting.  For example,
;;  if your regexp input is `min.*buf' then various different
;;  substrings (such as `minibuf' from `minibuffer-complete' and
;;  `mint-truncate-buf' from `comint-truncate-buffer') are highlighted
;;  in red, but these share no common substring.
;;
;;  You will also see only red highlighting if what your input matches
;;  directly is the same as the longest common match.  For example, if
;;  a function `moccur-regexp-read-from-minibuf' is defined (it is in
;;  library `color-moccur.el'), and your input to `C-h f' is
;;  `min[^-]*buf', then only `minibuf' is highlighted in red.
;;
;;  Longest-common-match completion is convenient, but when
;;  apropos-completing you often need to try variants of a regexp,
;;  editing it and observing which candidates match in *Completions*,
;;  until you get the regexp right.  Longest-common-match completion
;;  has the disadvantage that you lose your regexp as input, which
;;  makes it hard to edit it!  To retrieve it, simply use `C-l'
;;  (`icicle-retrieve-last-input') in the minibuffer.  Repeat `C-l' as
;;  needed.  Set option `icicle-expand-input-to-common-match-flag' to
;;  nil to turn off longest-common-match completion altogether, if you
;;  prefer.

;;
;;
;;  Progressive Completion
;;  ----------------------
;;
;;  The best way to explain this feature is to use a familiar analogy.
;;  Unix or GNU/Linux command `grep' takes a regular-expression
;;  argument, and matches it against lines in files.  A common idiom
;;  that people use is to chain, or cascade, multiple calls to `grep',
;;  using the output of one as the input to the next.  For example:
;;
;;    grep plant *.txt | grep food | grep mineral
;;
;;  The output of the search for "plant" is used as the input for the
;;  search for "food", and the output of that search serves as the
;;  input for the search for "mineral".  The order of the three
;;  component searches can make a difference in terms of performance,
;;  but not in terms of the result, which is always the set of lines
;;  in files *.txt that match "plant" AND "food" AND "mineral", in any
;;  order.  Each of the `grep' operations defines a set of matches,
;;  and the chain of `grep' operations effects the intersection of
;;  those sets.
;;
;;  Of course, you could try to accomplish the same thing with a
;;  single call to `grep' using a complex regexp.  But why would you?
;;
;;  The same idea is behind the Icicles feature of progressive
;;  completion: instead of trying to come up with a complex regexp
;;  that does what you want, try getting there a step at a time:
;;
;;   1. Match an input regexp against the set of all possible
;;      completions.
;;
;;   2. Narrow the set of matched candidates by matching them against
;;      another input regexp.
;;
;;   3. Narrow those results down by matching them against a third
;;      input regexp.
;;
;;   4... And so on.
;;
;;  During completion, `M-*' is bound in the minibuffer to command
;;  `icicle-narrow-candidates', which prompts for a new regexp and
;;  matches it against the current set of completion candidates.
;;
;;  For example, suppose that you want to know about a function that
;;  Emacs has that deletes the character to the left of point (that
;;  is, backward).  You don't recall if it's `delete-character-back',
;;  `delete-backward-char', `character-delete-backward', or whatever.
;;  You take a guess that the name contains `delete', `char', and
;;  `back'.
;;
;;   1. `C-h f char S-TAB' displays function names that contain
;;      `char'.
;;
;;   2. `M-* delete' narrows that set of function names to those that
;;      also contain `delete'.
;;
;;   3. `M-* back' narrows the set of matching names further, to those
;;      that also contain `back'.
;;
;;  This displays a list of functions like this in *Completions* (your
;;  list might be somewhat different):
;;
;;    backward-delete-char        backward-delete-char-untabify
;;    delete-backward-char        icicle-backward-delete-char-untabify
;;    icicle-delete-backward-char
;;    quail-conversion-backward-delete-char
;;
;;  Then, of course, you can pick one (or you can use `C-next'
;;  repeatedly to view the doc of each of these functions in turn -
;;  see "Help on Candidates").
;;
;;  You get the idea.  This feature is both very simple to use and
;;  very useful.  It's easy to appreciate using multiple simple
;;  matching steps (regexp or not) instead of a single regexp.  Try it
;;  once, and you'll be hooked.
;;
;;  `M-*' works with both prefix completion and apropos completion.
;;  You can first use `TAB' to require the target to start with some
;;  string, and then use `M-*' to specify other patterns that parts of
;;  it must also match.  However, it of course makes no sense to use
;;  `TAB' instead of `S-TAB' after you use `M-*': once you've said
;;  that the target must start with "fo" there is no sense saying that
;;  it also starts with "ti"!
;;
;;  For lack of a better name, I'm calling this feature "progressive
;;  completion".  If the name "incremental completion" (= icompletion)
;;  were not already taken to mean incremental completion *help*
;;  (which performs no completion), then that might be a good name for
;;  this.  This might also be called "stepped", "cascaded", or
;;  "piecewise" completion.
;;
;;  Another possible name for it would be "multiple completion", but I
;;  use that to stand for simultaneous (parallel) completion of
;;  multiple parts of a compound target, which is something different
;;  (see "Multi-Completions", below).  Progressive completion is a set
;;  of mini-completions that are wired in series, not in parallel.
;;
;;  Note that when you use `M-*' in the minibuffer, it calls
;;  `completing-read', which creates a recursive minibuffer.  That is,
;;  the minibuffer depth is increased.  In Emacs there is no indicator
;;  of the current minibuffer depth, and this can sometimes be
;;  disorienting.  Each time you use `M-*' you push down one level of
;;  minibuffer recursion (that is, minibuffer depth is incremented).
;;  Each time you use, say, `C-g', you pop up one level of minibuffer
;;  recursion (that is, minibuffer depth is decremented).
;;
;;  If you use my library `oneonone.el', then you get visual feedback
;;  on the current minibuffer depth.  One-On-One Emacs gives you a
;;  standalone minibuffer frame, and it changes the background hue
;;  (color) of that frame slightly with each change in minibuffer
;;  depth.  This is especially helpful with Icicles, where use of
;;  `M-*' is common.
;;
;;  Note too that there is a slight difference in behavior between
;;  Icicles commands and some other Emacs commands when you accept
;;  input after `M-*'.  When possible, Icicles accepts your input and
;;  passes it immediately to the top level, bypassing any intermediate
;;  recursive minibuffer levels that are waiting for inupt.  However,
;;  Emacs commands that are defined with literal-string `interactive'
;;  specs, such as (interactive "fFile: "), do not use
;;  `completing-read' or `read-file-name', so there is no way for
;;  Icicles to take this shortcut with them.  In that case, you will
;;  simply need to hit `RET' again to accept your input at each
;;  recursive minibuffer level, until you get back to the top level.
;;  Sorry for this inconvenience!  If you are an Emacs-Lisp
;;  programmer, note that this is one reason to use `completing-read'
;;  and `read-file-name' when you write commands that use completion.
;;
;;  See Also:
;;
;;  * "Sets of Completion Candidates", below, for another way to
;;    perform a set intersection on sets of candidate completions
;;
;;  * "Search Enhancements", below, for a way to search using two
;;    regexps - command `icicle-search' uses the same idea as that
;;    behind progressive completion
;;
;;  * "Compile/Grep Search", below, for a way to search files using
;;    three levels of regexps

;;
;;
;;  Inserting a Regexp from a Variable
;;  ----------------------------------
;;
;;  Regexps are powerful, but they can sometimes be complex to compose
;;  and hard to remember once composed.  A shortcut is to compose a
;;  regexp that you want to use and assign it to an Emacs variable.
;;  Then, whenever you are typing input in the minibuffer, you can use
;;  `C-=' (bound to command `icicle-insert-string-from-variable') to
;;  insert the regexp.
;;
;;  If you use `C-u C-=' (provide a prefix argument) then you are
;;  prompted for the variable to use.  You can use any variable.
;;  Without `C-u', the default variable is used (no prompting),
;;  `icicle-input-string'.  So, for example, if `icicle-input-string'
;;  had value "[a-zA-Z]+" then it would match any completion candidate
;;  composed only of letters.  You can customize
;;  `icicle-input-string'.
;;
;;  For convenience, instead of using Lisp evaluation of a sexp such
;;  as (setq icicle-input-string "[a-zA-Z]+") or (setq my-var ".*"),
;;  you can use Icicles command `icicle-save-string-to-variable' to
;;  save a regexp to a variable.  You are prompted for the regexp to
;;  save.  Just as for `icicle-insert-string-from-variable', with a
;;  prefix argument you are prompted for the variable to use; with no
;;  prefix argument the regexp is saved to variable
;;  `icicle-input-string'.
;;
;;  This shortcut feature is especially convenient for use with
;;  command `icicle-search' - you can use it to search text for
;;  sentences, paragraphs, file names, URLs, dates, times, function
;;  definitions, and any other text entities that you can specify by
;;  regexp.  Create a library of regexp-valued variables that are
;;  useful to you, and use `C-=' to quickly access them in
;;  `icicle-search'.  See "Search Enhancements", below, for more
;;  information.

;;
;;
;;  What About Special-Character Conflicts?
;;  ---------------------------------------
;;
;;  Regular-expression syntax treats some characters specially, but
;;  some of these special characters have another special meaning in
;;  Emacs when used with file-name inputs.  What about the conflict
;;  between interpreting characters such as `$', `\', `.', `?', and
;;  `*' as 1) regexp special characters and 2) special characters for
;;  file-name input?  For example, when inputting a file name, should
;;  `*' be treated as a regexp multiple-occurrences operator or as a
;;  file-name wildcard?
;;
;;  In Emacs file-name input:
;;
;;  - `$' can be used to prefix environment variables.
;;
;;  - `*' and `?' can be used as wildcards, effectively inputting
;;    multiple file names at once.
;;
;;  - `.' and `..' can be used to navigate a directory hierarchy.
;;
;;  - `\' is a directory separator, like `/', on MS Windows, at least.
;;
;;  Icicles handles the conflict by interpreting such characters as
;;  regexp special characters only during input completion and cycling
;;  - and then only if you do not escape them (with `\').  If present
;;  in the input when you finally accept it (using `RET'), they take
;;  on their normal Emacs meanings for file-name input:
;;  environment-variable prefix, wildcard, directory abbreviation, or
;;  directory separator.
;;
;;  That is, whenever there is a potential conflict of interpretation,
;;  the regexp meaning is used for completion and cycling, and the
;;  standard interpretation for file-name input is used for accepting
;;  the input.  So, for example, to get the wildcard interpretation of
;;  `*', just forego regexp completion and cycling.  And vice versa:
;;  forego the wildcard interpretation to use regexp completion and
;;  cycling.
;;
;;  This is in any case the behavior of vanilla Emacs as well.  If, in
;;  vanilla Emacs, you use `ici*' or `ici*.el' as input to `find-file'
;;  and hit `TAB', there is no completion available.  File-name
;;  globbing and completion are independent.
;;
;;  Note: Because `?' is useful in regexp syntax, the standard Emacs
;;        minibuffer binding of `?', which just displays the
;;        completion-candidates list, is not used in Icicles.  In
;;        Icicles, `?' self-inserts in the minibuffer, like any other
;;        printable character.  (Use `TAB' or `S-TAB' to display the
;;        list.)  In standard Emacs, you must quote `?' or
;;        copy-and-paste it, to insert it in the minibuffer for use as
;;        a file-name wildcard.
;;
;;  The interpretation conflict for `\' (on MS Windows) is not really
;;  a problem, anyway.  Although you cannot use a backslash (`\') as a
;;  directory separator during completion and cycling, you can always
;;  use a slash (`/') instead - even on MS Windows.  Just break with
;;  MS-Windows syntax, and get in the habit of using `/' as the
;;  directory-separator character.
;;
;;  Even if you use only slash, not backslash, as a directory
;;  separator when inputting, however, it's possible that you could
;;  run into some trouble (on MS Windows) - you might (knowingly or
;;  not) use `\' as a directory separator in the values of environment
;;  variables that you use as part of file-name input.  Because the
;;  expanded input is treated as a regexp by apropos completion, you
;;  should use only prefix completion with input that includes
;;  environment variables, if their expansions include backslashes.
;;
;;  The interpretation conflict for `$' is also not a real problem.
;;  You can get the effect of both interpretations of `$' at the same
;;  time, because Icicles recognizes that `$' at the end of input
;;  cannot be an environment-variable prefix.  This means, for
;;  example, that you can use a pattern such as `$HOME.*t$' to match
;;  the files in your home directory whose names end in `t'.
;;
;;  The first `$' here is not treated specially during regexp matching
;;  and cycling; the environment variable `$HOME' is expanded by the
;;  shell to a directory name. The second `$' is treated as the regexp
;;  special character that matches at the end of a line.
;;
;;  Tip: Because slash (`/') is about the only non-word syntax
;;       character that is likely to appear in file-name completions,
;;       you can usually use `\W$' to match only directories (by
;;       matching the `/' at the end of their names).  `\W' is the
;;       regexp pattern that matches any character that does not
;;       appear in words.
;;
;;  Finally, you can toggle interpretation vs escaping of regexp
;;  special characters at any time using `C-`' in the minibuffer
;;  (command `icicle-toggle-regexp-quote').  Escaping special
;;  characters this way means they are no longer special; they simply
;;  match themselves.  This has the effect of reducing apropos
;;  completion to simple substring completion.  If you never want to
;;  use regexp matching (*not* recommended!), you can customize user
;;  option `icicle-regexp-quote-flag', setting it to non-nil.
;;
;;  See Also:
;;
;;  * "Sets of Completion Candidates", below, for a way to use Icicles
;;    regexp-matching to open Dired on sets of files that you cannot
;;    specify using file-name wildcards.
;;
;;  * "Multi-Commands", below, for a way to open multiple files whose
;;    names match a regular expression.
;;
;;  * "File-Name and Directory-Name Completion Tips", below, for a way
;;    to locate and open files by regexp anywhere in your file system
;;    - that is, match against directory-name as well as file-name
;;    components.

;;
;;
;;  Alternative Libraries: Other Methods of Choosing Default Values
;;  ---------------------------------------------------------------
;;
;;  There are other libraries that give you alternative ways to pick a
;;  candidate default value.  There are, for instance, many libraries
;;  that provide ways to switch buffers.  Some of these present
;;  candidates in the minibuffer and choose one as soon as you type
;;  enough of its name to select it unambiguously - without your
;;  needing to confirm your choice (with `RET', for example).  Library
;;  `ido.el' is an example of such a library.  Library `iswitchb.el'
;;  is another such library; it is specialized for choosing a buffer.
;;
;;  Choosing without confirming can be very quick, but I prefer to
;;  confirm a choice.  In any case, you can also use Icicles to choose
;;  without confirming, if you wish - see "Multi-Commands", below.
;;  See also "Exiting the Minibuffer Without Confirmation" for how to
;;  obtain the complete-and-exit behavior of library `iswitchb.el'.
;;
;;  The main reason I prefer Icicles is because of its generality.
;;  You use the same input, cycling, and completion method for
;;  everything.  There is no need to be familiar with one method for
;;  switching buffers, another method for choosing a command, another
;;  for choosing a variable, and so on.  Library `ido.el' is quite
;;  general too, but perhaps a little less so.
;;
;;  Also, I like to be able to edit the value in the minibuffer.  For
;;  instance, in a situation where you are not required to enter one
;;  of the default values (e.g. no REQUIRE-MATCH argument to
;;  `completing-read'), you can use completion to retrieve a default
;;  value that is similar to what you want to enter, then edit it and
;;  hit `RET' to submit the actual value you want.  Library `ido.el'
;;  does have an "edit" command or mode, but I find Icicles better for
;;  letting me edit input.
;;
;;  Icicles has many additional features that are not available in
;;  other libraries (see below), but its main advantage is its
;;  generality: you use the same user interface for input of any kind.

;;
;;
;;  Exiting the Minibuffer Without Confirmation: `S-RET'
;;  ----------------------------------------------------
;;
;;  Normally, if you exit the minibuffer with input that only
;;  partially matches a completion candidate, the value you input is
;;  exactly what you typed.  That is, exiting does not automatically
;;  complete your input - what you type is what you get.  This is
;;  desirable most of the time, because it lets you input a value that
;;  does not correspond to any of the completion candidates.  This is
;;  how, for instance, you can use `C-x C-f' to open a new file or
;;  `C-x b' to create a new buffer.
;;
;;  However, some people prefer to limit input to the available
;;  completion candidates.  This can be handy in the case of switching
;;  to a buffer, for instance.  If you have a buffer named
;;  `new-ideas.txt', you might like to be able to type only `new'
;;  followed by `RET', and not have to first complete the input text.
;;  This is the behavior of libraries `ido.el' and `iswitchb.el'.
;;
;;  It is the command you use that decides whether `RET' first
;;  completes your input before exiting the minibuffer.  This is done
;;  in the command definition by providing a non-nil REQUIRE-MATCH
;;  argument to function `completing-read', which prompts you and
;;  reads your input, possibly completing it.
;;
;;  If you use standard Emacs command `switch-to-buffer' then `RET'
;;  does not behave this way; it simply accepts your input, `new', and
;;  creates a new buffer with that name.  If you set user option
;;  `icicle-bind-top-level-commands-flag' to nil, then Icicles will
;;  not change any of your global bindings, and, in that case, when
;;  you use `C-x b' you get the `switch-to-buffer' behavior in the
;;  context of Icicles.
;;
;;  By default, Icicles command `icicle-buffer' behaves the same way
;;  as `switch-to-buffer' in this regard.  However, you can obtain the
;;  complete-and-exit behavior with `icicle-buffer' by setting option
;;  `icicle-buffer-require-match-flag' to `partial-match-ok'.  This
;;  value overrides the REQUIRE-MATCH argument to `completing-read',
;;  in effect forcing it to `t'.
;;
;;  Whenever completion *requires* a match against one of the
;;  completion candidates (typically, an existing file or buffer
;;  name), you can complete and exit the minibuffer all at once, with
;;  only partial input in the minibuffer, by using `RET'.  But what
;;  about apropos completion?  Simply use `S-RET' instead of `RET':
;;  `RET' is standard in Emacs and uses prefix completion; `S-RET' is
;;  specific to Icicles and uses apropos completion.  For example, you
;;  can type `idea' followed by `S-RET' to switch to buffer
;;  `new-ideas.txt'.

;;
;;
;;  *Completions* Display
;;  ---------------------
;;
;;  Icicles adds a few enhancements to the *Completions* display, for
;;  convenience.  The following apply whenever buffer *Completions* is
;;  displayed:
;;
;;  1. When you cycle completions in the minibuffer, the current
;;     candidate is highlighted in *Completions*.
;;
;;  2. You can use the `insert' key to move back and forth between the
;;     minibuffer and *Completions*.  In each direction, the current
;;     candidate is tracked in the destination buffer.  For example,
;;     if the candidate in the minibuffer is `foobar', after you hit
;;     `insert' the cursor is on `foobar' in *Completions*.  In the
;;     other direction, if the cursor is on `foobar' in *Completions*,
;;     after you hit `insert' the current input in the minibuffer is
;;     `foobar'.
;;
;;  3. In buffer *Completions*, you can use the arrow keys to navigate
;;     among the candidate completions.  The current candidate (under
;;     the cursor) is highlighted.
;;
;;  4. *Completions* can also serve as a new kind of icompletion help
;;     - see "Icompletion", below.
;;
;;  5. You can choose multiple candidates during completion, by
;;     clicking them with `mouse-2' while holding the Control key
;;     pressed.  See "Multi-Commands", below.

;;
;;
;;  Icompletion
;;  -----------
;;
;;  Emacs incremental completion, or icompletion, provided by standard
;;  library `icomplete.el', displays matching prefix completions in
;;  the minibuffer.  This display is updated incrementally as you type
;;  characters.  In spite of the name, icompletion does not, in fact,
;;  provide any completion; it provides completion help, letting you
;;  know which (prefix) completions are available.
;;
;;  Icicles enhances Emacs icompletion in two ways:
;;
;;  1. It works with library `icomplete+.el' to provide minibuffer
;;     feedback on the number of completion candidates.
;;
;;  2. It provides a new kind of icompletion using buffer
;;     *Completions*.
;;
;;  ** icomplete+.el Displays the Number of Other Prefix Candidates **
;;
;;  Library `icomplete+.el' enhances `icomplete.el' in various ways.
;;  One of these ways is to complement Icicles by displaying the
;;  number of other prefix-completion candidates when cycling.  This
;;  number is displayed whenever you change direction when cycling.
;;  For example:
;;
;;      M-x forward-line   [Matched]  (13 more)
;;
;;  Like `icomplete.el', `icomplete+.el' provides help for only prefix
;;  completions, not apropos completions.  (Reminder: There is no
;;  icompletion for file-name completion - see standard library
;;  `icomplete.el'.)
;;
;;  ** Icompletion in *Completions*: Apropos and Prefix Completion **
;;
;;  Buffer *Completions* shows you the current set of candidates for
;;  either prefix or apropos completion.  Together, user options
;;  `icicle-incremental-completion-flag',
;;  `icicle-incremental-completion-delay', and
;;  `icicle-incremental-completion-threshold' control incremental
;;  updating of *Completions*.
;;
;;  If `icicle-incremental-completion-flag' is non-nil, then
;;  *Completions* is automatically updated when you change your input
;;  in the minibuffer - that is, with each character that you type or
;;  delete.  This is another form of icompletion, unique to Icicles.
;;  It uses buffer *Completions*, instead of the minibuffer, to show
;;  the completion help.
;;
;;  The particular non-nil value of
;;  `icicle-incremental-completion-flag' determines when *Completions*
;;  is displayed and updated.  The default value, t, means that
;;  *Completions* is updated only if it is already displayed.  Use t
;;  if you don't want *Completions* to be too intrusive but you want
;;  it to provide the most help when you ask for help (via `TAB' or
;;  `S-TAB').
;;
;;  Any other non-nil value displays and updates *Completions*
;;  whenever there is more than one completion candidate.  That can be
;;  more helpful, but it can also be more distracting.  A value of nil
;;  turns off automatic updating altogether - *Completions* is then
;;  displayed only upon demand.  I find that t represents a good
;;  compromise, providing help when I ask for it, and then continuing
;;  to help until I've finished choosing a candidate.
;;
;;  Option `icicle-incremental-completion-delay' is the number of
;;  seconds to wait before updating *Completions* incrementally.  It
;;  has an effect only when the number of completion candidates is
;;  greater than `icicle-incremental-completion-threshold'.  This
;;  delay can improve performance when there are many candidates.  It
;;  lets you type ahead before any redisplay occurs; otherwise,
;;  redisplay occurs for each character you type or delete.
;;
;;  You can toggle incremental completion at any time (changing
;;  `icicle-incremental-completion-flag' between nil and t) using
;;  command `icicle-toggle-incremental-completion', which is bound to
;;  `C-#' in the minibuffer.  If the number of completion candidates
;;  is very large, then use `C-#' to toggle incremental completion off
;;  - that will save time by not updating *Completions*.  See also
;;  "Dealing With Large Candidate Sets", below, for other ways to deal
;;  with a large number of candidates.
;;
;;  There are several advantages of using *Completions* for
;;  icompletion, as opposed to the minibuffer:
;;
;;  1. Many more candidates can be displayed in *Completions* than can
;;     be displayed by standard icompletion, which uses the minibuffer
;;     for feedback.
;;
;;  2. Standard (minibuffer) icompletion provides feedback only on
;;     matches for prefix completion.  If you use both standard
;;     icompletion and Icicles icompletion, you can have incremental
;;     help for both prefix completion and apropos completion at the
;;     same time, one in the minibuffer and the other in
;;     *Completions*.
;;
;;  3. The other Icicles *Completions* features are available for the
;;     current set of matching candidates: cycling, highlighting of
;;     match root, highlighting of previously used candidates, and so
;;     on.  See "*Completions* Display", above.

;;
;;
;;  Help on Candidates
;;  ------------------
;;
;;  While you are cycling among completion candidates for input to a
;;  command, you can simultaneously display help on each candidate or
;;  any given candidate.  To show help on each candidate as you cycle,
;;  press and hold the Control key while using the vertical arrow keys
;;  (for prefix completion) or the Page Up/Down keys (for apropos
;;  completion).  To show help on any individual candidate, just
;;  navigate to it (cycling or using completion), and hit `C-RET' - or
;;  press Control and click it with `mouse-2' (`C-mouse-2') in buffer
;;  *Completions*.
;;
;;  For example, if you use `C-next' (apropos completion) to cycle
;;  among commands to execute with `M-x', the documentation for each
;;  command is displayed in the *Help* buffer as the candidate appears
;;  in the minibuffer.  As another example, if you cycle among buffers
;;  with `C-down' (prefix completion) for `C-x b', the major and minor
;;  modes of each candidate buffer are described in buffer *Help* as
;;  the buffer name appears in the minibuffer.
;;
;;  ** Use Candidate Help Like You Use Command `apropos' **
;;
;;  You can use this functionality as a kind of expanded `apropos'
;;  functionality.  As an example, type `C-h v out', then type `S-TAB'
;;  to display all variables that match "out" (in buffer
;;  *Completions*).  Then use `C-next' to cycle among those variables,
;;  displaying their documentation in the *Help* buffer as they appear
;;  one by one in the minibuffer.  Or click individual variable names
;;  with `C-mouse-2', to display their documentation.  The standard
;;  `apropos' commands show only the first doc-string line; Icicles
;;  shows the complete doc string.
;;
;;  This can be handy, for instance, when you are unsure which of
;;  several similarly named candidates to choose.  Seeing a
;;  candidate's documentation along with its name can help you decide.
;;
;;  This also works with menu items, if you load library
;;  `icicles-menu.el' as well as `icicles.el'.  As you cycle among
;;  matching menu items, the corresponding command documentation is
;;  displayed in *Help*.
;;
;;  For more information about the types of candidates and their
;;  associated documentation, see the documentation for command
;;  `icicle-help-on-candidate'.  This command is bound to `C-help',
;;  `C-f1', and, by default, `C-RET'.
;;
;;  If you use one-buffer-per-frame (`pop-up-frames' non-nil), then
;;  displaying *Help* in one frame can interfere with viewing
;;  *Completions* in another.  For that reason, the *Completions*
;;  frame is raised to the front.  Also, if user option
;;  `icicle-Completions-frame-at-right-flag' is non-nil (default
;;  value: `t'), then the *Completions* frame is moved to the right,
;;  out of the way, when you access *Help*.
;;
;;  ** Other Icicles Apropos Commands **
;;
;;  There are also Icicles replacements for the standard Emacs
;;  `apropos' commands.  They act the same, but they also let you see
;;  the list of regexp matches incrementally (as with all Icicles
;;  commands), using `S-TAB'.  If you also use my library
;;  `apropos-fn+var.el', then these Icicles commands take advantage of
;;  the apropos enhancements in that library.
;;
;;  The Icicles apropos commands are: `icicle-apropos',
;;  `icicle-apropos-command', `icicle-apropos-function',
;;  `icicle-apropos-option', `icicle-apropos-variable', and
;;  `icicle-apropos-zippy'.
;;
;;  In addition, Icicles commands `icicle-doc', `icicle-fundoc', and
;;  `icicle-vardoc' provide the functionality of standard Emacs
;;  command `apropos-documentation', but with additional features.
;;  See "Multi-Completions", below.
;;
;;  One difference between Icicles apropos commands and the standard
;;  commands, besides the Icicles enhancements already described, is
;;  that (starting with Emacs 22) the standard commands let you input
;;  a set of keywords, as an alternative to inputting a regexp.
;;  Icicles apropos commands do not allow for keyword input, as such.
;;  However, Icicles progressive completion (see "Progressive
;;  Completion", above) provides a more powerful way to search with
;;  multiple keywords (in fact, multiple regexps) - you can of course
;;  use it with the Icicles apropos commands.  Also, there are several
;;  problems with the standard Emacs apropos commands, with respect to
;;  interpreting your input as either a set of keywords or a regexp.
;;  Because they allow two very different syntaxes as input, the
;;  standard apropos commands are forced to make some limiting
;;  compromises for keyword searching.

;;
;;
;;  Multi-Commands
;;  --------------
;;
;;  Certain Icicles commands let you make multiple input choices in a
;;  single command execution.  In effect, you can choose multiple
;;  items from a set of choices, using buffer *Completions* as a
;;  multiple-choice "menu".  (It's not necessary to display
;;  *Completions*, however.)
;;
;;  I call such multiple-choice commands "multi-commands".  When a
;;  multi-command prompts you for input, you can make a single choice
;;  and press `RET' to confirm it, as usual, or you can choose any
;;  number of completion candidates, using `C-RET' (or `C-mouse-2')
;;  for each.  You can thus act on multiple candidates, or even
;;  multiple times on the same candidate, during the same execution of
;;  the command.
;;
;;  For example, command `icicle-delete-file' lets you delete a single
;;  file or a set of files that match your minibuffer input - all in
;;  the same command execution.  If you type no input, then all files
;;  in the current directory match, and you can delete any number of
;;  them individually.  If you type `~$' and hit `S-TAB'
;;  (`apropos-complete'), then all files that end in `~' match, and
;;  you can delete any number of them.  Similarly, command
;;  `icicle-buffer-other-window' lets you display any number of
;;  buffers, and so on.
;;
;;  You make multiple choices this way by cycling through the
;;  candidate completions, as usual, and hitting `C-RET' whenever you
;;  want to choose (act on) the current cycle candidate.  Or, just
;;  press and hold Control while clicking each candidate with
;;  `mouse-2'.
;;
;;  Sound familiar?  This is the same thing that you do to access help
;;  on a candidate (see previous section).  A multi-command is just
;;  any command that has a special action defined for use with `C-RET'
;;  (command `icicle-candidate-action') on the current cycle
;;  candidate.  If no such special action is defined, then help on the
;;  candidate is displayed - displaying help is just the default
;;  action, used when no other action is defined.
;;
;;  Similarly, you can use `C-next', `C-prior', `C-down', and `C-up'
;;  to both choose (that is, act on) the current candidate and move
;;  forward or backward to the next successive candidate.  You can
;;  thus just hold down the Control key while cycling, to act on each
;;  candidate in turn, if you want.
;;
;;  Instead of, or in addition to, cycling, you can use completion to
;;  get to a particular candidate you want.  No matter how a candidate
;;  is made current, you can choose the current candidate (perform the
;;  action on it) using `C-RET', `C-mouse-2', `C-next', and so on.
;;
;;  As always, hitting `RET' (or `S-RET') ends the command.  For most
;;  multi-commands, hitting `RET' performs the same action as `C-RET',
;;  but it is possible to have a command that acts differently for
;;  `RET' and `C-RET'.  That is the case, for instance, when help is
;;  displayed via `C-RET'.
;;
;;  You can use `C-g' to exit a multi-command at any time, without
;;  making a final choice using `RET'.  If the actions performed by a
;;  multi-command are easily reversible, `C-g' will often restore
;;  things to the way they were before performing the actions.
;;
;;  You can also cycle among elements of a set, performing actions, if
;;  you use my libraries `doremi.el', `doremi-cmd.el', and
;;  `doremi-frm.el'.  Like Icicles, DoReMi lets you see the effect of
;;  a choice immediately, whenever you make changes.  Each library has
;;  its own advantages and special uses. Advantages of Icicles
;;  include:
;;
;;    - completion to candidate values
;;    - restoration after making changes, letting you preview changes
;;      without actually applying them
;;
;;  See also: "Defining Multi-Commands" for how to define your own
;;  multi-commands.

;;
;;
;;  Choose All Completion Candidates
;;  --------------------------------
;;
;;  The previous section describes how you can use `C-RET'
;;  (`icicle-candidate-action') to choose (act on) multiple completion
;;  candidates, individually.  If you hold down the Control key while
;;  you cycle through the candidates, you can run through each of
;;  them, one by one.
;;
;;  Command `icicle-all-candidates-action', which is bound to `C-!' in
;;  the minibuffer, is just a shorthand way of doing that: act on all
;;  candidates that match the current input.  `C-!' reports on the
;;  objects that were not acted upon successfully (in buffer *Help*).
;;
;;  All multi-commands let you use `C-!' in this way.  Whenever a
;;  command defines a special action for `C-RET' to perform on the
;;  current completion candidate, you can use `C-!' to perform it on
;;  all candidates at once.
;;
;;  Perhaps you already use `% m' (command `dired-mark-files-regexp')
;;  in Dired to mark all files that match a given regular expression,
;;  and then operate on all of the marked files in some way (search
;;  with `A', query-replace with `Q', open with `F', delete with `D',
;;  and so on).  When you execute a multi-command, `C-!' lets you do
;;  something similar.  It applies `icicle-candidate-action-fn' to
;;  each completion that matches (apropos or prefix) the current input
;;  in the minibuffer.
;;
;;  Most top-level Icicles commands are multi-commands.  Command
;;  `icicle-delete-file' is an example.  Instead of entering a file
;;  name at the prompt (e.g. using completion or cycling), you can
;;  type a regular expression, use `S-TAB' to see all matching files,
;;  and then use `C-!' to delete all of them at once.
;;
;;  You get the idea: Use the minibuffer to determine a set of objects
;;  by pattern matching, and then act on all elements of the set.

;;
;;
;;  Sets of Completion Candidates
;;  -----------------------------
;;
;;  Whereas `C-RET' (see "Multi-Commands, above) acts on individual
;;  objects, `C-!' (see "Choose All Completion Candidates", above)
;;  acts on an entire set of objects at once, via their names: the set
;;  of all current completion candidates.  There are additional
;;  Icicles commands that act, not on individual completion
;;  candidates, but on one or more sets of completion candidates.
;;
;;  One of these is `M-*', which effectively narrows the set of
;;  completion candidates by taking the intersection of the candidate
;;  sets defined by various input regexps.  See "Progressive
;;  Completion", above.
;;
;;  This section presents some more Icicles commands that act on sets
;;  of completion candidates.  The basic idea is that you can perform
;;  set operations using the current set of completion candidates,
;;  changing it into a different set.  The available set-operation
;;  commands presented here are these:
;;
;;  * `icicle-candidate-set-save', bound to `C->'.  Save the current
;;    set of completion candidates, for use in a subsequent set
;;    operation (see below).  The saved set is not persistent; it is
;;    saved only until the next `C->' in the same Emacs session.
;;
;;  * `icicle-candidate-set-retrieve', bound to `C-<'.  Retrieve the
;;    saved set of completion candidates, making it the current set.
;;
;;  * `icicle-candidate-set-swap', bound to `C-%'.  Swap the saved and
;;    current sets of completion candidates.
;;
;;  * `icicle-candidate-set-define', bound to `C-:'.  Define the
;;    current set of completion candidates by evaluating an input
;;    sexpr.  The sexpr must evaluate to a list of strings, such as is
;;    returned by `all-completions'.  You can use this to substitute
;;    any list of strings, and then operate on them as completions,
;;    using any Icicles functionalities.  Keep in mind, however, that
;;    the completions must be of the proper type for the context in
;;    which they are used.  For example, if you are executing a
;;    command, they must be command names.
;;
;;  * `icicle-candidate-set-complement', bound to `C-~'.  Complement
;;    the current set of candidates: replace the current candidate set
;;    with its set complement.  This means all possible completions of
;;    the appropriate type that do *not* match the current input.
;;
;;  * `icicle-candidate-set-union', bound to `C-+'.  Replace the
;;    current candidate set by its union with the saved set of
;;    candidates.
;;
;;  * `icicle-candidate-set-difference', bound to `C--'.  Replace the
;;    current candidate set by its set difference with the saved set
;;    of candidates.  That is, the saved candidates are subtracted
;;    from the current candidates, and the result becomes the current
;;    candidate set.  To obtain the opposite set difference,
;;    subtracting the current candidates from the saved candidates,
;;    just use `icicle-candidate-set-swap' followed by
;;    `icicle-candidate-set-difference'.
;;
;;  * `icicle-candidate-set-intersection', bound to `C-*'.  Replace
;;    the current candidate set by its intersection with the saved set
;;    of candidates.  Unlike the set intersection provided by `M-*'
;;    (see "Progressive Completion", above), `C-*' is, in itself, a
;;    one-time operation.  `M-*' can be repeated, using the previous
;;    intersection as one of the sets to be intersected in a new
;;    operation.  Both `C-*' and `M-*' use the current set of matching
;;    candidates as one of the sets being intersected.  But `M-*'
;;    reads another input regexp to define the other set to be
;;    intersected, whereas `C-*' uses the saved candidates set as the
;;    other set.  `M-*' is useful for chaining, to achieve progressive
;;    approximation.  `C-*' is useful to perform an intersection on a
;;    set from a previous input reading.
;;
;;  You can operate on or choose from all input values in the set
;;  resulting from any of these set operations.  For example, you can
;;  use `C-~' to see the list of objects that do not match the current
;;  input, to cycle among those objects, or to operate on any or all
;;  of them.  Use `C-~' at any time to switch to the complement of the
;;  current set of candidates.
;;
;;  Example: To cycle through all files whose names do not end in
;;           `el', you can do the following:
;;
;;  1. Use `C-f' to read a file name.
;;  2. Type `el$' to match all file names that end in `el'.
;;  3. Use `S-TAB' to show the matching files.
;;  4. Use `C-~' to flip to the complement: files not ending in `el'.
;;  5. Use `next' or `prior' to cycle among the new set of candidates.
;;
;;  A minibuffer message briefly confirms each of the set operations.
;;
;;  When buffer *Completions* is displayed, the union, difference, and
;;  intersection commands scroll the buffer when repeated, just like
;;  `TAB' and `S-TAB' do.  Repeating `icicle-candidate-set-complement'
;;  complements the complement, of course, giving the original set.
;;
;;  Once you have established a set of completion candidates using any
;;  of the candidate-set commands, you can cycle among the candidates
;;  of that set using either prefix or apropos cycling (that is,
;;  `next'/`prior' or `down'/`up').  However, switching from prefix to
;;  apropos cycling (or completion), or vice versa, establishes a new
;;  completion set of the appropriate type, as usual.  Switching
;;  completion type signifies that you are finished with the specially
;;  defined completion set, and you want to redefine it using apropos
;;  or prefix cycling or completion.
;;
;;  When you save a set of completion candidates, it is saved in
;;  variable `icicle-saved-completion-candidates', by default.  You
;;  can process this list of names in any way you like using Emacs
;;  Lisp.  For example, you can save a list of file names that match a
;;  regexp, then print the list or process the individual files in
;;  some way.  Here, for instance, is how to save the set of file
;;  names that contain either `dir' or `ici':
;;
;;    `C-x C-f \(dir\|ici\) S-TAB C-> C-g'
;;
;;  You can save the current set of completions to a different
;;  variable from `icicle-saved-completion-candidates' by using a
;;  numeric prefix argument to command `icicle-candidate-set-save';
;;  that is, use `C-u N C->'.  Alternatively, use `M-C-}', which is
;;  bound to command `icicle-candidate-set-save-to-variable'.  You are
;;  prompted for the name of the variable, and you can use completion
;;  when inputting it.  Such variables are in a special namespace, so
;;  they are the only candidates for this completion.
;;
;;  To retrieve completion candidates that were previously saved to a
;;  variable other than `icicle-saved-completion-candidates', so that
;;  they become the current set of candidates, use either `C-u N C-<',
;;  where N is an integer, or `M-C-{' (`icicle-candidate-set-retrieve'
;;  or `icicle-candidate-set-retrieve-from-variable').
;;
;;  Note that using a plain prefix argument (`C-u' without a number)
;;  with `C->' and `C-<' saves or retrieves a completion-candidates
;;  set using a cache file, not a variable.  See "Persistent Sets of
;;  Completion Candidates", below.
;;
;;  You can use Icicles commands `icicle-dired-saved-file-candidates'
;;  and `icicle-dired-saved-file-candidates-other-window' to open
;;  Dired on a saved list of file names - only those files are listed
;;  in the Dired buffer.
;;
;;  Note: Prefix icompletion (`icomplete.el' or `icomplete+.el' - see
;;        "Icompletion", above) does not take into account the
;;        candidate set resulting from a set operation: it always
;;        displays the normal set of prefix completions in the
;;        minibuffer.
;;
;;  You might have noticed that, as a mnemonic device, the keys bound
;;  to the various set operations use the corresponding binary
;;  arithmetic or Boolean operators: `~' (unary negation) for
;;  complement (not); `*' (multiplication) for intersection (and); `+'
;;  (addition) for union (or); and `-' (subtraction) for difference.
;;
;;  For other examples of using set operations, see also:
;;
;;   * "Progressive Completion"
;;   * "History Enhancements"
;;   * "Search Enhancements"
;;
;;  For saving completion candidates persistently and retrieving them
;;  later, see "File-Name Input, Locating Files, and Persistent
;;  Candidate Sets", below.

;;
;;
;;  File-Name Input and Locating Files Anywhere
;;  -------------------------------------------
;;
;;  Most Icicles commands that target file or directory names make use
;;  of function `read-file-name' to read the file name with
;;  completion.  This includes all Icicles commands defined with
;;  `icicle-define-file-command'.  An example is multi-command
;;  `icicle-find-file', which visits one or more files.
;;
;;  When `read-file-name' is used to read input, only the file name
;;  itself, not the directory portion, is used for matching.  The
;;  behavior is thus the same whether or not the directory is present
;;  in the minibuffer.  If you prefer, you can delete the directory
;;  first, using `M-k' (the `default-directory' is used, by default).
;;
;;  This means, in particular, that you can use apropos completion to
;;  match a substring, without needing to prefix the substring with
;;  `.*' in the minibuffer.  For example, to match the file named
;;  `favorite-foo-file.bar' in directory `/some/path/to/my/', you need
;;  not use `/some/path/to/my/.*foo'; it is sufficient to use either
;;  `foo' or `/some/path/to/my/foo'.
;;
;;  It is possible, however, for a command to target file names but
;;  make use of `completing-read' instead of `read-file-name'.  In
;;  that case, there is, a priori, no notion of `default-directory';
;;  the completion candidates are treated simply as strings.  Since
;;  `completing-read' has no notion of file names, it is also the case
;;  that file-name wildcards such as `*' are not taken into account by
;;  it.  Unless the command that calls `completing-read' does
;;  something special to interpret such wildcards, you cannot use them
;;  in input strings.  (You can of course use `*' in regexps - see
;;  "What About Special-Character Conflicts?", above, for the
;;  distinction.)
;;
;;  Examples of Icicles multi-commands that read a file name using
;;  `completing-read', not `read-file-name', are `icicle-locate-file',
;;  `icicle-locate-file-other-window', and `icicle-recent-file'.  The
;;  advantage of using `completing-read' is the flip side of the main
;;  disadvantage: There is no notion of `default-directory'.  This
;;  means that these commands let you regexp-match against any part of
;;  the absolute file name, including directory components.  This
;;  makes sense for these commands, because the set of completion
;;  candidates can include absolute file names from many different
;;  directories.
;;
;;  Command `icicle-recent-file' lets you open any file that you have
;;  visited recently, perhaps in a previous Emacs session.  Commands
;;  `icicle-locate-file' and `icicle-locate-file-other-window' can be
;;  used to find a file when you do not know what directory it is in.
;;  They look throughout a given directory, including throughout all
;;  of its subdirectories.
;;
;;  By default, the target directory is the current directory, but if
;;  you supply a prefix argument then you are prompted for the
;;  directory to search.  If you use the root of your file system as
;;  the search directory, then the locate-file commands will match
;;  completion candidates anywhere in your file system.
;;
;;  This can be quite useful.  It gives you much of the power of the
;;  Unix `find' command just for completing input!  And with
;;  incremental completion (see "Icompletion", above), you can see
;;  what matches your input as you type.
;;
;;  Obviously, if you use your entire file system as the set of
;;  completion candidates then gathering and matching such a large set
;;  of file names can take some time.  On my hard drive, for instance,
;;  there are 36 GB full of files, and it takes about 40 seconds to
;;  gather all of the file names.  In spite of this inconvenience,
;;  this functionality can be useful.  And of course searching a
;;  shallower directory tree presents less of a performance penalty -
;;  you pay for what you get.
;;
;;  There is a way, however, of having your cake and eating it too.
;;  You can gather all of the file names in your file system once, and
;;  save that list of completion candidates to a cache file on disk,
;;  as a snapshot.  See "Persistent Sets of Completion Candidates",
;;  below, for more on this.
;;
;;  See also: "Dealing With Large Candidate Sets", below.

;;
;;
;;  Persistent Sets of Completion Candidates
;;  ----------------------------------------
;;
;;  Section "Sets of Completion Candidates", above, describes how you
;;  can save the current set of completion candidates and reuse it
;;  later.  This is not a persistent save, however; the candidates are
;;  simply saved in variable `icicle-saved-completion-candidates' for
;;  the duration of your Emacs session (or until you save candidates
;;  again).
;;
;;  You can save the current set of completions (whatever it is)
;;  persistently by supplying a plain prefix argument (`C-u') when you
;;  use `C->' (`icicle-candidate-set-save').  Alternatively, you can
;;  use `C-}', bound to `icicle-candidate-set-save-to-cache-file',
;;  which does the same thing.  To retrieve completion candidates that
;;  were previously saved to a cache file, so that they become the
;;  current set of candidates, use either `C-u C-<' or `C-{'
;;  (`icicle-candidate-set-retrieve' or
;;  `icicle-candidate-set-retrieve-from-cache-file').
;;
;;  Note that using a numeric prefix argument (`C-u' with a number)
;;  with `C->' and `C-<' saves or retrieves a completion-candidates
;;  set using a variable that you name, not a cache file.  See "Sets
;;  of Completion Candidates", above.
;;
;;  You can have any number of cache files to persistently save
;;  different sets of completion candidates.  Each file saves a set of
;;  candidates that was current at some time (when you saved it).  You
;;  name the sets of saved candidates, and these names are associated
;;  with the cache files in user option
;;  `icicle-saved-completion-sets'.  That is an alist of entries, each
;;  of which is of the form (SET-NAME . CACHE-FILE-NAME).  You can
;;  customize it, as usual, or set it in your init file (~/.emacs).
;;
;;  Alternatively, you can simply try to save a set of completion
;;  candidates persistently, using `C-u C->' or `C-}'.  You are then
;;  prompted for the names of the candidate set and cache file to use,
;;  and the names you enter are automatically entered in option
;;  `icicle-saved-completion-sets'.  That option is automatically
;;  saved to your custom file, so the next time you use Emacs you can
;;  retrieve any saved set of candidates that you like.
;;
;;  When you try to retrieve a persistent set of completion
;;  candidates, you are similarly prompted for the candidate-set name
;;  and the cache-file name.
;;
;;  "File-Name Input and Locating Files Anywhere", above, tells you
;;  how you can locate any file in your file system.  If you save the
;;  set of all file names persistently, you will increase the
;;  performance of using it - it is much faster to retrieve the list
;;  of all file names than it is to generate it.
;;
;;  With 36 GB of files in my file system, my all-file-system cache
;;  file is 20 MB, and retrieving the file-name completions from it
;;  takes only a few seconds.  With this feature, Icicles essentially
;;  gives you the functionality of the Unix `locate' command, but with
;;  the addition of real-time regexp matching.  Here is all you do:
;;
;;    M-x icicle-locate-file RET
;;    C-#        ; Once or twice: turn off incremental completion.
;;    C-{        ; Retrieve all file names from your cache file.
;;               ; You are prompted for the set name and file name.
;;    foo.*bar   ; Regexp to match names with `foo' followed by `bar'.
;;    S-TAB      ; Update *Completions* display (because of `C-#').
;;
;;  Of course, once you have retrieved a set of candidates from your
;;  cache file, you can access them again without re-reading the file.
;;  When they are retrieved from your cache they are saved in variable
;;  `icicle-saved-completion-candidates', so the next time you want to
;;  use them, just retrieve them from this variable with `C-<'.
;;
;;  See also:
;;
;;  * "File-Name Input and Locating Files Anywhere" for information on
;;    finding files located anywhere in your file system
;;
;;  * "Icompletion" for information on `C-#' (toggle incremental
;;    completion)
;;
;;  * "Sets of Completion Candidates" for information on `C->' (save
;;    current candidates)
;;
;;  * "Dealing With Large Candidate Sets"

;;
;;
;;  Dealing With Large Candidate Sets
;;  ---------------------------------
;;
;;  One of the advantages Icicles provides is the ability to deal with
;;  large sets of completion candidates with ease.  There are other
;;  libraries that also let you cycle among various choices of
;;  different kinds (buffers, files, and so on), but cycling quickly
;;  loses its effectiveness as the number of candidates increases.
;;
;;  Icicles apropos matching lets you work with a large initial set of
;;  candidates by filtering them, quickly reducing the number
;;  candidates to cycle through.  Filtering by a prefix only (vanilla
;;  Emacs) is not very potent.  Until you get used to Icicles, you
;;  will be surprised at your ability to manipulate even humongous
;;  sets of choices.
;;
;;  Nevertheless, there can be times when a candidate set is so large
;;  that you need to use a few tricks to deal with it efficiently.
;;  There are two main things that take time when dealing with a large
;;  set: computing the set and displaying it (with highlighting) in
;;  buffer *Completions*.  In particular, incremental completion
;;  display is costly because it does both of these, recompute the set
;;  and redisplay it, each time you type or delete a character in the
;;  minibuffer.
;;
;;  Here are some tips to improve performance with a large set of
;;  candidates:
;;
;;  * Turn off incremental completion display in buffer *Completions*.
;;    You can do this on the fly at any time by using `C-#' in the
;;    minibuffer - use `C-#' again to turn it back on.  See
;;    "Icompletion", above.
;;
;;  * Compute a large candidate set only once, cache the result, and
;;    reuse it later by reading the cache instead of recomputing.
;;    This is useful, for instance, for the candidate set of all files
;;    on your file system.  You can cache a set of candidates in
;;    either a variable (quickest, but not persistent) or a disk file
;;    (slower, persistent).  See "Persistent Sets of Completion
;;    Candidates", above.
;;
;;  * Compute a large candidate set (and perhaps cache it or filter
;;    it) without displaying it in *Completions*, by using `M-C-TAB'
;;    or `S-M-C-TAB' instead of `TAB' or `S-TAB', respectively.  These
;;    are bound to commands `icicle-prefix-complete-no-display' and
;;    `icicle-apropos-complete-no-display'.  For example, when
;;    initially computing the set of all files on your file system for
;;    `M-x C-u icicle-locate-file', use `S-M-C-TAB' to compute the
;;    set, then use `C-}' to save it to a cache file - you need never
;;    display it.

;;
;;
;;  History Enhancements
;;  --------------------
;;
;;  Icicles enhances minibuffer history in three independent ways:
;;
;;  1. Candidates displayed in *Completions* are highlighted (blue, by
;;     default), when they have been used previously, so you can more
;;     easily recognize them.
;;
;;  2. Command `icicle-keep-only-past-inputs' (`M-pause' in the
;;     minibuffer) restricts the current set of completion candidates
;;     to those that you have used previously.  In other words, it
;;     keeps only those candidates that are highlighted in blue.  To
;;     use `M-pause', you must first have used `TAB' or `S-TAB' to
;;     establish an explicit candidate set.
;;
;;  3. Command `icicle-history' (`M-h' in the minibuffer) matches the
;;     current input against the minibuffer history directly.
;;
;;  All three of these enhancements let you see the complete list of
;;  previous inputs that match your current input.  In vanilla Emacs,
;;  the history lists are never shown as such; you can access previous
;;  inputs only one at a time, in order (with `M-p').  In vanilla
;;  Emacs, you can use a regexp to search the history list (via
;;  `M-r'), but the regexp matching is not dynamic, and the first
;;  match found is the (only) one you get.
;;
;;  Displaying previous inputs that match the current input sounds
;;  like a minor advantage, but it is actually quite helpful in
;;  practice.  Among other things, it means that you can work with
;;  long history lists in a practical way.
;;
;;  Both `M-h' and `M-pause' can be used toward the same end.  They
;;  both work for all input types.  They both use the appropriate
;;  history list for the current command.  They both provide apropos
;;  completion and cycling for the minibuffer history (as well as
;;  prefix completion, of course).  Use them as another way to search
;;  through a history list or complete to one of its elements.
;;
;;  For example, If you use `C-x C-f' to find a file, and then use
;;  `M-h' or `M-pause', the completion candidates will be the names of
;;  files that you have previously accessed (file names you have input
;;  in the minibuffer), and which match the current minibuffer input.
;;
;;  `M-h' lets you complete your input against the minibuffer input
;;  history.  `M-pause' lets you restrict the current explicit set of
;;  completion candidates to those that are also in the minibuffer
;;  history.
;;
;;  They provide similar functionality in different ways.  The
;;  difference is that `M-pause' takes the current set of matching
;;  candidates into account.  It is a completion-candidates set
;;  operation, similar to those described in "Sets of Completion
;;  Candidates", above.
;;
;;  This means, in particular, that with `M-pause' you can first
;;  perform set operations on the set of candidates, and then use that
;;  result to restrict the history search.  For example, you can first
;;  complement the candidate set using `C-~', then use `M-pause' to
;;  restrict those candidates to matches in the history list.  In this
;;  way, you avoid including matches from the original match set when
;;  searching the history.
;;
;;  Example: You are in a directory with lots of files that have the
;;  prefix `foo' and lots of C-language source files.  You happen to
;;  be interested in another file, however.  One way to get to that
;;  file is to use Dired's ability to mark files by matching a regexp
;;  and then use Dired's ability to omit the marked files from view.
;;  You can scan through those that remain, and pick the one you want.
;;  However, it turns out that even then there are many files to scan.
;;  You accessed the one you want now just the other day, but the file
;;  date is unfortunately not significant.
;;
;;  In Icicles, you use regexp matching and take the set complement of
;;  the hits, just like in Dired: `C-x C-f foo.*\.c$' defines the
;;  candidate set as all files whose names start with `foo' and have
;;  extension `c'. `C-~' then defines the candidate set as all files
;;  whose names are not like that. Finally, you use `M-pause' to
;;  restrict the file-name candidates to names that you have used
;;  before. You've accessed many, many files recently, so just cycling
;;  through the history with `M-p' would be tedious. You could match a
;;  regexp against the file history, but how can you come up with a
;;  regexp that finds anti-matches?
;;
;;  A consequence of this difference between `M-h' and `M-pause' is
;;  that using `TAB' or `S-TAB' after `M-pause' abandons use of the
;;  minibuffer history and starts a new set of completion candidates.
;;  It simply completes the current input in the context of the
;;  current command; `TAB' and `S-TAB' have nothing to do with the
;;  minibuffer history in this case.  Using `TAB' or `S-TAB' after
;;  `M-h', however, re-completes your input against the current
;;  history list.
;;
;;  Another consequence is that you can use `down' or `C-down' on the
;;  candidates displayed by `M-h', but not on those displayed by
;;  `M-pause'.  For example, to cycle through the doc for each
;;  variable that starts with `icicle-' which you have previously
;;  input, you can use `C-h v icicle- M-h', then repeatedly use
;;  `C-down'.
;;
;;  Also, file-name and directory-name completion works differently in
;;  these two commands.  By default, the current directory is (as
;;  always) inserted into the minibuffer by commands such as
;;  `find-file', so either `M-h' or `M-pause' after `C-x C-f' will
;;  match previously input file names from the current directory.
;;
;;  However, in the case of `M-h', the entire minibuffer input is
;;  matched against the history list, which is a list of absolute file
;;  names.  `M-pause' works only with the current candidate set,
;;  which, if you have already used `TAB' or `S-TAB' in the current
;;  directory, is a set of relative file names in that directory.
;;
;;  This difference has a consequence for apropos (regexp) completion
;;  with `M-h'.  It means that to match a file name using a substring
;;  you must, in the minibuffer, either not specify a directory (erase
;;  it) or explicitly use `.*' before the file-name substring.
;;
;;  For example, with `M-h', `/foo/bar/lph' will not apropos-match the
;;  previously input file name `/foo/bar/alphabet-soup.el'; you should
;;  use either `/foo/bar/.*lph' or `lph' (no directory).
;;
;;  In the case of `M-pause', however, the input is matched against
;;  the history list as restricted by the existing completion list.
;;  And, since apropos file-name completion uses only the relative
;;  file name, without the directory name, as a regexp, the candidate
;;  list that is restricted has already matched the input regexp.  The
;;  action of `M-pause' is simply to filter the list of candidates,
;;  keeping those that are in the history list.  This means that, with
;;  `M-pause', the input `/foo/bar/lph' will match against the
;;  previously input file name `/foo/bar/alphabet-soup.el'.
;;
;;  If this all sounds confusing, just give it a try; it is much
;;  harder to describe than it is to experience.

;;
;;
;;  Search Enhancements
;;  -------------------
;;
;;  There are two, unrelated enhancements that Icicles provides for
;;  searching:
;;
;;  - An extension to standard Emacs incremental search that lets you
;;    use Icicles completion against previous search strings.
;;
;;  - Top-level Icicles commands that provide an entirely new and
;;    different way for you to search.
;;
;;  ** Isearch Completion Against the Search History **
;;
;;  When you search incrementally (`C-s'), Emacs (21 or later) lets
;;  you complete your input to a string that you have looked for
;;  previously.  In Icicle mode, this feature is enhanced so that you
;;  can use all of the completion behavior provided by Icicles.
;;
;;  In vanilla Emacs, you use `M-TAB' to complete against the search
;;  ring (that is, the search history).  In Icicles, you use `S-TAB'
;;  (`icicle-isearch-complete') to do this - that's what Icicles users
;;  are in the habit of using for (apropos) completion.  They are also
;;  in the habit of using `TAB' for prefix completion, but in Isearch
;;  `TAB' inserts a tab, which is a useful character to include in
;;  search strings.
;;
;;  When you use `S-TAB' while searching, Isearch exits momentarily,
;;  giving way to Icicles completion in the minibuffer (Isearch
;;  actually uses the echo area, not the minibuffer).  You can then
;;  use either `S-TAB' or `TAB' to complete your search string.  After
;;  you finish completing (e.g. by hitting `RET'), Isearch resumes
;;  with the new, completed search string.  It's pretty seamless, and
;;  easier to try than to describe.
;;
;;  One reminder: Using `S-TAB' vs `TAB' for (regexp vs non-regexp)
;;  completion against previous search strings has nothing to do with
;;  regexp vs non-regexp searching.  You can of course use either kind
;;  of searching before or after having used either kind of
;;  completion.  Isearch uses different search rings for regexp and
;;  non-regexp searching.  The kind of search in progress (regexp or
;;  not) at the moment you call for completion determines which search
;;  ring provides the candidates for completion.
;;
;;  ** Icicles Search Commands: `icicle-search' and `icicle-occur' **
;;
;;  The main idea behind the Icicles search commands `icicle-search'
;;  and `icicle-occur' is this: Regular expressions are powerful for
;;  searching, but they can also be cumbersome sometimes.  Why not use
;;  one simple regexp to set up a set of candidates and then use a
;;  second simple regexp to filter those candidates?  This is the same
;;  idea as that behind progressive completion with `M-*'.  (However,
;;  using `M-*' together with the search commands serves no purpose;
;;  its filtering is not cumulative in this context.)
;;
;;  Icicles search commands search the entire buffer, not just the
;;  part that follows the cursor.  If a region is active, however,
;;  then the search is confined to the region.
;;
;;  You've no doubt used standard Emacs command `occur'.  It finds all
;;  lines in a buffer that match a regexp that you enter.  It displays
;;  the matching lines as links in buffer *Occur* - you can click a
;;  link to navigate to the corresponding line in the original buffer.
;;  Using buffer *Occur* is similar to using the output of Emacs
;;  `grep' command.
;;
;;  Command `icicle-occur' is similar to `occur', but instead of
;;  entering a regexp (with `RET') you type a regexp and then use
;;  `S-TAB' to show the matching lines in buffer *Completions*.  As
;;  usual in Icicles, you can complete to a single candidate, or cycle
;;  among candidates to choose one.  To navigate to a match in the
;;  original buffer, use `C-RET', `C-mouse-2', `C-next', or `C-prior'.
;;  One advantage of `icicle-occur' is that you can change the regexp
;;  on the fly to match different sets of lines.  Another, major
;;  advantage is that you can use progressive completion to find lines
;;  using multiple, simple regexps.
;;
;;  Command `icicle-search' is a generalization of `icicle-occur'.
;;  You enter an initial regexp (using `RET'), which is used to define
;;  a set of completion candidates: all of the matching strings in the
;;  current buffer.  Command `icicle-occur' is really `icicle-search'
;;  with an implicit initial regexp of `.*' (you do not enter that).
;;  That is, the initial completion candidates for `icicle-occur' are
;;  all of the lines of the buffer.  With `icicle-search', the
;;  candidates need not be single, complete lines but can be any
;;  strings in the buffer, including multiple-line strings.  Your
;;  initial regexp is used over and over in the region or buffer that
;;  you search to find the set of matching strings, which then serve
;;  as completion candidates.
;;
;;  When you use `C-RET', `C-mouse-2', `C-next', or `C-prior', both
;;  `icicle-occur' and `icicle-search' highlight the current
;;  occurrence using face `icicle-search-main-regexp-current'.  For
;;  `icicle-occur', the current occurrence is the current line; for
;;  `icicle-search', it is whatever your initial regexp matches.
;;
;;  In addition, within the current occurrence, both `icicle-occur'
;;  and `icicle-search' highlight whatever your current input matches
;;  (using face `icicle-search-current-input').  For `icicle-search',
;;  this is whatever you type after you have entered the initial
;;  regexp with `RET' - typically, it is a second regexp used to
;;  refine the search.
;;
;;  User options for `icicle-search':
;;
;;  * `icicle-search-hook': Functions run after searching and moving
;;    to a match, whether by `RET' or `C-RET' (or `C-next' or
;;    `C-prior').  See the definition of command
;;    `icicle-compilation-search' for an example of its use.
;;
;;  * If `icicle-search-highlight-all-flag' is non-nil (the default
;;    value) then `icicle-search' highlights all of the matches for
;;    your initial regexp at once (using face
;;    `icicle-search-main-regexp-others').  The effect is similar to
;;    the Emacs 22+ lazy search highlighting of `isearch' (except that
;;    the highlighting is not in fact lazy).
;;
;;  * If `icicle-search-cleanup-flag' is non-nil (the default value)
;;    then search highlighting is removed after the search.  If you
;;    set this to nil then you can remove search highlighting manually
;;    later using command `icicle-search-highlight-cleanup'.  One use
;;    of nil `icicle-search-cleanup-flag' is to highlight regexp
;;    matches throughout a region or buffer.  In that capacity,
;;    `icicle-search' acts like some of the highlighting commands in
;;    my library `highlight.el'.
;;
;;  It can sometimes be useful to highlight all regexp matches using
;;  non-nil `icicle-search-highlight-all-flag' and nil
;;  `icicle-search-cleanup-flag', and then set
;;  `icicle-search-highlight-all-flag' to nil and use `icicle-search'
;;  again with a different regexp to search through the same region or
;;  buffer.  This lets you see the relation between the two sets of
;;  regexp matches.
;;
;;  You can use `icicle-search' to find text entities of a certain
;;  kind - sentences, paragraphs, file names, URLs, and so on.  A
;;  convenient way to do this is to use `C-=' in the minibuffer
;;  (`icicle-insert-string-from-variable') to insert a predefined
;;  regexp that matches a particular kind of text entity.
;;
;;  For example, suppose you are in a mail client and you want to move
;;  between mail headers.  If you use a regexp that matches the header
;;  field you want (e.g. the sent date or sender) then `icicle-search'
;;  highlights all such occurrences and lets you navigate among them -
;;  instant mail browser!  Or, suppose you are in a C++ or Perl file
;;  and you want to navigate among function or other definitions.  If
;;  you have a canned regexp that matches the start of a definition,
;;  then you can use `C-=' to quickly turn `icicle-search' into a code
;;  browser.  In a log file, navigate among date or time entries or IP
;;  addresses...  Of course, most programming modes and mail clients
;;  already provide other ways to navigate, but you get the idea -
;;  `icicle-search' provides a general way to navigate among things,
;;  as long as you can match them with regexps, and `C-=' lets you
;;  quickly access a library of predefined regexps.
;;
;;  You can find useful regexps to store in variables in the standard
;;  Emacs Lisp libraries.  Grep for `font-lock-keywords' or `regexp'
;;  in the Emacs `lisp' directory and its subdirectories.
;;
;;   See `align.el' for regexps for programming languages.
;;   See `url-dav.el' for regexps matching iso8601 dates.
;;   See `rmail.el', `sendmail.el', and `mh-show.el' for regexps
;;   matching mail-header fields.
;;
;;  Imenu regexps occurring as parts of different values of
;;  `imenu-generic-expression' for different buffer types can be used
;;  as variable values for `C-='.  They all work fine with
;;  `icicle-search', turning it into a navigator for the given mode.
;;  See, for example, `generic-x.el' and `lisp-mode.el'.  Here is a
;;  regexp for Javascript function definitions from `generic-x.el':
;;
;;   "^function\\s-+\\([A-Za-z0-9_]+\\)"
;;
;;  And `lisp-imenu-generic-expression' (in `lisp-mode.el') provides
;;  regexps for Lisp function, variable, and type definitions.  Here
;;  is the variable-definition regexp:
;;
;;   "^\\s-*(\\(def\\(c\\(onst\\(ant\\)?\\|ustom\\)\\|ine-symbol-macro
;;   \\|parameter\\|var\\)\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)"
;;
;;  You certainly don't want to type that into the minibuffer (and the
;;  function-definition regexp is twice as complex)!  Put it into a
;;  variable once and use `C-=' from then on to retrieve it - simple.
;;
;;  If it's so simple then why not let a command do it?  Done.  Just
;;  use command `icicle-imenu'; it is an ''Imenu browser''.  You don't
;;  need to bother looking up Imenu regexps and assigning them to
;;  variables for use with `C-=' and `icicle-search'.
;;
;;  If you look at the definition of `icicle-imenu' you'll see that it
;;  simply lets you choose an Imenu submenu appropriate for the
;;  current buffer type, and then it calls `icicle-search'.  That's
;;  the real point here: you can define your own commands similarly
;;  using `icicle-search' to browse regexp matches.  You get all of
;;  the features of `icicle-search' when you do this.  For example,
;;  `icicle-imenu' gives you these advantages over a standard Imenu
;;  menu:
;;
;;  * You can restrict navigation (search) to a region.
;;
;;  * You can navigate (browse) among multiple entries, instead of
;;    choosing them one by one from a menu.
;;
;;  * You can restrict the entries to browse using (regexp) pattern
;;    matching.
;;
;;  In sum: For complete interactivity, type a regexp dynamically as
;;  input to `icicle-search'.  For isolated special regexps that you
;;  use, save them in variables and use `C-=' with `icicle-search'.
;;  For well-defined sets of regexps, especially if used frequently,
;;  define a command that uses `icicle-search'.  There is a spectrum
;;  of use cases for `icicle-search'.
;;
;;  Command `icicle-search' is very general and very powerful.  It
;;  might never replace incremental search - either regexp or literal
;;  string search, but in some cases it can be quite handy.  Think of
;;  it as another tool to add to your search-tool belt.  Admittedly,
;;  it does take a little getting used to.  Remember, in particular,
;;  that the initial regexp you enter (with `RET') cannot be changed
;;  without re-executing `icicle-search'.
;;
;;  And remember too that `C-l' (`icicle-retrieve-last-input') is your
;;  friend - it clears the minibuffer during cycling, retrieving your
;;  last real input.  Use it to modify your second and subsequent
;;  regexps on the fly - those that filter the initial candidate list
;;  further.
;;
;;  Oh - And don't forget that you can do things like take the
;;  complement of your fine-tuning regexp matches, within the context
;;  of your coarse-tuning matches (see "Sets of Completion
;;  Candidates", above).  For example, use `^.*defun.*$' as the main
;;  regexp, to find all lines containing `defun'.  Then type `icicle'
;;  to match only the lines with `defun' that also contain `icicle'.
;;  Then complement (`C-~') that set, to see the lines that contain
;;  `defun' but not `icicle'.
;;
;;  And you can then save that set of matches, and then subtract it
;;  from another set of matches in a different search...  You get the
;;  idea.  When performing set operations combined with
;;  `icicle-search', keep in mind that the saved set does not include
;;  any position information - it is only a set of matching strings.
;;  So, in particular, a set-union operation (`C-+') is not useful
;;  with `icicle-search' (adding a saved set of strings without
;;  positions is useless).  Still, you can do things like match lines
;;  that contain `defun' followed somewhere by `()', and then subtract
;;  the (saved) set of lines in the same region that contain `icicle'.
;;  Try it in an Icicles library, using regexps `.*icicle.*$' and
;;  `^*.defun.*().*$'.
;;
;;  One more reminder: When you save a set of completion candidates
;;  (`C->'), make sure that you actually have a set of candidates to
;;  save!  It is not enough to just enter a regexp at the
;;  `icicle-search' prompt.  You must also use some Icicles command,
;;  such as `TAB', `S-TAB', `next', or `down' to tell Icicles how to
;;  create the candidate set - how to match the regexp.
;;
;;  See also:
;;
;;  * "Progressive Completion", above, for information on `M-*'
;;  * "Multi-Commands", above, for information on `C-RET',
;;    `C-mouse-2', `C-next', and `C-prior'.
;;  * "Customization and General Tips", below, for information on the
;;    `icicle-search-*' faces, which control `icicle-search'
;;    highlighting.
;;  * "Inserting a Regexp from a Variable", above, for more about
;;    `C-='.
;;  * The doc string (`C-h f') of `icicle-search' for information
;;    about cycling, multiple occurrences of candidates, and `RET'.

;;
;;
;;  Compile/Grep Search
;;  -------------------
;;
;;  In a compilation-results buffer, such as `*grep*', command
;;  `icicle-compilation-search' can be useful for searching among the
;;  result set (hits).  It is the same function as `icicle-search',
;;  except that it calls `compile-goto-error' as the
;;  completion-candidate action function.  That is, you can use
;;  `C-RET', `C-mouse-2', `C-prior', `C-next', `C-up', and `C-down' to
;;  display the target corresponding to each line in the compilation
;;  buffer that matches a regexp.  As for `icicle-search', you can
;;  further narrow the match candidates by typing a second regexp to
;;  search for among the first matches.
;;
;;  Altogether, using this with `grep' gives you two or three levels
;;  of regexp searching: 1) the `grep' regexp, 2) the major
;;  `icicle-search' regexp, and optionally 3) a refining
;;  `icicle-search' regexp.  And you can of course use progressive
;;  completion (`M-*') to add any number of additional levels.
;;
;;  See also:
;;
;;  * "Search Enhancements", above, for information on
;;    `icicle-search'
;;
;;  * "Multi-Commands", above, for information on using `C-RET',
;;    `C-mouse-2', `C-prior', `C-next', `C-up', and `C-down'
;;
;;  * "Progressive Completion", above, for information on using any
;;    number of regexps with `M-*'

;;
;;
;;  Using Complex Completion Candidates
;;  -----------------------------------
;;
;;  This section could also be called "Mapping over Sets", because it
;;  is about applying some function to members of a set of completion
;;  candidates.  You already know that you can manipulate sets of
;;  candidates - see "Sets of Completion Candidates", above.  The
;;  elements of those sets are strings; you choose candidate names.
;;  Sometimes, however, you need to choose among named items that are
;;  themselves complex, containing more information than just the
;;  name.  That is the idea behind command `icicle-map', which this
;;  section introduces.
;;
;;  You (or a command that you use) can obtain the information
;;  associated with a name after you choose the name, whether during
;;  multi-command cycling (see "Multi-Commands", above) or after final
;;  candidate choice (`RET').  This is what happens, for instance,
;;  when you use `find-file'; the command looks up the file associated
;;  with the name after you choose the name.  Likewise, for
;;  `icicle-bookmark' and other commands.  Multi-commands need to
;;  perform this lookup both when you use a candidate while cycling
;;  and when you make a final candidate selection.
;;
;;  Names and their associated information can be available in Emacs
;;  Lisp in the form of an association list (alist), that is, a list
;;  whose items are conses.  An alist is often used to represent a
;;  function or relation that maps one set of things to another.  The
;;  conses in the alist are the tuples (typically pairs) of related
;;  items.  The car of each cons is called its "key"; the cdr is
;;  called its "value".  Different alists have different kinds of keys
;;  and values.  Typical key types include symbols and strings;
;;  typical value types include symbols, strings, numbers, and lists.
;;  There are quite a few standard Emacs-Lisp variables whose value is
;;  an alist.  Most are internal variables, but some are user options.
;;  See the Emacs-Lisp manual for more about alists.
;;
;;  The completion mechanism of Emacs function `completing-read' takes
;;  an alist as input: the keys are completion-candidate strings.  For
;;  completion, however, the value (cdr) of each alist key/value entry
;;  is completely ignored.  Icicles uses `completing-read', so it
;;  works the same way.  If a command needs to access the value
;;  associated with a name, then it must somehow do so independently
;;  of completion.
;;
;;  Command `icicle-search' offers an example of this.  The completion
;;  alist contains key/value pairs whose car (key) is a buffer string
;;  that matches your search string and whose cdr (value) is the
;;  corresponding buffer position.  When you use completion with this
;;  command, you work only with the keys, but `icicle-search' keeps
;;  track of the corresponding positions for you as well.  The logic
;;  for doing this is coded into the definition of `icicle-search'.
;;
;;  It is common to want to do something interesting interactively
;;  with the values, not just the keys, of a completion alist.  Why
;;  lose the important value information when you choose a key?  And
;;  instead of requiring the logic of each command to deal with this
;;  need separately, why not provide a general mechanism for accessing
;;  this information - both by program and interactively?  This is
;;  what command `icicle-map' is for.
;;
;;  To make use of completion-alist values, you need to access the cdr
;;  of a key/value pair.  However, different alists are structured
;;  differently: the cdr can itself be complex (structured).  In
;;  general, you want to access the key/value pair as a whole, to do
;;  what you want with it, that is, to apply some function to it.
;;
;;  Emacs-Lisp programmers sometimes map functions over lists to
;;  obtain a different list.  For example, mapping the function `1+'
;;  over the list (3 1 4 1 5 9) gives the list (4 2 5 2 6 10).  The
;;  command `icicle-map' is inspired by this common practice, but it
;;  applies only to alists, and it lets you choose interactively which
;;  list elements to use.
;;
;;  Command `icicle-map' lets you apply a function of your choice
;;  (choose the function via completion, if you like) to any number of
;;  key/value entries in an alist.  The alist is used for completion,
;;  so you can choose among the possible keys.  But the function is
;;  applied to the corresponding key/value pairs, not to just the
;;  keys.
;;
;;  For example, given the alist `auto-mode-alist' and the function
;;  `cdr', you can choose to apply `cdr' to selected alist entries.
;;  This acts as a simple lookup function, because `cdr' just returns
;;  the value associated with a chosen key.  If you choose, for
;;  example, the candidate (key) "\.el\'", then the result is the
;;  symbol `emacs-lisp-mode'.  In this case, the chosen key/value pair
;;  is ("\\.el\\'" . emacs-lisp-mode).  (A literal backslash must be
;;  doubled in an Emacs-Lisp string.)
;;
;;  Function `cdr' returns the value, which is `emacs-lisp-mode' here.
;;  If, instead of `cdr', you use the function (lambda (x)
;;  (describe-function (cdr x))), then the result of choosing
;;  candidate "\.el\'" is to display the help for function
;;  `emacs-lisp-mode'.  Yes, you can type in a lambda expression when
;;  prompted for the function - any function can be used.  Keep in
;;  mind, however, that the function must target a key/value pair
;;  (cons).  This is why you could not simply use `describe-function'
;;  as the function here.
;;
;;  So what is `icicle-map' really for?  Anything you want.  You can
;;  use it to simply browse an alist or to perform actions on things.
;;  The idea is to let you take advantage of Icicles features to
;;  interactively filter and manipulate a set of completion keys, and
;;  then apply any function you like to them - not just to the keys,
;;  but to the keys or their values, or both.
;;
;;  You can use apropos (regexp) matching or prefix matching to filter
;;  the alist, as always, during completion.  You can use `C-next',
;;  `C-RET', and so on to act on (that is, apply the function to)
;;  selected key/value pairs that match your current input, or you can
;;  use `C-!' to act on *all* such pairs.  The latter case corresponds
;;  to mapping a function over all of the items in a list.
;;
;;  As an Emacs-Lisp programmer, you can use function `icicle-map'
;;  programmatically to let users look things up in alists that you
;;  construct or to act on selected alist entries in complex ways.
;;  Icicles just provides the interactive completion features.  The
;;  real value of `icicle-map' comes from what you do with it.  Use it
;;  with a database of geographical coordinates to look up location
;;  names provided by users and draw corresponding vicinity maps.  Use
;;  it with a list of hardware configurations to let users perform
;;  diagnostic or maintenance operations on selected equipment.  You
;;  get the idea - use your imagination.
;;
;;  Note: Although completion alists normally require string-valued
;;  keys, `icicle-map' is designed to work with any alist.

;;
;;
;;  Multi-Completions
;;  -----------------
;;
;;  Have you ever used standard Emacs command `apropos-documentation'?
;;  It searches the doc strings of all Emacs-Lisp symbols for matches
;;  to an input regexp, and displays the hits.  It can be useful when
;;  you don't remember the name of a function or variable but you can
;;  guess at terms that might occur in its doc string.  Typically,
;;  people resort to it only after first trying apropos commands that
;;  match against the function or variable name.
;;
;;  The idea behind `apropos-documentation' also motivates Icicles
;;  commands `icicle-doc', `icicle-vardoc' and `icicle-fundoc'.  These
;;  are multi-commands (see "Multi-Commands", above), so you can use
;;  `C-RET' and `C-next' to browse the regexp matches, displaying the
;;  documentation of each match in turn, and you can change the regexp
;;  to get different matches during the same command invocation.
;;
;;  Like `apropos-documentation', `icicle-doc' lets you match a regexp
;;  against the doc strings of functions and variables.  Commands
;;  `icicle-vardoc' and `icicle-fundoc' work a bit differently.  They
;;  let you match both the name and the doc string, using two
;;  different regexps.
;;
;;  The way `icicle-vardoc' and `icicle-fundoc' work is a bit
;;  inelegant (a hack), and it can take some getting used to.  You
;;  provide two regexps, one to match the name of a function or
;;  variable and one to match its doc string.  However, since
;;  completion candidates are not multipart, you actually type a
;;  single regexp that is the concatenation of the two.  You join
;;  these two regexps using `icicle-list-join-string' (a user option),
;;  which, by default, is `^G^J', that is, a control-G character
;;  followed by a control-J (newline) character.  As always, you input
;;  control characters using `C-q', so to input `^G^J' you use `C-q
;;  C-g C-q C-j'.
;;
;;  For instance, to match a function name that contains `dired' and a
;;  doc string that contains `file', you would use this:
;;
;;    M-x icicle-fundoc dired^G^Jfile
;;
;;  That is, you type:
;;
;;    M-x icicle-fundoc dired C-q C-g C-q C-j file
;;
;;  Well, almost.  The way it works is that the completion candidates
;;  are themselves formed by concatenating symbol names with their doc
;;  strings, using `icicle-list-join-string'.  Your input regexp is
;;  matched against those candidates.  This means that the input
;;  regexp `dired^G^Jfile' would actually match only function names
;;  that *end* with `dired' and doc strings that *begin* with `file'.
;;
;;  To match `file' against any part of the doc string, you must
;;  explicitly link the two component regexps with a regexp that
;;  matches anything.  If you want to search only the first lines of
;;  doc strings, you can use `.*' to do that: `dired.*^G^J.*file' will
;;  match all functions whose names contain `dired' and whose doc
;;  strings' first lines contain `file'.  This is because `.' does not
;;  match newline characters.
;;
;;  If you want to search the entire doc strings, then you can use a
;;  connecting regexp such as `[^^G]*', which matches any sequence of
;;  characters except character `^G'.  To match any character, even
;;  `^G', you can use `\(.\|^J\)'.  (It's too bad that Emacs doesn't
;;  have a dot-matches-newline-too option.)
;;
;;  Here, `.*' is used to match anything following `dired' in the
;;  function name, and `[^^G]*' is used to match anything (including
;;  newline) before `file' in the doc string.
;;
;;    M-x icicle-fundoc dired.*^G^J[^^G]*file
;;
;;  That is, you type:
;;
;;    M-x icicle-fundoc dired.* C-q C-g C-q C-j [^ C-q C-g]* file
;;
;;  These commands that accept a multipart regexp are examples of
;;  Icicles "multi-completion".  Icicles extends standard function
;;  `completing-read' so that it will accept, as the set of completion
;;  candidates, an alist argument whose candidates are not only
;;  individual strings but can also be lists of strings.  Each string
;;  in the list is one part of a multipart completion candidate, that
;;  is, a multi-completion.  The strings are joined together pairwise
;;  using `icicle-list-join-string' by `completing-read'.  Commands
;;  `icicle-fundoc' and`icicle-vardoc' each use lists of two strings
;;  (name and doc), but a multi-completion can have any number of
;;  strings.
;;
;;  Why is the default value of `icicle-list-join-string' so odd:
;;  `^G^J'?  You can use any string you like, but here is the
;;  rationale behind the default choice:
;;
;;  - ^G does not normally occur in simple strings such as doc strings
;;  - a newline (^J) visually separates the multiple component strings
;;  - ^G^J is not too difficult to enter: `C-q C-g C-q C-j'
;;
;;  It is important that the value of `icicle-list-join-string' not be
;;  something that is, itself, likely to match any of the candidates.
;;  Otherwise, it would not serve its role as separator.
;;
;;  As an Emacs-Lisp programmer, you can define other Icicles commands
;;  that use multi-completions.  Depending on your needs, you can of
;;  course bind `icicle-list-join-string' to a different separator.
;;
;;  Note that there is (only) a superficial similarity between Icicles
;;  multi-completion and the functionality provided by function
;;  `completing-read-multiple' of standard Emacs library `crm.el'.
;;  The latter lets you complete multiple strings in the minibuffer,
;;  one at a time.  It involves ordinary Emacs prefix completion, and
;;  it uses the same set of completion candidates for each of the
;;  strings in the input.
;;
;;  By contrast, Icicles multi-completion completes each part of your
;;  input against a different set of completion candidates.  For
;;  example, when you use `icicle-vardoc', it completes the
;;  variable-name part of your input against the names of defined
;;  variables, and the variable-description part against the doc
;;  strings of defined variables.  Standard Emacs command
;;  `completing-read-multiple' lets you complete several different
;;  variable names at the same minibuffer prompt, but they each
;;  complete against the same set of variable names.
;;
;;  Multi-completion matches a list of regexps in parallel.  See also
;;  the description of `M-*', which matches a list of regexps in
;;  series: "Progressive Completion".

;;
;;
;;  Completion in Other Buffers
;;  ---------------------------
;;
;;  In addition to input completion, you can use Icicles to complete
;;  words and symbols in other buffers, besides the minibuffer.
;;  Icicles enhances this completion in these ways:
;;
;;  1. Lisp symbol completion via `ESC-TAB' (`lisp-complete-symbol')
;;
;;  2. word completion using the dynamic abbreviation of standard
;;     Emacs library `dabbrev.el'
;;
;;  3. word completion using the words and phrases in a thesaurus
;;
;;  Because these enhancements use Icicles completion, you must use
;;  `RET' (or `S-RET') to confirm completion.  This is one difference
;;  in completion behavior that you will notice.  The other difference
;;  is that you have all Icicles features available to you: apropos
;;  (regexp) completion, cycling of candidates, and so on.
;;
;;  ** Dynamic Abbreviation **
;;
;;  Library `dabbrev.el' lets you type a few characters in a buffer
;;  and then prefix-complete them (in the same buffer) to a full word
;;  or symbol name.  The completion candidates come from words or
;;  symbol names in buffers that you are editing.  This functionality
;;  is called "dynamic abbreviation", though that is not a very good
;;  term for it (words are completed, not abbreviated, dynamically).
;;
;;  In Emacs, there are two ways to "dynamically abbreviate" text:
;;
;;  a. `M-/' (command `dabbrev-expand') completes to a candidate word.
;;     Repeating it replaces the completion with a different one -
;;     that is, it cycles candidates in the text buffer (not in the
;;     minibuffer).
;;
;;  b. `M-C-/' (command `dabbrev-completion') completes to the common
;;     root of all completion candidates.  Repeating it displays
;;     buffer *Completions* for you to choose a candidate.  However,
;;     in this case, there is no way to cycle among the candidates.
;;
;;  If there are many candidate completions, then cycling among them
;;  with `M-/' can be tedious.  You can use `M-C-/' to complete to a
;;  common root, thus narrowing the set of candidates, but then you
;;  lose the ability to cycle among them.
;;
;;  If user option `icicle-redefine-standard-commands-flag' is non-nil
;;  (which is the case by default), then Icicles redefines
;;  `dabbrev-completion' (it does not change `dabbrev-expand') so that
;;  it uses Icicles completion when you repeat `M-C-/'.  (Before
;;  repeating `M-C-/', the common root is completed as usual.)  You
;;  can then use any Icicles features, such as apropos completion and
;;  candidate cycling.
;;
;;  ** Thesaurus Completion **
;;
;;  Library `synonyms.el' provides various features for defining a
;;  thesaurus and looking up words and phrases in it.  Command
;;  `icicle-complete-thesaurus-entry' takes advantage of these
;;  features.  You can use it to complete a word in a text buffer to
;;  any word or phrase in the thesaurus.  If user option
;;  `icicle-bind-top-level-commands-flag' is non-nil (which it is by
;;  default), then this is bound to `C-c /'; if it is nil, then
;;  Icicles makes no global bindings.
;;
;;  Tip: You can use `icicle-complete-thesaurus-entry' to quickly
;;  check the spelling of a word.  If it is correctly spelled, then it
;;  appears as a complete completion (is highlighted as such in the
;;  minibuffer).
;;
;;  Another Icicles command that uses the thesaurus is
;;  `icicle-insert-thesaurus-entry'.  It lets you use Icicles
;;  completion, cycling, and so on to insert thesaurus words and
;;  phrases in any buffer.  It does not complete the word at point.
;;  It is a multi-command (see "Multi-Commands", above), so you can,
;;  within a single call to it, insert any number of thesaurus
;;  entries, in succession.  If you wanted to, you could write an
;;  entire book using a single call to
;;  `icicle-insert-thesaurus-entry'!
;;
;;  Both commands, `icicle-complete-thesaurus-entry' and
;;  `icicle-insert-thesaurus-entry', require that you first load
;;  library `synonyms.el'.  See library `synonyms.el' for more
;;  information.

;;
;;
;;  Customization and General Tips
;;  ------------------------------
;;
;;  This section contains some tips on using Icicles and descriptions
;;  of Icicles user options.
;;
;;  See Also:
;;
;;  * "File-Name and Directory-Name Completion Tips", below, for tips
;;    on using Icicles to complete file names
;;
;;  * "Dealing With Large Candidate Sets", above, for tips on
;;    improving performance when dealing with a large number of
;;    completion candidates
;;
;;  * "Customizing Key Bindings", below, for information on
;;    customizing Icicles key bindings
;;
;;  ** Using Icicles with Delete Selection Mode **
;;
;;  Icicles works especially well with Delete Selection mode, which I
;;  use and recommend.  (Likewise, for PC selection mode, which uses
;;  Delete Selection mode.)  In Delete Selection mode, whenever the
;;  region (selection) is active (highlighted), you can simply type to
;;  replace text in the region, or hit `DEL' (Backspace) or `C-d'
;;  (Delete) to delete the region.
;;
;;  However, library `delsel.el', which provides Delete Selection
;;  mode, binds keys in minibuffer maps that are also bound by
;;  Icicles.  For this reason, if you use both Icicles and Delete
;;  Selection mode, you must turn on Icicle mode after you turn on
;;  Delete Selection mode.  If you forget to do this, you will notice
;;  that `C-g' does not abort minibuffer input.  The remedy is simply
;;  to turn Icicle mode off, then on again.
;;
;;  ** Icicles User Options and Faces **
;;
;;  There are several user options (variables) and faces that Icicles
;;  defines, and you can also use various standard user options,
;;  including Icomplete options, that control various aspects of
;;  completion.
;;
;;  * Case sensitivity: Standard user options `completion-ignore-case'
;;    and `read-file-name-completion-ignore-case' (for Emacs 21 and
;;    later) control whether completion distinguishes between
;;    uppercase and lowercase letters.
;;
;;  * Face `icicle-region-background' and user options
;;    `icicle-point-position-in-candidate',
;;    `icicle-mark-position-in-candidate', and
;;    `icicle-change-region-background-flag' are all used to define
;;    the region (the selected text) when cycling completion
;;    candidates.  They are described below individually.  The region
;;    is active when cycling, so you can easily delete it or replace
;;    it.
;;
;;  * User option `icicle-point-position-in-candidate' defines the
;;    minibuffer cursor position (point) while cycling candidate
;;    completions.  By default, the cursor is placed at the end of the
;;    root being completed.  You can instead place it at the root
;;    beginning or at the beginning or end of the complete minibuffer
;;    input.  For file-name input, the beginning of minibuffer input
;;    starts after the directory name (which is inserted
;;    automatically).
;;
;;  * Similarly, user option `icicle-mark-position-in-candidate'
;;    defines the position of the mark; by default, it is at the end
;;    of the input.  Together, these two options control the size and
;;    placement of the region in a flexible way.  You can make the
;;    region include all of the input, only the root, from beginning
;;    to root, or from root to end.  You can put the cursor at either
;;    end of the region.  You can get rid of the region altogether, by
;;    making point and mark coincide (at any of the possible
;;    positions).
;;
;;  * Because the region background color is often quite different
;;    from the frame background color (in order to have it stand out),
;;    it can be a bit hard to read the completion candidates when the
;;    region is highlighted during input cycling.  If user option
;;    `icicle-change-region-background-flag' is non-nil, however, then
;;    the region background is changed to a color that differs only
;;    slightly from the frame background, making it easier to read the
;;    completion candidates.  The actual background color used is the
;;    value of `icicle-region-background', which you can customize.
;;    If you make this color the same as the frame background, then
;;    the region background is, in effect, invisible.
;;
;;  * The default value of `icicle-change-region-background-flag' is
;;    determined by the current value of `delete-selection-mode', that
;;    is, whether or not Delete Selection mode is enabled, when
;;    Icicles is loaded.  For this reason, if you use Delete Selection
;;    mode and you want the region background to change in the
;;    minibuffer, you should either turn on Delete Selection mode
;;    before loading `icicles.el' or explicitly customize
;;    `icicle-change-region-background-flag' to non-nil.
;;
;;  * User option `icicle-init-value-flag' controls the treatment of a
;;    default value for minibuffer input.  This includes not only
;;    functions that read input with completion (`completing-read',
;;    `read-file-name'), but also other input-reading functions:
;;    `read-from-minibuffer' and `read-string'.  Non-nil means to
;;    automatically insert the default value into the minibuffer as an
;;    initial value.  Standard Emacs behavior is for the default value
;;    not to be inserted.  I prefer to have it inserted, as I often
;;    use the default value (perhaps editing it).  The option is nil
;;    by default only because people are not used to the (better)
;;    behavior of `insert'.  I recommend that you try `insert' for a
;;    while, before giving up on it.  If you leave this as nil,
;;    remember that you can always insert the default value manually
;;    with `M-n'.
;;
;;  * The particular non-nil value of `icicle-init-value-flag'
;;    controls whether or not the initial value is preselected, and,
;;    if preselected, where to leave the cursor: at the beginning or
;;    end of the value.  Preselecting the value can be useful in
;;    Delete Selection mode or PC Selection mode, because it makes it
;;    easy to replace that value by typing characters, or delete it by
;;    hitting `DEL' (Backspace) or `C-d' (Delete).  However, all of
;;    the initial input is lost if you type or hit `C-d' or `DEL',
;;    which is inconvenient if you want to edit it only slightly.
;;
;;  * User option `icicle-thing-at-point-functions' controls the
;;    behavior of `M-.' in the minibuffer, which grabs text from the
;;    current buffer and yanks it into the minibuffer. See "Inserting
;;    Text Found Near the Cursor", above, and the doc string (`C-h v')
;;    of `icicle-thing-at-point-functions' for more information.
;;
;;  * User option `icicle-input-string' is a regexp string that is
;;    inserted in the minibuffer when you use `C-='.  See "Inserting a
;;    Regexp from a Variable", above.
;;
;;  * Face `icicle-current-candidate-highlight' highlights the current
;;    completion candidate in buffer *Completions*.  Face
;;    `icicle-complete-input' highlights minibuffer input when it is
;;    complete.  Faces `icicle-match-highlight-minibuffer' and
;;    `icicle-match-highlight-Completions' highlight whatever your
;;    input matches, in the minibuffer and in buffer *Completions*,
;;    respectively.  Face `icicle-common-match-highlight-Completions'
;;    highlights the longest common match among all completions,
;;    provided user option `icicle-expand-input-to-common-match-flag'
;;    is non-nil.
;;
;;  * Top-level command `icicle-search' uses several faces to
;;    highlight found text that matches your input.  Faces
;;    `icicle-search-main-regexp-current' and
;;    `icicle-search-main-regexp-others' highlight what your initial
;;    regexp (entered with `RET') matches.  The former highlights only
;;    the current match; the latter highlights all other matches.
;;    Face `icicle-search-current-input' highlights what your current
;;    input (typically another regexp) matches; that is, it highlights
;;    a match within the initial-regexp match.
;;
;;  * User option `icicle-search-hook' is a list of functions to be
;;    run after searching and moving to an `icicle-search' match,
;;    whether you move there by `RET', `C-RET', `C-next', or
;;    `C-prior'.  See the definition of command
;;    `icicle-compilation-search' for an example of its use.
;;
;;  * User option `icicle-search-highlight-all-flag' controls
;;    highlighting with face `icicle-search-main-regexp-others': if
;;    nil, no such highlighting occurs - only the current match is
;;    highlighted.  The effect a non-nil value (the default) has is
;;    similar to the Emacs 22+ lazy search highlighting of `isearch'
;;    (except that the highlighting is not in fact lazy).
;;
;;  * Non-nil user option `icicle-search-cleanup-flag' (the default)
;;    means that `icicle-search' highlighting is removed after the
;;    search.  If you set this to nil then you can remove search
;;    highlighting manually later using command
;;    `icicle-search-highlight-cleanup'.  One use of nil
;;    `icicle-search-cleanup-flag' is to highlight regexp matches
;;    throughout a region or buffer.  In that capacity,
;;    `icicle-search' acts like some of the highlighting commands in
;;    my library `highlight.el'.
;;
;;  * User option `icicle-show-Completions-initially-flag' controls
;;    whether or not buffer *Completions* is shown initially, without
;;    your needing to hit `TAB' or `S-TAB' to show it.  The default
;;    value is nil, meaning that *Completions* is not shown until you
;;    hit `TAB' or `S-TAB'.  More typical than setting this option to
;;    non-nil globally is to bind it to non-nil in Emacs-Lisp code, to
;;    display *Completions* as a menu. For example, pass a non-nil
;;    binding to `icicle-define-command' to create a command that
;;    displays a multiple-choice menu.  As an alternative to a non-nil
;;    `icicle-show-Completions-initially-flag', you can set option
;;    `icicle-incremental-completion-flag' to a value that is neither
;;    nil nor t; that will display *Completions* as soon as you type
;;    or delete input (but not initially).
;;
;;  * User option `icicle-incremental-completion-flag' controls
;;    whether or not *Completions* is updated incrementally
;;    (icompletion) as you type.  You can toggle incremental
;;    completion at any time using `C-#'.  For more information, see
;;    "Icompletion", above.
;;
;;  * User options `icicle-incremental-completion-delay' and
;;    `icicle-incremental-completion-threshold' together cause a delay
;;    before incremental completion takes effect.  See section
;;    "Icompletion", above.
;;
;;  * User option `icicle-Completions-frame-at-right-flag' controls
;;    whether `icicle-candidate-action' moves the frame showing buffer
;;    *Completions* to the right, out of the way of other frames.
;;    This can be useful if you use one-buffer-per-frame (non-nil
;;    `pop-up-frames').  In that case, I recommend that you also try
;;    my library `oneonone.el'.  See section "Note on Non-Nil
;;    `pop-up-frames' on MS Windows", below, for more advice about
;;    non-nil `pop-up-frames'.
;;
;;  * User option `icicle-sort-function' controls the order of
;;    completion candidates during cycling and in buffer
;;    *Completions*.  If nil, then no sorting is done.  If non-nil,
;;    then the value must be a string-comparison function - the
;;    function is passed to the standard function `sort' to do the
;;    sorting.  The default value for `icicle-sort-function' is
;;    `string-lessp', which sorts alphabetically.  You can toggle
;;    sorting at any time using `C-,'.  If you are an Emacs-Lisp
;;    programmer and you write new commands using Icicles
;;    functionalities, you can bind `icicle-sort-function' temporarily
;;    to any sort function you need.
;;
;;  * User options `icicle-require-match-flag' and
;;    `icicle-buffer-require-match-flag' let you override the value of
;;    the REQUIRE-MATCH argument provided to `completing-read' or
;;    `read-file-name'.  They are provided mainly for use (binding) in
;;    `icicle-define-command' and `icicle-define-file-command', but
;;    you may also use them globally, if you wish.  See "Exiting the
;;    Minibuffer Without Confirmation: `S-RET'", above.
;;
;;    A typical use is made in the definition of command
;;    `icicle-buffer': `icicle-buffer-require-match-flag' is used to
;;    bind `icicle-require-match-flag', so that you can, for example,
;;    match only existing buffers and be able to match on partial
;;    input without explicitly completing (hitting `TAB' or `S-TAB').
;;    Simply set the option to `partial-match-ok' to get this
;;    behavior.  To apropos-complete and exit the minibuffer, use
;;    `S-RET' instead of `RET'.  See "Exiting the Minibuffer Without
;;    Confirmation: `S-RET'", above, for more information.
;;
;;  * Non-nil user option `icicle-ignore-space-prefix-flag' means to
;;    ignore completion candidates that start with a space.  However,
;;    such candidates are not ignored for prefix completion if the
;;    input also starts with a space.  Naturally, apropos completion
;;    is not affected by whether or not the input starts with a space.
;;
;;    Option `icicle-buffer-ignore-space-prefix-flag' lets you
;;    override the value of `icicle-ignore-space-prefix-flag' for use
;;    with buffer-name completion (the names of internal buffers start
;;    with a space).  It is provided mainly for binding in
;;    `icicle-define-command' (`icicle-buffer' does this).
;;
;;    You can toggle `icicle-ignore-space-prefix-flag' at any time
;;    using `C-^' in the minibuffer.  If the current command binds
;;    this option locally, then it is the local, not the global, value
;;    that is changed.  For example, if
;;    `icicle-buffer-ignore-space-prefix-flag' is non-nil, then `C-^'
;;    toggles `icicle-ignore-space-prefix-flag' to nil only for the
;;    duration of `icicle-buffer'.
;;
;;  * Non-nil user option `icicle-regexp-quote-flag' reduces apropos
;;    completion to simple substring completion.  Regexp special
;;    characters are no longer recognized as special in this case;
;;    they simply match themselves.  You probably do not want to
;;    customize this option.  Instead, you can toggle it at any time
;;    using `C-`' in the minibuffer.
;;
;;  * User option `icicle-redefine-standard-commands-flag' controls
;;    whether Icicles redefines some standard commands, enhancing them
;;    to use Icicles completion.  A non-nil value causes redefinition.
;;
;;  * User options `icicle-buffer-match-regexp',
;;    `icicle-buffer-no-match-regexp', `icicle-buffer-predicate', and
;;    `icicle-buffer-extras' determine the behavior of commands
;;    `icicle-buffer' and `icicle-buffer-other-window'.  They
;;    determine the set of buffer-name candidates initially available
;;    for completion.  The first three restrict this set to names that
;;    satisfy the properties they specify.  Option
;;    `icicle-buffer-extras' lets you add additional buffer names to
;;    the set of candidates, after restriction by the other options.
;;    Since these are user options, they provide an additional, more
;;    static way to filter the set of candidates.  Typing input
;;    (e.g. a regexp) then dynamically filters the result of applying
;;    the filter options.
;;
;;  * User option `icicle-buffer-sort' is a predicate used to sort
;;    buffer-name candidates in commands `icicle-buffer' and
;;    `icicle-buffer-other-window'.  One possible value is
;;    `icicle-buffer-sort-*...*-last', which sorts names of internal
;;    buffers, which begin with `*', after other buffer names.
;;
;;  * User option `icicle-buffer-configs' is a list of named
;;    configurations of options `icicle-buffer-match-regexp',
;;    `icicle-buffer-no-match-regexp', `icicle-buffer-predicate',
;;    `icicle-buffer-extras', and `icicle-buffer-sort'.  You use
;;    command `icicle-buffer-config' to choose one of the
;;    configurations to be current.  You can use commands
;;    `icicle-add-buffer-config' and `icicle-remove-buffer-config' to
;;    add and remove configurations from the list.
;;
;;    Example: A configuration such as the following, named "Files and
;;    Scratch", defines `icicle-buffer-predicate' to display only file
;;    buffers, and it defines `icicle-buffer-extras' to include the
;;    extra buffer `*scratch*':
;;
;;     ("Files and Scratch" nil nil
;;      (lambda (bufname) (buffer-file-name (get-buffer bufname)))
;;      ("*scratch*") icicle-sort-function)
;;
;;    The idea of buffer-option configurations was borrowed from
;;    library `bs.el', by Olaf Sylvester <olaf@geekware.de>.
;;
;;  * User option `icicle-list-join-string' is described in
;;    "Multi-Completions", above.  It is the separator string that
;;    joins together the parts of a multi-completion.
;;
;;  * User options `icicle-regexp-search-ring-max' and
;;    `icicle-search-ring-max' act as `regexp-search-ring-max' and
;;    `search-ring-max', respectively, when you are in Icicle mode.
;;    (When you exit Icicle mode, `regexp-search-ring-max' and
;;    `search-ring-max' are restored.)  The reason for having these
;;    options is that with Icicles you will likely want to use a much
;;    longer search history.  By default, these are as large as
;;    possible (virtually unlimited).
;;
;;    Suggestion: If you use library `savehist.el' (recommended),
;;    customize `savehist-additional-variables' to include variables
;;    `search-ring' and `regexp-search-ring', so that your search
;;    histories will be saved between Emacs sessions.
;;
;;    Note: You can clear (empty) a given search history with command
;;    `icicle-clear-option'.  For example, to clear the
;;    regular-expression search history, do this:
;;
;;      `C-u M-x icicle-clear-option RET regexp-search-ring RET'
;;
;;    (The `C-u' is needed because this variable is not a user
;;    option.)  If you use my library `misc-cmds.el', you can clear
;;    search histories easier, using commands `clear-search-history',
;;    `clear-regexp-search-history', and `clear-search-histories'.
;;
;;  * User option `icicle-reminder-prompt-flag' controls the addition
;;    of a reminder about Icicles bindings to the minibuffer prompt.
;;    This is the reminder: "(<S-tab>, TAB: list, C-h: help)".  If
;;    this is nil or 0, the reminder is never displayed.  If this is
;;    t, the reminder is always displayed.  If this is a whole number,
;;    the reminder is displayed for that many Emacs sessions; it is
;;    not displayed thereafter. The default value is 20, meaning that
;;    you see the reminder during 20 Emacs sessions.  Regardless of
;;    the value of this option, the reminder only appears when there
;;    is sufficient minibuffer space.
;;
;;  * Non-nil user option `icicle-touche-pas-aux-menus-flag' means
;;    that Icicles will not add menu items to menu-bar menus, except
;;    for the Icicles and Minibuf menus.  Default value nil means that
;;    whenever an appropriate menu-bar menu exists, Icicles items are
;;    added to it.  For example, if nil, then Delete File is added to
;;    the File menu; otherwise it is added to the Icicles menu.  The
;;    value of this option is used only when Icicle mode is initially
;;    established, so changing it has no effect after Icicles has been
;;    loaded.  However, you can change it and save the new value, so
;;    it will be used next time.
;;
;;  * Non-nil user option `icicle-expand-input-to-common-match-flag'
;;    means that `S-TAB' (`apropos-complete') expands your minibuffer
;;    input to the longest common match among all completion
;;    candidates.  This replaces your regexp input, completing it as
;;    far as possible.  If you want to edit your original regexp
;;    input, use `C-l'.  If your input has been expanded, then hit
;;    `C-l' twice: once to replace a completion candidate (from, say,
;;    `next') with the common match string, and a second time to
;;    replace the common match string with your original regexp input.
;;    If you want to always work with a regexp in the minibuffer, then
;;    set this option to nil.  See "Longest-Common-Match Completion",
;;    above.
;;
;;  * Non-nil user option `icicle-arrows-respect-completion-type-flag'
;;    causes the vertical arrow keys (`up' and `down') to act
;;    differently during completion, depending on whether you precede
;;    their use by `TAB' or `S-TAB'.  The default vertical-arrow
;;    behavior if this option is non-nil is to traverse the input
;;    history.  After `TAB', `up' and `down' cycle prefix completions;
;;    after `S-TAB', they cycle apropos completions.  If this option
;;    is non-nil you can still use `M-p' and `M-n' to traverse the
;;    input history, `C-p' and `C-n' to cycle prefix completions, and
;;    `prior' and `next' to cycle apropos completions.  If you do
;;    that, you need not use `TAB' and `S-TAB' to switch between the
;;    two completion types.  Once you have used `TAB' or `S-TAB', the
;;    only way to traverse the history is via `M-p' and `M-n'.
;;
;;  * Non-nil option `icicle-highlight-input-initial-whitespace-flag'
;;    uses face `icicle-whitespace-highlight' to highlight any
;;    whitespace that starts your minibuffer input.  This is done to
;;    help you recognize accidentally typing such whitespace.
;;    Otherwise, you might not understand the set of matching
;;    completion candidates (or lack thereof).  There is not
;;    necessarily anything wrong with input that starts with
;;    whitespace - it might be what you want, but without this
;;    highlighting it is easy to not notice the whitespace.
;;
;;  * Non-nil option `icicle-bind-top-level-commands-flag' binds
;;    several top-level Icicles commands to key sequences.  It does
;;    this by loading library `icicles-keys.el'.  Top-level commands
;;    are commands that are not used only in the minibuffer.  Be aware
;;    that this changes some standard key bindings.  For example, it
;;    substitutes `icicle-kill-buffer' for `kill-buffer' (binding it
;;    to whatever `kill-buffer' was bound to).  If you want to bind
;;    top-level Icicles commands but you want different bindings from
;;    those defined in `icicles-keys.el', then you can add your own
;;    bindings, either in place of, or after, loading library
;;    `icicles-keys.el'.

;;
;;
;;  File-Name and Directory-Name Completion Tips
;;  --------------------------------------------
;;
;;  This section contains some tips on completing file and directory
;;  names.  See also section "Customization and General Tips", above,
;;  for general tips about using Icicles.  Many of those tips apply
;;  also to file-name and directory-name completion.
;;
;;  * Function `icicle-sort-dirs-last' is provided as a possible value
;;    for user option `icicle-sort-function'.  It treats file and
;;    directory names specially, sorting directory names after file
;;    names; otherwise, it is the same as `string-lessp'.  (You can of
;;    course reach directory names before, instead of after, file
;;    names, by using `up' and `prior' instead of `down' and `next'.)
;;
;;  * User option `icicle-cycle-into-subdirs-flag' controls whether or
;;    not minibuffer-input cycling explores subdirectories.  By
;;    default, it is nil, meaning that cycling does not descend into
;;    subdirectories.
;;
;;    non-nil - When this option is non-nil, you might want to use a
;;          function such as `icicle-sort-dirs-last' for option
;;          `icicle-sort-function', to prevent cycling depth first
;;          into the subdirectories.
;;
;;    nil - When this option is nil, you can still choose to cycle
;;          into a given directory (which is why nil is the default
;;          value).  When cycling reaches a candidate directory that
;;          you want to cycle through, just: 1) move the cursor
;;          (e.g. `C-e'), 2) hit `TAB' or `S-TAB' to "complete" the
;;          candidate, and then 3) use any of the cycle keys, such as
;;          `up', to cycle within the candidate directory.
;;
;;          Although the candidate directory was already completed by
;;          cycling, moving the cursor and explicitly "completing" it
;;          tells Icicles that you want to treat the candidate in the
;;          minibuffer as real input, just as if you had typed it, not
;;          merely as a cycling candidate.
;;
;;  * You can use `..' during completion to access a parent directory,
;;    and you can use `/' and `~/' to shadow input to the left.  There
;;    is currently no special treatment of MS Windows drive letters
;;    (e.g. `C:') - I use Cygwin on Windows.
;;
;;  * Standard user option `completion-ignored-extensions' controls
;;    which file names are ignored for completion and completion
;;    cycling.  You can toggle this ignoring at any time using command
;;    `icicle-toggle-ignored-extensions', bound to `C-.' in the
;;    minibuffer.
;;
;;  * Remember that you can use a regular expression to
;;    apropos-complete file names.  This is a powerful feature.  Do
;;    not confuse its use with the ability to use shell wildcards to
;;    access multiple files at once.  For example, if you use `C-x 4 f
;;    *.el RET', then all files with suffix `el' will be opened.
;;    Regexp matching is used only for apropos (not prefix) completion
;;    and cycling.  See section "What About Special-Character
;;    Conflicts?", above.
;;
;;  * You can use `$' for both environment variables and as a regexp
;;    special character.  For example, you can use a pattern such as
;;    `$HOME.*t$' to match the files in your home directory (`$HOME')
;;    whose names end in `t'.  See section "What About
;;    Special-Character Conflicts?", above.
;;
;;  * You can use the idiom `\W$' as input to match only directories,
;;    when a command asks for a file or directory name.  The `\W' says
;;    to match any non word-syntax character.  The `$' says to match
;;    this at the end of the name.  This works because directory names
;;    appear as completion candidates with a trailing slash (`/'), and
;;    slash (`/') is about the only non word-syntax character that is
;;    likely to appear in file-name completions.  See section "What
;;    About Special-Character Conflicts?", above.
;;
;;  * You can use library `ffap.el', if you like, with Icicles, to
;;    pick up the file, directory, or URL name under the cursor.  All
;;    Icicles features are available during file-name and URL
;;    completion.  If you like `ffap.el', you might also like to try
;;    my extension library `ffap-.el'.  See also "Inserting Text Found
;;    Near the Cursor", above.
;;
;;  * Most Icicles commands that target file or directory names look
;;    only in the current directory (`default-directory').  This means
;;    that the directory part of the name is ignored for matching
;;    purposes.  You can thus use apropos completion to match a
;;    substring, without needing to prefix the substring with `.*'.
;;    For example, to match file `favorite-foo-file.bar' in directory
;;    `/some/path/to/my/', it is sufficient to use either `foo' or
;;    `/some/path/to/my/foo'.
;;
;;  * Some Icicles commands that target file names match your input
;;    against absolute file-name completion candidates.  This is the
;;    case for `icicle-recent-file', `icicle-locate-file', and
;;    `icicle-locate-file-other-window'.  These commands let you
;;    regexp-match against any part of the absolute file name,
;;    including directory components.  See "File-Name Input, Locating
;;    Files, and Persistent Candidate Sets", above.

;;
;;
;;  Key Bindings
;;  ------------
;;
;;  Icicles makes three kinds of key bindings:
;;
;;  1. global bindings (optional)
;;  2. Icicle-mode bindings
;;  3. minibuffer local bindings
;;
;;  These are discussed here in two sections, one for the optional
;;  bindings (#1) and one for the others (#2, #3).
;;
;;  Note that #2 and #3 are also optional in the sense that you can
;;  override them with any bindings you like.  You can prevent global
;;  bindings (#1) from being made in the first place.
;;
;;
;;  ** Global Bindings **
;;
;;  Global bindings are changed by Icicles only if user option
;;  `icicle-bind-top-level-commands-flag' is non-nil (which it is by
;;  default).  If you set this option to nil, then Icicles will _not
;;  change any of your global key bindings_.
;;
;;  There is an exception: In Icicle mode, various Icicles commands
;;  are added to menu-bar menus.  File commands are added to the File
;;  menu, and so on.  Those that do not belong naturally to any
;;  existing menu-bar menu are added to a new Icicles menu and
;;  existing menu Minibuf.  Whatever the menu they appear in, however,
;;  Icicles menu items are enabled only when Icicle mode is active.
;;  Those that are in a menu other than the Icicles menu have "[Icy]"
;;  prefixed to their name so you can easily identify them.
;;
;;  If you do not want Icicles to add items to menus besides Minibuf
;;  and Icicles, then set option `icicle-touche-pas-aux-menus' to
;;  non-nil.  See "Customizing Key Bindings", below.
;;
;;  There is one more thing to say about Icicles global bindings,
;;  should you decide to accept them: they are pretty innocuous.
;;  Either the same key sequences are not bound in vanilla Emacs or
;;  they are bound to similar commands.  And, in the latter case, the
;;  Icicles commands generally revert to doing what the standard
;;  commands do when you are not in Icicle mode.  In particular, the
;;  Icicles versions are multi-commands (see "Multi-Commands", above)
;;  that simply act as normal commands except in Icicle mode.
;;
;;  Icicles makes the following global bindings:
;;
;;  * `C-c C-s'        - `icicle-search'
;;  * `C-c C-s'        - `icicle-compilation-search' (in grep etc.)
;;  * `C-c /'          - `icicle-complete-thesaurus-entry'
;;  * `M-x'            - `icicle-execute-extended-command'
;;  * `ESC M-x', `M-`' - `icicle-execute-menu-command'
;;
;;  Icicles also substitutes all of the key bindings for some standard
;;  commands.  For example, instead of simply binding `icicle-buffer'
;;  to `C-x b', it binds it to all keys originally bound to standard
;;  command `switch-to-buffer'.  The following standard commands have
;;  their bindings coopted this way by Icicles commands:
;;
;;  Standard Command                   Icicles Command
;;
;;  `switch-to-buffer'.................`icicle-buffer'
;;  `switch-to-buffer-other-window'....`icicle-buffer-other-window'
;;  `find-file'........................`icicle-find-file'
;;  `find-file-other-window'...........`icicle-find-file-other-window'
;;  `kill-buffer'......................`icicle-kill-buffer'
;;
;;  Here are some other Icicles commands that you might want to bind
;;  to keys:
;;
;;  `icicle-add-buffer-candidate' -
;;                          Add buffer to those always shown
;;  `icicle-add-buffer-config' - Add to `icicle-buffer-configs'
;;  `icicle-apropos'      - `apropos', but shows matches
;;  `icicle-apropos-command' - Enhanced `apropos-command'
;;  `icicle-apropos-variable' - Enhanced `apropos-variable'
;;  `icicle-apropos-zippy' - Show matching Zippy quotes
;;  `icicle-bookmark'     - Jump to a bookmark
;;  `icicle-buffer-config' - Pick `icicle-buffer' options
;;  `icicle-buffer-list'  - Choose a list of buffer names
;;  `icicle-clear-option' - Set the value of a binary option to nil
;;  `icicle-color-theme'  - Change color theme
;;  `icicle-complete-thesaurus-entry' -
;;                          Complete word using thesaurus
;;  `icicle-completion-help' - Display Icicles help
;;  `icicle-customize-icicles-group' -
;;                          Customize Icicles options and faces
;;  `icicle-delete-file'  - Delete a file or directory
;;  `icicle-delete-windows-on' - Delete all windows for buffer
;;  `icicle-doc'          - Display the doc of a function or variable
;;  `icicle-font'         - Change the frame font
;;  `icicle-frame-bg'     - Change the frame background color
;;  `icicle-frame-fg'     - Change the frame foreground color
;;  `icicle-fundoc'       - Display the doc of a function
;;  `icicle-imenu'        - Navigate among Imenu entries
;;  `icicle-insert-thesaurus-entry' -
;;                          Insert a thesaurus entry
;;  `icicle-locate-file'  - Open a file located anywhere
;;  `icicle-map'          - Apply function to alist items
;;  `icicle-occur'        - `occur' + apropos icompletion
;;  `icicle-recent-file'  - Open a recently used file
;;  `icicle-remove-buffer-candidate' - 
;;                          Remove buffer from those always shown
;;  `icicle-remove-buffer-config' - 
;;                          Remove from `icicle-buffer-configs'
;;  `icicle-reset-option-to-nil' -
;;                          Set value of binary option to nil
;;  `icicle-save-string-to-variable' -
;;                          Save text for use with `C-='
;;  `icicle-search'       - Search with regexps & cycling
;;  `icicle-set-option-to-t' - Set value of binary option to t
;;  `icicle-toggle-option' - Toggle the value of a binary option
;;  `icicle-vardoc'       - Display the doc of a variable
;;
;;
;;  ** Minibuffer and Icicle-mode Bindings **
;;
;;  Again, other than the following exceptions, Icicles binds keys
;;  only in the Icicle-mode keymap and in minibuffer local keymaps:
;;
;;  * If `icicle-bind-top-level-commands-flag' is non-nil, then some
;;    Icicles commands are bound globally to keyboard key sequences.
;;
;;  * If `icicle-touche-pas-aux-menus' is nil, then some Icicles
;;    commands are added to menu-bar menus such as File and Options.
;;
;;  The following key bindings are made for the minibuffer completion
;;  keymaps.  They are in effect whenever you are using the minibuffer
;;  for input with completion (e.g. `completing-read',
;;  `read-file-name', `M-x').
;;
;;    Keys bound globally to `next-line' and `previous-line' are bound
;;    to `icicle-next-prefix-candidate' and
;;    `icicle-previous-prefix-candidate'.  Those are the commands
;;    that cycle candidate prefix completions.  By default, this means
;;    keys `down', `up', `C-n', and `C-p'.
;;
;;    Keys bound globally to `scroll-up' and `scroll-down' are bound
;;    to `icicle-next-apropos-candidate' and
;;    `icicle-previous-apropos-candidate'.  Those are the commands
;;    that cycle candidate apropos completions.  By default, this
;;    means keys `next', `prior', `C-v', and `M-v'.
;;
;;    Keys bound globally to `help-command' (`C-h', `f1') are bound to
;;    `icicle-completion-help': Pop up a *Help* buffer with
;;    information on using completion.
;;
;;    Keys bound globally to commands that perform simple text
;;    insertion, deletion, and transposition operations - commands
;;    such as `self-insert-command' - are bound to Icicles versions of
;;    those commands that do the same thing but also provide apropos
;;    icompletion.  This includes keys such as `C-d', `M-d', `C-y',
;;    `C-k', and `C-w' (and lots more).  See "Icompletion", above.
;;
;;    `insert' - `icicle-switch-to-Completions-buf': Move cursor to
;;               the current candidate in buffer *Completions*.
;;
;;    `TAB'    - `icicle-prefix-complete': Complete current input in
;;               minibuffer, as a prefix.  If there is more than one
;;               prefix-completion candidate, display them in buffer
;;               *Completions*, highlighting the common prefix.  This
;;               replaces `minibuffer-complete'.
;;
;;    `M-SPC' - `icicle-prefix-word-complete': Complete current input
;;               in minibuffer, as a prefix, a single word at a time.
;;               This replaces `minibuffer-complete-word'.  In fact,
;;               it is the value of `icicle-word-completion-key' that
;;               is bound to this command; `M-SPC' is the default
;;               value of this user option.
;;
;;    `SPC'    - `icicle-self-insert' (see above): Insert a space.
;;
;;    `S-TAB'  - `icicle-apropos-complete': Like `TAB', but use
;;               apropos completion.
;;
;;  If you prefer, you can use `up' and `down' for both prefix and
;;  apropos completion, as well as for input-history traversal - the
;;  behavior is determined by whether you have previously used `TAB'
;;  or `S-TAB'.  To obtain this modal behavior, set user option
;;  `icicle-arrows-respect-completion-type-flag' to non-nil.  The
;;  documentation here assumes the default value of nil.  See
;;  "Customization and General Tips", above.
;;
;;  The following minibuffer binding is made to clear minibuffer
;;  input, making it handy for editing and removing completions
;;  (e.g. default or initial values) in the minibuffer.  It has an
;;  alternative behavior when you are cycling minibuffer history
;;  items: it deletes the current item from the history.
;;
;;    `M-k' - `icicle-erase-minibuffer-or-history-element'
;;
;;  The following minibuffer binding can be used to get rid of a
;;  completion inserted during cycling, and retrieve the last real
;;  input:
;;
;;    `C-l' - `icicle-retrieve-last-input'
;;
;;  `C-l' also has another important use: You can use it to retrieve
;;  your last input in case you never actually entered that input (via
;;  `RET').  For example, suppose that you used `C-h v RET hook' to
;;  examine various hook variables, and you did this using`C-next' to
;;  display their documentation.  If you finished the command by just
;;  typing `C-g', then your input (`hook') was never really entered,
;;  so it is not available via the minibuffer history (`M-p').  You
;;  can retrieve it with `C-l', to use it again, in your next command.
;;
;;  You of course have the standard access to the minibuffer history,
;;  via `M-p', `M-n', `M-r', and `M-s'.  In addition to these, the
;;  following minibuffer bindings let you use apropos completion on
;;  the current minibuffer history list.  For explanation, see
;;  "History Enhancements", above.
;;
;;    `M-h'     - `icicle-history'
;;    `M-pause' - `icicle-keep-only-past-inputs'
;;
;;  The following minibuffer bindings let you act on candidate
;;  completions.  For explanation, see "Multi-Commands" and "Choose
;;  All Completion Candidates", above.
;;
;;    `C-RET'     - `icicle-candidate-action': current candidate
;;    `C-!'       - `icicle-all-candidates-action': all candidates
;;    `C-up'      - `icicle-previous-prefix-candidate-action'
;;    `C-down'    - `icicle-next-prefix-candidate-action'
;;    `C-prior'   - `icicle-previous-apropos-candidate-action'
;;    `C-next'    - `icicle-next-apropos-candidate-action'
;;    `C-mouse-2' - `icicle-mouse-candidate-action': clicked candidate
;;
;;  (The binding for `icicle-mouse-candidate-action' is actually in
;;  the *Completions* buffer.)
;;
;;  The following minibuffer bindings let you perform set operations
;;  on sets of completion candidates.  For explanation, see "Sets of
;;  Completion Candidates", above.
;;
;;    `C-~'     - `icicle-candidate-set-complement'
;;    `C--'     - `icicle-candidate-set-difference'
;;    `C-+'     - `icicle-candidate-set-union'
;;    `C-*'     - `icicle-candidate-set-intersection'
;;    `C->'     - `icicle-candidate-set-save': save current set
;;    `C-<'     - `icicle-candidate-set-retrieve': retrieve saved set
;;    `C-%'     - `icicle-candidate-set-swap': swap saved and current
;;    `C-:'     - `icicle-candidate-set-define': define current (Lisp)
;;
;;  The following minibuffer bindings insert text in the minibuffer:
;;
;;    `M-.'     - `icicle-insert-string-at-point'
;;    `C-='     - `icicle-insert-string-from-variable'
;;
;;  The following minibuffer bindings let you toggle Icicles options.
;;
;;    `C-,'     - `icicle-toggle-sorting'
;;    `C-.'     - `icicle-toggle-ignored-extensions': file extensions
;;    `C-^'     - `icicle-toggle-ignored-space-prefix'
;;    `C-#'     - `icicle-toggle-incremental-completion'
;;    `C-`'     - `icicle-toggle-regexp-quote'
;;
;;  The following bindings are made for `completion-list-mode', that
;;  is, for buffer *Completions*, which shows the list of candidate
;;  completions:
;;
;;    `left', `right' - Navigate backward & forward among candidates
;;    `up', `down'    - Navigate up & down among candidates
;;    `insert'        - `icicle-switch-to-minibuffer':
;;                        Move cursor to the minibuffer, with the
;;                        current *Completions* candidate as input
;;    `C-g', `q'      - `icicle-abort-minibuffer-input'
;;    `C-mouse-2'     - `icicle-mouse-candidate-action'
;;
;;  If you are used to using `down', `up', `C-n', and `C-p' for the
;;  minibuffer history, you can restore those bindings and bind the
;;  corresponding Icicles commands to different keys.  See
;;  "Customizing Key Bindings", below.

;;
;;
;;  Customizing Key Bindings
;;  ------------------------
;;
;;  See "Key Bindings", above, for a description of the key bindings
;;  defined by Icicles.
;;
;;  Key bindings are very personal choices, and reflect preferences
;;  and habits, as well as keyboard and other configurations.  You
;;  might want to change some of the bindings that Icicles creates.
;;  This section tells you how to do that.
;;
;;  However, before doing so, unless the default bindings present a
;;  hardware or OS configuration problem for you, please try using the
;;  default bindings for a while, before deciding that you want to
;;  change them.  Habit is a powerful persuader, but its advice is not
;;  always the best ;-).
;;
;;  There are two kinds of Icicles bindings:
;;
;;  * Additions to menu-bar menus
;;  * Minibuffer bindings
;;
;;  ** Customizing Menu-bar Menus **
;;
;;  Icicles normally adds items to appropriate existing menu-bar
;;  menus, such as File and Options, as well as to menu-bar menus
;;  Minibuf and Icicles, but you can prevent this, if you like.  If
;;  you do not want to add items to menus besides Minibuf and Icicles,
;;  then just set option `icicle-touche-pas-aux-menus-flag' to non-nil
;;  before loading Icicles.  The items in question are then added to
;;  the Icicles menu instead.
;;
;;  ** Customizing Minibuffer Bindings **
;;
;;  Note: If you are thinking about customizing key bindings just so
;;  you can use `up' and `down' for the minibuffer history, consider
;;  setting `icicle-arrows-respect-completion-type-flag' to t instead;
;;  it should give you the behavior you want.  If you still want to
;;  customize keys to do this, then see the example below.
;;
;;  To understand how you can modify Icicles minibuffer bindings, it
;;  helps to know how Icicles creates the default bindings.  For that,
;;  the best advice is to consult the Emacs-Lisp code in library
;;  `icicle-fn.el'.
;;
;;  Even if you are not very familiar with Emacs-Lisp, however, you
;;  should be able to do what you want by adapting the example in this
;;  section.
;;
;;  Suppose that you use `up', `down', `C-p', and `C-n' outside of
;;  Emacs to traverse history lists.  This is a common feature of
;;  various shells, for instance.  And suppose you want to use these
;;  keys similarly within Emacs.  Suppose that you decide to replace
;;  the Icicles minibuffer bindings for these keys with bindings to
;;  the history-traversal commands, and replace the history-traversal
;;  bindings of Icicles, `M-p' and `M-n', with bindings to the Icicles
;;  commands that are bound by default to `up', `down', `C-p', and
;;  `C-n'.  That is, suppose that you want to remap:
;;
;;    `previous-history-element'         to `up' and `C-p'
;;    `next-history-element'             to `down' and `C-n'
;;    `icicle-previous-prefix-candidate' to `M-p'
;;    `icicle-next-prefix-candidate'     to `M-n'
;;
;;  You can do that by inserting code such as the following into your
;;  init file (~/.emacs), before the code that requires (loads)
;;  library `icicles.el':
;;
;;  (add-hook 'icicle-mode-hook 'bind-my-icicles-keys)
;;  (defun bind-my-icicles-keys ()
;;    "Replace some default Icicles bindings with others I prefer."
;;    (dolist
;;        (map
;;          (append
;;           (list minibuffer-local-completion-map
;;                 minibuffer-local-must-match-map)
;;           (and (fboundp
;;                 'minibuffer-local-filename-completion-map)
;;                (list minibuffer-local-filename-completion-map))))
;;      (when icicle-mode
;;        (icicle-remap 'previous-line 'previous-history-element map)
;;        (icicle-remap 'next-line 'next-history-element map)
;;        (define-key map [?\M-p] 'icicle-previous-prefix-candidate)
;;        (define-key map [?\M-n] 'icicle-next-prefix-candidate))))
;;
;;  In the `global-map', command `next-line' is bound to `down' and
;;  `C-n'.  Icicles uses function `icicle-remap' to remap `next-line'
;;  to `icicle-next-prefix-candidate'.  Your `bind-my-icicles-keys'
;;  code then remaps it to `next-history-element'.  Likewise for
;;  `previous-line'.  Icicles does not change the standard Emacs
;;  minibuffer bindings for `M-p' and `M-n', so they are still
;;  `previous-history-element' and `next-history-element'.  You can
;;  just use `define-key' to change the bindings of `M-p' and `M-n'.
;;
;;  See also "Customization and General Tips", above, for information
;;  on other customizations, besides key bindings.

;;
;;
;;  Icicles Redefines Some Standard Commands
;;  ----------------------------------------
;;
;;  If user option `icicle-redefine-standard-commands-flag' is
;;  non-nil, then Icicles automatically redefines a few standard Emacs
;;  commands when you are in Icicle mode, enhancing them to use
;;  Icicles completion:
;;
;;    `customize-apropos', `customize-apropos-faces',
;;    `customize-apropos-groups', `customize-apropos-options',
;;    `dabbrev-completion', `lisp-complete-symbol',
;;    `repeat-complex-command'.
;;
;;  When you exit Icicle mode, the pre-Icicles definitions are
;;  restored.

;;
;;
;;  Defining Multi-Commands
;;  -----------------------
;;
;;  If you are an Emacs-Lisp programmer, you can define your own
;;  multi-commands (see "Multi-Commands", above).  You define a
;;  function that acts on a single object, and then use that function
;;  in a multi-command to act on either a single object or multiple
;;  objects.  There are lots of possible applications.
;;
;;  To write your own multi-command, make the command do this:
;;
;;  1. Call `completing-read' or `read-file-name', and perform some
;;     action on the completed input.
;;
;;  2. Bind `icicle-candidate-action-fn' to a function that performs
;;     an action on a completion candidate - possibly the same action.
;;
;;  #1 just lets people use the command normally, to perform the #1
;;  action on a completion candidate entered with `RET'.  Because of
;;  #2, people can perform the #2 action on any completion candidates,
;;  while still continuing to cycle or complete candidates.
;;  Typically, the actions for #1 and #2 are the same, but nothing
;;  prevents you from using different actions.
;;
;;  When internal variable `icicle-candidate-action-fn' is not bound,
;;  the default action is performed: display help on the current
;;  completion candidate.
;;
;;  As a illustration of what is involved, here is the definition of a
;;  command similar to `icicle-delete-file':
;;
;;  (defun delete-one-or-more-files ()
;;    "Delete one or more files that match the current input."
;;    (interactive)
;;    (let* ((icicle-candidate-action-fn
;;            'my-delete-file-or-directory) ; Action #2
;;           (the-file
;;            (condition-case fail
;;                (completing-read
;;                 "Delete file: "
;;                 (mapcar #'list (directory-files default-directory))
;;                 nil t)
;;              (error (error-message-string fail)))))
;;      (when the-file
;;        (icicle-delete-file-or-directory the-file)))) ; Action #1
;;
;;  Here, the function that acts on a single object (file) is
;;  `my-delete-file-or-directory'.  It is called on the result of
;;  `completing-read' (action #1), and it is also bound to
;;  `icicle-candidate-action-fn' (action #2), so that it will be
;;  applied to the current candidate via `C-RET'.
;;
;;  Command `icicle-all-candidates-action' (`C-!' -- see "Choose All
;;  Completion Candidates", above) reports (in buffer `*Help*') on the
;;  objects that it did not act upon successfully.  For this
;;  reporting, the function bound to `icicle-candidate-action-fn'
;;  (e.g. `my-delete-file-or-directory', above) should return `nil'
;;  for "success" and non-`nil' (for example, an error message) for
;;  "failure", whatever "success" and "failure" might mean in the
;;  context of use.
;;
;;  As illustrated by the definition of `delete-one-or-more-files',
;;  the logic of a multi-command implementation is a bit complex.  In
;;  fact, the necessary logic is more complex that what is shown here.
;;  There is, fortunately, an easier way to define such a command, as
;;  explained in section "Defining Icicles Commands", below.  It
;;  introduces simple-to-use macros that implement both #1 and #2 for
;;  you.

;;
;;
;;  Defining Icicles Commands
;;  -------------------------
;;
;;  This section is for Emacs-Lisp programmers.
;;
;;  ** Nothing To It! **
;;
;;  Defining a command that uses Icicles completion and cycling is
;;  simple: just call `completing-read' or `read-file-name' to read
;;  input, then act on that input.  Here, for instance, is a simple
;;  command that reads a font name and then changes the selected frame
;;  to use that font.  Completion and cycling are available, using all
;;  available font names as the pool of candidates.
;;
;;  (defun change-font ()
;;    "Change font of selected frame."
;;    (modify-frame-parameters
;;     (selected-frame)
;;     (list (cons 'font (completing-read
;;                        "Font: " (mapcar #'list (x-list-fonts "*"))
;;                        nil t)))))
;;
;;  Nothing could be simpler - just use `completing-read'!  Icicles
;;  does the rest.  This is the most important thing to learn about
;;  defining Icicles commands: you don't need to do anything except
;;  call `completing-read' or `read-file-name' as you would normally.
;;  Or at least as I ''hope'' you would normally.  I fear that many
;;  Emacs-Lisp programmers don't take advantage of `completing-read'
;;  when they could, using instead a function such as `read-string'.
;;
;;  ** Multi-Commands Are Not So Easy To Get Right **
;;
;;  Although defining a command that takes advantage of most Icicles
;;  features is effortless, what if you want to define a multi-command
;;  - that is, a command that takes advantage of an action function
;;  when cycling candidates, as described in "Multi-Commands" and
;;  "Choose All Completion Candidates", above?  In that case, things
;;  get much trickier, as hinted at in "Defining Multi-Commands",
;;  above.
;;
;;  Here's a definition of command `change-font' that takes advantage
;;  of an action function when cycling candidates:
;;
;;  1  (defun change-font ()
;;  2    "Change font of current frame."
;;  3    (interactive)
;;  4   (let* ((orig-frame (selected-frame))
;;  5          (orig-font (frame-parameter nil 'font))
;;  6          (icicle-candidate-action-fn
;;  7           ;; Perform the action on a candidate, without leaving
;;  8           ;; `completing-read'.  You can do this over and over.
;;  9           (lambda (font)
;;  10             (modify-frame-parameters orig-frame
;;  11                                      (list (cons 'font font))))))
;;  12     (condition-case nil
;;  13         (modify-frame-parameters
;;  14          orig-frame
;;  15          (list
;;  16           (cons 'font
;;  17                 ;; Perform the action on your final choice.
;;  18                 (completing-read
;;  19                  "Font: "
;;  20                  (mapcar #'list (x-list-fonts "*")) nil t))))
;;  21       ((quit error)
;;  22        (modify-frame-parameters
;;  23         orig-frame
;;  24         (list (cons 'font orig-font)))))))
;;
;;  As you can see, there is a lot more going on here than in the
;;  previous version, above.  These are the points to keep in mind,
;;  when defining such a command:
;;
;;  1. Save anything you need to restore, so you can, in effect, undo
;;     the action in case of `C-g' (lines 4-5).
;;
;;  2. Bind `icicle-candidate-action-fn' to the action to perform
;;     (lines 6-11).
;;
;;  3. Perform the action, using `completing-read' to provide the
;;     target candidate (lines 13-20).  Do this in the body of a
;;     `condition-case' (lines 12-24).
;;
;;  4. Restore the original context in the error-handling part of the
;;     `condition-case' (lines 22-24).  Include `quit' in the
;;     error-type list.
;;
;;  The above definition is not quite complete, in fact.  To let
;;  `icicle-all-candidates' be able to report on failures, the
;;  `icicle-candidate-action-fn' code should also trap errors and
;;  return nil as an error indicator.
;;
;;  In fact, things can get even hairier (much hairier) still, if the
;;  function at the core of your command does things like create a new
;;  frame - especially on MS Windows, with its click-to-focus window
;;  manager.  The action of `change-font' doesn't do that, but if it
;;  did, you would need to redirect the focus back to the minibuffer
;;  frame, using `select-frame-set-input-focus'.  As an illustration
;;  of what's involved, here's a definition that would deal with such
;;  problems.  It also traps `icicle-candidate-action-fn' errors,
;;  returning nil to report success and the error message to report
;;  failure.
;;
;;  (defun change-font ()
;;    "Change font of current frame."
;;    (interactive)
;;    (let* ((orig-buff (current-buffer))
;;           (orig-window (selected-window))
;;           (orig-frame (selected-frame))
;;           (orig-font (frame-parameter nil 'font))
;;           (icicle-candidate-action-fn
;;            (lambda (candidate)
;;              (condition-case action-fn-return
;;                  (progn
;;                    (modify-frame-parameters
;;                     orig-frame (list (cons 'font candidate)))
;;                    (select-frame-set-input-focus
;;                     (window-frame (minibuffer-window)))
;;                    nil) ; Return nil to report success.
;;                ;; Return error message to report error.
;;                (error (error-message-string action-fn-return))))))
;;      (condition-case act-on-choice
;;          (modify-frame-parameters
;;           orig-frame
;;           (list (cons 'font
;;                       (completing-read
;;                        "Font: " (mapcar #'list (x-list-fonts "*"))
;;                        nil t nil nil nil nil))))
;;        (quit (switch-to-buffer orig-buff)
;;              (modify-frame-parameters
;;               orig-frame (list (cons 'font orig-font))))
;;        (error (switch-to-buffer orig-buff)
;;               (modify-frame-parameters
;;                orig-frame (list (cons 'font orig-font)))
;;               (error (error-message-string act-on-choice))))))
;;
;;  ** Help Is On the Way **
;;
;;  That's a lot of (error-prone) work!  You obviously don't want to
;;  be doing that a lot.  Fortunately, help is on the way: macro
;;  `icicle-define-command'.  Here is how it could be used to define
;;  `change-font'.  The resulting generated code is similar to the
;;  mess shown above.
;;
;;  1  (icicle-define-command
;;  2   change-font "Change font of current frame."
;;  3   (lambda (font)
;;  4     (modify-frame-parameters orig-frame
;;  5                              (list (cons 'font font))))
;;  6   "Font: " (mapcar #'list (x-list-fonts "*"))
;;  7   nil t nil nil nil nil
;;  8   ((orig-frame (selected-frame))
;;  9    (orig-font (frame-parameter nil 'font)))
;;  10  nil
;;  11  (modify-frame-parameters orig-frame
;;  12                           (list (cons 'font orig-font)))
;;  13  nil)
;;
;;  That might not look very readable, but it is straightforward to
;;  use `icicle-define-command'.  The arguments to it are as follows:
;;
;;  Command name    (line 2)
;;  Doc string      (line 2)
;;  Action function (lines 3-5)
;;  Args passed to `completing-read' (lines 6-7)
;;  Additional bindings (lines 8-9)
;;  Additional initialization code (line 10)
;;  "Undo" code to run in case of error or quit (lines 11-12)
;;  Additional code to run at the end (line 13)
;;
;;  The following bindings are pre-included - you can refer to them in
;;  the command body:
;;
;;   `orig-buff'   is bound to (current-buffer)
;;   `orig-window' is bound to (selected-window)
;;
;;  Before running any "undo" code that you supply, the original
;;  buffer is restored, in case of error or user quit.
;;
;;  Most of the arguments to `icicle-define-command' are optional.  In
;;  this case, optional arguments were provided to save (lines 8-9)
;;  and then restore (lines 11-12) the original font and frame.
;;
;;  If the action function that you use to define a multi-command acts
;;  on a file name or a directory name, then you will want to use
;;  `icicle-define-file-command', instead of `icicle-define-command'.
;;  It defines commands that use `read-file-name', rather than
;;  `completing-read', to read their input.  As an example of its use,
;;  here is the definition of `icicle-find-file':
;;
;;  (icicle-define-file-command
;;   icicle-find-file "Visit a file or directory."
;;   find-file "File or directory: ")
;;
;;  See how easy it is to turn standard command `find-file' into an
;;  Icicles multi-command?  The arguments to
;;  `icicle-define-file-command' are the same as those to
;;  `icicle-define-command', except for arguments that are passed to
;;  `read-file-name' instead of `completing-read'.
;;
;;  Several top-level Icicles commands have been defined using
;;  `icicle-define-command' and `icicle-define-file-command'.  You can
;;  use their definitions as models.
;;
;;  `icicle-add-buffer-candidate' - Add buffer to those always shown
;;  `icicle-add-buffer-config' - Add to `icicle-buffer-configs'
;;  `icicle-bookmark'     - Jump to a bookmark
;;  `icicle-buffer'       - Switch to another buffer
;;  `icicle-buffer-config' - Choose a config for `icicle-buffer'
;;  `icicle-buffer-list'  - Choose a list of buffer names
;;  `icicle-clear-option' - Set the value of a binary option to nil
;;  `icicle-color-theme'  - Change color theme
;;  `icicle-delete-file'  - Delete a file or directory
;;  `icicle-doc'          - Display the doc of a function or variable
;;  `icicle-execute-extended-command' -
;;                          A multi-command version of `M-x'
;;  `icicle-find-file'    - Open a file or directory
;;  `icicle-font'         - Change the frame font
;;  `icicle-frame-bg'     - Change the frame background color
;;  `icicle-frame-fg'     - Change the frame foreground color
;;  `icicle-fundoc'       - Display the doc of a function
;;  `icicle-insert-thesaurus-entry' -
;;                          Insert thesaurus entry(s)
;;  `icicle-kill-buffer'  - Kill a buffer
;;  `icicle-locate-file'  - Open a file located anywhere
;;  `icicle-recent-file'  - Open a recently used file
;;  `icicle-remove-buffer-candidate' - 
;;                          Remove buffer from those always shown
;;  `icicle-remove-buffer-config' - 
;;                          Remove from `icicle-buffer-configs'
;;  `icicle-reset-option-to-nil' -
;;                          Set value of binary option to nil
;;  `icicle-set-option-to-t' -
;;                          Set value of binary option to t
;;  `icicle-toggle-option' - Toggle the value of a binary option
;;  `icicle-vardoc'       - Display the doc of a variable
;;
;;  For simplicity, the descriptions of these commands are singular
;;  actions (e.g. "kill a buffer"), but each of them can be used to
;;  act on any number of items any number of times (e.g. kill one or
;;  more buffers).
;;
;;
;;  See also: library `synonyms.el', which uses macro
;;  `icicle-define-command' to define command `synonyms'.  This
;;  command lets you use Icicles completion on input regexps when you
;;  search a thesaurus.

;;
;;
;;  Global Filters
;;  --------------
;;
;;  This section is for Emacs-Lisp programmers.
;;
;;  Which completion candidates get displayed?  To review:
;;
;;  1. The domain of discourse, that is, all possible candidates, is
;;     determined by the arguments to `completing-read',
;;     `read-file-name', or `M-x'.
;;
;;  2. You type something in the minibuffer.  This narrows the
;;     possible candidates to those that match your input.  Matching
;;     can be prefix-matching or apropos-matching.
;;
;;  Wouldn't it sometimes be useful to filter #1 in a global way,
;;  before filtering it with your input (#2)?  Functions
;;  `completing-read' and `read-file-name' take a predicate argument,
;;  so that can be used for global filtering.  However, those
;;  functions are usually called from some command, and it would also
;;  be useful to give end users, not just programmers, some way to
;;  globally filter candidates.
;;
;;  For example, if you have a command, like `icicle-buffer', that
;;  reads a buffer name and displays the buffer, some users might
;;  always be interested only in buffers that are associated with
;;  files.  They don't want to see possible candidates like
;;  `*scratch*' and `*Messages*'.  What they need is a way to apply a
;;  global predicate that limits candidates to file-buffer names - but
;;  they don't have access to the call to `completing-read' that is
;;  inside the command definition.
;;
;;  For this reason, some global filtering variables are provided by
;;  Icicles:
;;
;;    `icicle-must-match-regexp', `icicle-must-not-match-regexp',
;;    `icicle-must-pass-predicate', `icicle-extra-candidates'.
;;
;;  The first and second of these are regexps that candidates must
;;  match and must not match, respectively, in order for them to be
;;  displayed.  The third is a predicate that candidates must satisfy.
;;  The fourth is a list of extra candidates to display.  Any of the
;;  filters can be nil, in which case it has no effect.
;;
;;  Variable `icicle-extra-candidates' is not really a "filter".  It
;;  does not restrict the set of possible candidates - rather, it
;;  extends that set.
;;
;;  These global variables are internal variables - they are not meant
;;  to be customized.  If you are not an Emacs-Lisp programmer, you
;;  will not use these variables, but some commands that you use might
;;  provide corresponding global-filter user options.  Icicles
;;  provides customizable user options for command `icicle-buffer',
;;  for example:
;;
;;    `icicle-buffer-match-regexp'    - Regexp that buffers must match
;;    `icicle-buffer-no-match-regexp' - Regexp buffers must not match
;;    `icicle-buffer-predicate'       - Predicate buffer must satisfy
;;    `icicle-buffer-extras'          - Extra buffers to display
;;
;;  You might, for instance, customize `icicle-buffer-no-match-regexp'
;;  to not display file-buffers whose names end in `.elc', and
;;  customize `icicle-buffer-predicate' to show only buffers that are
;;  associated with files.  The former would use a value of "\\.elc$",
;;  and the latter would use a value such as this:
;;
;;     (lambda (bufname) (buffer-file-name (get-buffer bufname)))."
;;
;;  If you, as a programmer, write a command, and you want to expose
;;  global filters to users of the command, you should:
;;
;;  1. Create corresponding user options that can be customized.
;;  2. Bind the user options to the corresponding filtering variables.
;;
;;  If you use `icicle-define-command' or `icicle-define-file-command'
;;  to define a command (recommended), then you can simply pass the
;;  filter-variable bindings as part of the BINDINGS argument.
;;
;;  For example, here is the core definition of `icicle-buffer':
;;
;;   (icicle-define-command
;;    icicle-buffer                          ; Command name
;;    "Switch to a different buffer."        ; Doc string
;;    switch-to-buffer                       ; Action function
;;    "Switch to buffer: "                   ; completing-read args
;;    (mapcar (lambda (buf) (list (buffer-name buf)))
;;            (buffer-list))
;;    nil nil (buffer-name (if (fboundp 'another-buffer)
;;                             (another-buffer nil t)
;;                           (other-buffer (current-buffer))))
;;    nil nil nil
;;    ;; Filter bindings
;;    ((icicle-must-match-regexp icicle-buffer-match-regexp)
;;     (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
;;     (icicle-must-pass-predicate icicle-buffer-predicate)
;;     (icicle-extra-candidates icicle-buffer-extras)
;;     (icicle-sort-function icicle-buffer-sort)))

;;
;;
;;  Note to Programmers
;;  -------------------
;;
;;  Here are some simple guidelines for using Icicles in Emacs-Lisp
;;  programming:
;;
;;  1. Function `icicle-next-candidate' is a general framework for
;;     letting users cycle completions of partial input strings.  I
;;     use it to define the cycling behavior for both prefix and
;;     apropos completions.  You can use it to easily define other,
;;     application-specific input matching/completion/cycling
;;     behavior.  Just supply it with a function that takes the
;;     current partial user input (a string) and returns a list of
;;     candidate completions, however those might be defined.
;;
;;  2. Use an input-completion read function, such as
;;     `completing-read' or `read-file-name', when you read input!
;;     There is almost never a reason not to use an input-completion
;;     function when reading user input - especially considering that
;;     you need not always provide a REQUIRE-MATCH argument.
;;
;;     Try also to find an appropriate PREDICATE argument, and a good
;;     set of default values to pass to `completing-read' as its TABLE
;;     argument.  Too often, I think, we use an overly general TABLE
;;     argument, such as the `obarray', and we don't provide a (good)
;;     PREDICATE.  Using an input-completion function with an
;;     appropriate candidate completion list and predicate can help
;;     users considerably.  I'm as guilty of TABLE and PREDICATE
;;     laziness as anyone, by the way.
;;
;;  3. Avoid using a literal-string `interactive' spec (e.g.
;;     (interactive "fFile: ")) that reads input with completion.
;;     Instead, call `completing-read' or `read-file-name' within the
;;     `interactive' spec.  This saves Icicles users of progressive
;;     completion the need to hit `RET' multiple times to pass their
;;     input up through multiple levels of recursive minibuffers to
;;     the top level. See "Progressive Completion", above.
;;
;;  4. In many cases, it makes sense to define a multi-command, rather
;;     than a simple command.  People can always use a multi-command
;;     as a simple command, but not vice versa.  See "Multi-Commands",
;;     "Defining Multi-Commands", and "Defining Icicles Commands",
;;     above.
;;
;;  5. If the potential number of completion candidates is enormous,
;;     then icompletion display in *Completions* can be slow.  In that
;;     case, consider turning it off for the duration of the command,
;;     by binding `icicle-incremental-completion-flag' to nil.  An
;;     alternative to turning it off is the approach taken in Icicles
;;     (e.g. `icicle-vardoc' and `icicle-insert-thesaurus-entry'):
;;     Just add a reminder to the doc string to tell users that they
;;     can toggle `icicle-incremental-completion-flag' with `C-#'.
;;
;;  6. You can bind `icicle-sort-function' temporarily to any sort
;;     function you need.
;;
;;  7. Another of my libraries that can help programmers provide
;;     default values is `thingatpt+.el'.  It provides functions for
;;     picking up symbols, sexps, numbers, words, and other sorts of
;;     thing near the text cursor (`point').
;;
;;  See also:
;;
;;     * "Defining Icicles Commands"
;;     * "Defining Multi-Commands"
;;     * "Global Filters"
;;     * "Multi-Completions"

;;
;;
;;  La Petite Histoire
;;  ------------------
;;
;;  1. This library started life as `elect-mbuf.el', by Hans Koomen.
;;
;;    Original posting:
;;    From koomen@cs.rochester.edu Mon Jun 19 19:27:58 1989
;;    To: info-gnu-emacs@prep.ai.mit.edu
;;    Cc: Hans <Koomen@cs.rochester.edu>
;;    Subject: elect-mbuf.el
;;    Date: Tue, 13 Jun 89 15:17:07 -0400
;;
;;  2. I hacked and enhanced the library in various relatively minor
;;  ways over the years, maintaining it as `elect-mbuf.el' - see
;;  details under "Change log", below.
;;
;;  I did not change the main functionality of the library during this
;;  period: it always cycled the COMPLETE list of (prefix) completion
;;  candidates passed to `completing-read'; it did not update the
;;  candidate list based on the current minibuffer contents.
;;
;;  So, for instance, if you had `M-x for' in the minibuffer, `C-n'
;;  would cycle among ALL Emacs commands, not just those that start
;;  with "for".  I used the library this way for fifteen years without
;;  thinking much about this behavior or the code behind it.
;;
;;  3. In July 2005, Lennart Borgman gave `elect-mbuf.el' a quick try,
;;  and intuitively expected to see behavior along the lines that you
;;  see now for prefix completion:
;;
;;  a. `C-n' should cycle completions relative to the current input,
;;     not all completions supplied to `completing-read'.
;;  b. If buffer *Completions* is displayed, `C-n' should highlight
;;     the current candidate there.
;;
;;  Good idea Lennart (<lennart.borgman.073@student.lu.se>).  So I
;;  implemented that behavior, and renamed the library "Icicles" (for,
;;  I suppose, "input cycles" or some such - or because it's "cool").
;;
;;  4. The code changes I made to implement #3 (completion cycling
;;  relative to current input) made me realize that other completion
;;  matchings could be implemented in a similar way.  Prefix
;;  completion (the completion provided by Emacs) is handy, but it is
;;  also sometimes a bit limited.  The idea of apropos completion
;;  occurred to me, and I implemented that as well.
;;
;;  5. I extended the library quite a bit more, in terms of
;;  convenience (highlighting, treatment of buffer *Completions*,...,
;;  but also in terms of functionality.  In particular, it now treats
;;  file names too.  And, because Emacs 21 and later versions use
;;  `read-file-name' for `find-file' and so on, Icicles now treats
;;  `read-file-name' the same as `completing-read'.
;;
;;  6. On another suggestion from LennartBorgman, I made Icicles take
;;  advantage of Delete Selection mode.  And I finally implemented it
;;  as a minor mode.
;;
;;  7, 8, 9,...  One thing has led to another, and I've just kept
;;  adding features.  Feature creep, I guess.  But the more I play
;;  with Icicles, the more I imagine new ways it might be made more
;;  useful.

;;
;;
;;  Note on Non-Nil `pop-up-frames' on MS Windows
;;  ---------------------------------------------
;;
;;  If you use `pop-up-frames' = t, like I do, you might have noticed
;;  that Emacs completion does not play well with using separate
;;  frames for each buffer.  In particular, it does not play well with
;;  having a separate frame for buffer *Completions*.  When you try to
;;  complete input using `TAB', a new frame is created for buffer
;;  *Completions*, and, at least on MS Windows, it is selected, taking
;;  the input focus away from the original frame's minibuffer!
;;
;;  This means that, once the *Completions* buffer has been displayed
;;  in a separate frame, you cannot, for instance, cycle completion
;;  candidates, without first reselecting the original frame manually.
;;  You cannot even use normal completion - you cannot add text in the
;;  minibuffer, or delete text there, because the minibuffer in the
;;  original frame no longer has the input focus.  Bummer.
;;
;;  In general, Emacs does not play too well with one-buffer-per-frame
;;  (`pop-up-frames' = t), and this is a good example of that general
;;  problem.
;;
;;  I reported this Emacs bug.  I'm hoping it will be corrected in
;;  Emacs 22.x.
;;
;;  I don't have this problem of loss of frame input focus in my own
;;  setup, even though I use `pop-up-frames' = t, because I use my
;;  library `oneonone.el'.  (Try it!)  If you need a solution while
;;  waiting for Emacs 22, you can try doing something similar to what
;;  I do in `oneonone.el':
;;
;;  1. Use dedicated frames for both *Completions* and the minibuffer.
;;
;;  2. Display buffer *Completions* using a special-display function
;;  that explicitly redirects the input focus from the *Completions*
;;  frame back to the minibuffer frame.

;;
;;
;;  Maybe To Do?
;;  ------------
;;
;;  1. Consider doing the key binding & unbinding differently - copy
;;     standard bindings when enter `icicle-mode' and restore them
;;     when leave it?
;;
;;  2. Replace redefinition of `exit-minibuffer' and/or
;;     `minibuffer-complete-and-exit' with icicles-only commands.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/05/26 dadams
;;     Mention M-k as icicle-erase-minibuffer-or-history-element.
;;     Don't mention M-S-backspace and M-S-delete any more.
;; 2006/05/19 dadams
;;     Renamed icicle-inhibit-reminder* to icicle-reminder*.
;;       Updated its doc to reflect new functionality.
;; 2006/05/18 dadams
;;     Change install instructions to include turning on Icicle mode.
;; 2006/05/16 dadams
;;     Require icicles-keys.el when icicle-bind-top-level-commands-flag.
;;     Updated doc to reflect new library icicles-keys.el.
;; 2006/05/15 dadams
;;     Renamed: ici*-nospace-flag to icicle-ignore-space-prefix-flag.
;;     Updated doc of icicle-ignore-space-prefix-flag.
;;     Added doc of icicle-buffer-ignore-space-prefix-flag.
;; 2006/04/14 dadams
;;     Added section Inserting a Regexp from a Variable.
;; 2006/04/09 dadams
;;     Added descriptions of icicle-arrows-respect-completion-type-flag.
;; 2006/03/19 dadams
;;     Added description of icicle-expand-input-to-common-match-flag.
;; 2006/03/07 dadams
;;     Correct the description of icicle-doc - match against only the
;;       doc, not the symbol name.
;; 2006/03/06 dadams
;;     Reordered Commentary sections, putting Emacs-Lisp stuff later.
;; 2006/03/05 dadams
;;     Mention icicle-touche-pas-aux-menus-flag.
;; 2006/03/03 dadams
;;     Clarified Multi-Completions description.
;; 2006/03/01 dadams
;;     Added: icicle-(complete|insert)-thesaurus-entry.
;;            Updated Commentary.
;; 2006/02/27 dadams
;;     Split into multiple libraries:
;;       *-cmd, *-face, *-fn, *-mac, *-mode, *-opt, *-var.
;; 2006/02/25 dadams
;;     Added: icicle-narrow-candidates (bound to M-*),
;;            icicle-icicle-completing-p, icicle-set-calling-cmd,
;;            icicle-reset-icicle-completing-p,
;;            icicle-run-icicle-(pre|post)-command-hook.
;;     Add all hooks in icicle-mode only, except for minibuffer-local
;;       hooks (pre- and post-command).
;;     Remove all hooks when exit Icicle mode.
;;     icicle-completing-read, icicle-read-file-name:
;;       Add catch icicle-read-top.  Set icicle-icicle-completing-p.
;;       Separate case of not Icicle mode from other no-prompt cases.
;;     Reordered some groups of functions.
;; 2006/02/24 dadams
;;     icicle-candidate-set-1: Treat empty set.
;; 2006/02/21 dadams
;;     icicle-prefix-complete:
;;       Implemented icompletion here, like icicle-apropos-complete-1.
;;     icicle-call-then-update-Completions:
;;       Use icicle-last-completion-command, not
;;           icicle-apropos-complete.
;;     Renamed icicle-apropos-icompleting-p to icicle-icompleting-p.
;;     Added: icicle-(kill|delete)(-backward)-*, icicle-yank etc.
;;            Bound them.
;;     Added: icicle-call-then-update-Completions.
;;     Added: icicle-incremental-completion-p.
;;       Use instead of icicle-incremental-completion-flag everywhere.
;;       Upgrade from t in icicle-display-candidates-in-Completions.
;;       Reset in icicle-minibuffer-setup.
;;     icicle-isearch-complete:
;;       Use search-ring symbol as history arg to completing-read.
;;     icicle-display-candidates-in-Completions,
;;     icicle-keep-only-past-inputs, icicle-history:
;;       Ensure that minibuffer-history-variable is a list.
;;     Fixed typos:
;;       icicle-keep-past-inputs -> icicle-keep-only-past-inputs.
;; 2006/02/20 dadams
;;     icicle-insert-string-at-point: Treat negative prefix arg.
;;     Added: icicle-signum.
;;     icicle-insert-thing:
;;       Remove text properties of string to insert.
;; 2006/02/19 dadams
;;     icicle-thing-at-point-functions:
;;       Added function to grab successive text.
;;     icicle-insert-string-at-point:
;;       Treat successive-grab fn and prefix arg.
;;     Added: icicle-default-thing-insertion,
;;            icicle-default-thing-insertion-flipped-p,
;;            icicle-insert-string-at-pt-(start|end),
;;            icicle-successive-grab-count, icicle-insert-thing.
;;     Renamed: icicle-insert-string-near-point to
;;              icicle-insert-string-at-point.
;; 2006/02/18 dadams
;;     icicle-retrieve-last-input:
;;       Don't reset icicle-last-completion-command if not interactive
;;     icicle-candidate-set-complement, icicle-keep-only-past-inputs:
;;       Use icicle-retrieve-last-input.
;;     icicle-keep-only-past-inputs:
;;       Rewrote modeled on icicle-apropos-complete:
;;        Take into account singleton and empty candidate set.
;;        Provide input to icicle-display-ca*.
;;        Set icicle-last-completion-command.
;;     icicle-history: Force redisplay of *Completions*.
;;                     Don't set this-command.
;;     icicle-completing-read: Ensure icicle-initial-value is not nil.
;;     icicle-save-or-restore-input: Don't restore empty input.
;;     icicle-recompute-candidates:
;;       Don't recompute if last completion cmd was
;;       icicle-keep-only-past-inputs.
;;     Added: icicle-historical-candidate,
;;            icicle-keep-only-past-inputs.
;;     icicle-display-candidates-in-Completions:
;;       Use icicle-historical-candidate.
;;     Bind icicle-keep-only-past-inputs to M-pause in minibuffer
;;       completion maps.
;; 2006/02/17 dadams
;;     Added: icicle-complete-input-overlay,
;;            icicle-highlight-complete-input, icicle-complete-input.
;;     icicle-(prefix|apropos)-complete(-1):
;;       Use icicle-highlight-complete-input.
;;     Added icicle-inhibit-reminder-prompt-flag.
;;           Thx to Jonathan Simms for the suggestion.
;;     icicle-completing-read, icicle-read-file-name:
;;       Use icicle-inhibit-reminder-prompt-flag.
;; 2006/02/12 dadams
;;     icicle-read-string: Finished bug fix of 2/11.
;;                         More thx to Andrey Zhdanov.
;; 2006/02/11 dadams
;;     icicle-insert-string-near-point:
;;       Always start with first function.
;;     read-from-minibuffer: Bug fix: don't use def if init is consp.
;;                           Thx to Andrey Zhdanov.
;; 2006/02/09 dadams
;;     Added: icicle-insert-string-near-point,
;;            icicle-thing-at-point-functions,
;;            icicle-thing-at-pt-fns-pointer.
;;            Bound icicle-insert-string-near-point.
;;     Added Commentary section "Inserting Text Found Near the Cursor"
;;     Require: thingatpt+.el, thingatpt.el.
;;     Bug fix: icicle-execute-extended-command(-1):
;;              Take care of last-command and this-command.
;; 2006/02/08 dadams
;;     icicle-completing-read: Treat consp case of initial-input.
;;     icicle-read-file-name: Fixed bug introduced 02/02:
;;       Don't ensure initial-input is not null.
;; 2006/02/07 dadams
;;     Bug fix: Files menu find-file stuff was bound to *recent-file*.
;; 2006/02/03 dadams
;;     icicle-init-value-flag: Use nil as the default value.
;;     Added: icicle-read-from-minibuffer, icicle-read-string.
;;              Use in icicle-(redefine|restore)-standard-commands.
;; 2006/02/02 dadams
;;     icicle-completing-read, read-file-name:
;;       Respect icicle-init-value-flag only if default value not nil.
;;     read-file-name: Ensure initial-value is not null.
;;                     Initialize icicle-initial-value.
;;                     Respect icicle-init-value-flag.
;; 2006/01/29 dadams
;;     icicle-completing-read, icicle-read-file-name:
;;       Remove binding of ESC-TAB.
;;     icicle-lisp-complete-symbol:
;;       Enable recursive minibuffers if in minibuffer.
;;     Commentary: Combine lisp-complete-symbol with dabbrev.
;;     Updated bindings listed in icicle-completion-help-string.
;; 2006/01/28 dadams
;;     New feature: icicle-lisp-complete-symbol (added).
;;                  Added to Commentary and moved section.
;;     Corrected fix of 2005/12/14:
;;       icicle-minibuffer-setup:
;;         Save region background at recursion level 1.
;;       icicle-saved-region-background: defvar to nil.
;;     Added: icicle-increment-color-hue.
;;            Use in icicle-region-background.
;;     Added: icicle-(re)set-option-to-(nil|t), icicle-clear-option,
;;            icicle-toggle-option, icicle-binary-option-p.
;; 2006/01/26 dadams
;;     Added: icicle(-saved)(-regexp)-search-ring-max,
;;            icicle-(redefine|restore)-standard-options.
;;     icicle-mode: Use icicle-(redefine|restore)-standard-options.
;;                  Use icicle-(redefine|restore)-standard-commands
;;                    for Emacs 21+ also (forgot?).
;;     icicle-(redefine|restore)-*: Use defalias, not fset.
;; 2006/01/24 dadams
;;     New feature: icicle-isearch-complete.
;;       Added: icicle-isearch-complete, icicle-isearch-resume,
;;              icicle-bind-isearch-keys.
;;       icicle-mode: add/remove isearch-mode-hook.
;;     Minor bug fix: initial value was treated as
;;                    icicle-last-completion-candidate.
;;       Added: icicle-initial-value.
;;       icicle-completing-read, icicle-read-file-name:
;;         Set icicle-initial-value,
;;             not icicle-last-completion-candidate.
;;       icicle-next-candidate:
;;         Initialize icicle-last-completion-candidate to
;;           icicle-initial-value.
;;       icicle-save-or-restore-input:
;;         Don't change icicle-current-input if = icicle-initial-value
;;       Renamed: icicle-init-value to icicle-init-value-flag.
;; 2006/01/23 dadams
;;     Use command remapping for self-insert-command in Emacs 22.
;;     Changed icicle-(re|un)map to defsubst.
;;     Removed Commentary section on icicle-execute-extended-command.
;;     icicle-apropos-complete-and-exit, icicle-apropos-complete-1:
;;       Use flag icicle-apropos-complete-and-exit-p to suppress
;;         minibuffer-message.
;; 2006/01/22 dadams
;;     Added: icicle-execute-extended-command*.
;;     completing-read, icicle-read-file-name:
;;       Corrected nil case for icicle-require-match-flag (bug fix).
;;       Hard-code bindings, instead of using \\[...], so the simpler
;;         bindings are shown.
;;     Changed C-o to C-RET for consistency (C-o still works too).
;;       icicle-(bind|restore)-completion-keys: Added C-RET binding.
;; 2006/01/21 dadams
;;     icicle-mouse-choose-completion:
;;       Don't save selected window if it's *Completions*.
;;     Added more Commentary about icicle-retrieve-last-input.
;; 2006/01/20 dadams
;;     icicle-sort-and-strip-ignored:
;;       Don't ignore names if only ignored extensions match.
;;     Added: icicle-apropos-complete-and-exit.
;;            Bound it in icicle-rebind-completion-maps.
;;     icicle-minibuffer-setup: Don't reset icicle-require-match-flag.
;;     icicle-apropos-complete: Return the list of candidates.
;; 2006/01/19 dadams
;;     Added: icicle(-buffer)-require-match-flag.
;;            Thanks to Mathias Dahl for feedback.
;;            Use in completing-read, read-file-name, and
;;              icicle-minibuffer-setup.
;;     Re-alphabetized defcustoms.
;; 2006/01/07 dadams
;;     Added :link.
;; 2005/12/31 dadams
;;     Added: icicle-fix-default-directory.
;;     icicle-read-file-name:
;;       Use icicle-fix-default-directory hack to fix bug.
;; 2005/12/26 dadams
;;     Added icicle-sort-case-insensitively.
;;     Added more parent groups for icicles group.
;; 2005/12/14 dadams
;;     icicle-minibuffer-setup:
;;       Only save region background when at top level.
;;     Added: icicle-Completions-frame-at-right-flag.
;;            Use in icicle-candidate-action.
;;     Added: defvars for font-lock-keyword-face,
;;            font-lock-function-name-face.
;; 2005/12/09 dadams
;;     Fontify icicle-define* in emacs-lisp-mode.
;; 2005/12/02 dadams
;;     Added: icicle-customize-apropos*.
;;            Use in icicle-(redefine|restore)-standard-commands.
;; 2005/12/01 dadams
;;     Added: icicle-repeat-complex-command,
;;            icicle-redefine-standard-commands-flag,
;;            icicle-(redefine|restore)-standard-commands.
;; 2005/11/30 dadams
;;     Added: icicle-apropos-zippy.
;;     icicle-apropos-command, icicle-apropos-variable:
;;       Corrected completing-read for do-all arg.
;;     icicle-apropos-command, icicle-apropos-option:
;;       My version must not respect apropos-do-all.
;; 2005/11/29 dadams
;;     Added: icicle-apropos*.
;;     icicle-help-on-candidate: Treat plists.
;;                               Message "No help" is the default.
;; 2005/11/25 dadams
;;     Added: icicle-dabbrev-completion.
;;     Renamed all names with "Completions" to use "Completions", for
;;       coherence with XEmacs port.
;; 2005/11/24 dadams
;;     icicle-mouse-choose-completion:
;;       Delete *Completions* window systematically.
;; 2005/11/21 dadams
;;     icicle-delete-windows-on:
;;       Avoid error Attempt to delete minibuffer or sole ... window.
;;     icicle-prefix-complete, icicle-apropos-complete-1,
;;     icicle-next-candidate:
;;       Use icicle-delete-windows-on, not delete-window.
;;     icicle-candidate-set-save: Use map in doc string.
;;     icicle-compilation-search: Tidied up doc string.
;;     Use #' for clarity.
;; 2005/11/20 dadams
;;     icicle-completing-read:
;;       Added treatment of completions that are lists of strings.
;;     Updated Commentary: new section on completions that are lists.
;;     Added: icicle-list-join-string, icicle-doc, icicle-fundoc,
;;            icicle-vardoc.
;; 2005/11/15 dadams
;;     Temporarily removed defadvice of next-history-element for
;;       Emacs 22.  Bug reported.
;;     icicle-minibuffer-prompt-end: Changed from defsubst to defun.
;; 2005/11/13 dadams
;;     icicle-mouse-candidate-action:
;;       buffer-substring -> buffer-substring-no-properties.
;;     icicle-completing-read:
;;       Bind, don't set, minibuffer-completion-table.
;;     icicle-buffer*: Use other buffer for DEF, not INIT-VALUE.
;;     Added: icicle-preselect-init-value-flag,
;;            icicle-(add|remove)-buffer-*,
;;            icicle-read-from-minibuf-nil-default,
;;            icicle-buffer-list, icicle-select-minibuffer-contents,
;;            icicle-completing-p.
;;     icicle-minibuffer-setup:
;;       Select minibuf contents if icicle-preselect-init-value-flag.
;;       Only display *Completions* if icicle-completing-p.
;;     Advised next-history-element.
;; 2005/11/11 dadams
;;     Added: icicle-show-*Completions*-initially-flag,
;;            icicle-display-*Completions*.
;;     icicle-minibuffer-setup:
;;       If icicle-show-*Completions*-initially-flag, display it.
;; 2005/11/09 dadams
;;     Added: icicle-mouse-candidate-action.
;;            Bind in icicle-rebind-completion-maps.
;;     icicle-buffer(-other-window):
;;       Use buffer-name-history as HIST arg to completing-read.
;; 2005/11/08 dadams
;;     Add/remove hook icicle-cancel-*Help*-redirection in
;;       icicle-mode, not at top level.
;;     Removed icicle-reset-icicle-menu-items-alist.
;;     Reset icicle-menu-items-alist in icicle-execute-menu-command
;;       of icicles-menu.el.
;; 2005/11/06 dadams
;;     Include minibuffer-local-filename-completion-map.
;; 2005/11/05 dadams
;;     icicle-display-candidates-in-*Completions*:
;;       Don't try to highlight root if it is "".
;;     icicle-help-on-candidate:
;;       Test null, not boundp icicle-menu-items-alist.
;;       If menu item's command is a lambda, set cand-symb to nil.
;;     icicle-mode: Use icicle-reset-icicle-menu-items-alist on
;;                  minibuffer-exit-hook.
;;     Added: icicle-reset-icicle-menu-items-alist.
;;     Added defvar for icicle-menu-items-alist.
;;     Added byte-compiler comments and defvars to quiet byte-compile.
;; 2005/11/04 dadams
;;     icicle-display-candidates-in-*Completions:
;;       Bug fix - use (functionp minibuffer-completion-table), not
;;                 (icicle-file-name-input-p).
;; 2005/11/03 dadams
;;     Added: icicle-filter-wo-input and vars icicle-must-*,
;;            icicle-extra*, icicle-buffer-*, icicle-buffer-config*,
;;            icicle-buffer-sort*.
;;     icicle-unsorted-*:
;;       Use icicle-filter-wo-input and icicle-extra-candidates.
;;     Added Commentary section Global Filters.
;;     icicle-buffer* commands: Added filter bindings.
;;     icicle-define(-file)-command: Minor bug fix:
;;       Ensure buffer is live before switching back.
;; 2005/11/01 dadams
;;     Added: icicle-must(-not)-match-regexp.
;;            Use in icicle-unsorted-*-candidates.
;; 2005/10/31 dadams
;;     Added: icicle-use-default-as-init-value-flag.
;;            Use in completing-read.
;;     icicle-find-file*: Minor bug fix - REQUIRE-MATCH should be nil.
;; 2005/10/29 dadams
;;     icicle-display-candidates-in-*Completions:
;;       Minor bug fix - wrap in save-window-excursion.
;;     icicle-minibuffer-contents-from-minibuffer:
;;       Minor bug fix - do nothing if file & user erased minibuffer.
;;     Menu-bar menus:
;;       Enable Icicles menu items only in Icicle mode.  Put search
;;       stuff on Search menu, if available.   Use "[Icy]" prefix for
;;       Icicles items in menus other than "Icicles".
;; 2005/10/28 dadams
;;     Added: icicle-define-file-command.
;;            Use it to define icicle-delete-file, icicle-find-file*.
;;     icicle-(next|previous)-(apropos|prefix)-candidate-action:
;;       Do action before moving to next|prev.
;;     icicle-candidate-action:
;;       Raise *Completions* frame, to keep it on top.
;; 2005/10/27 dadams
;;     Added: icicle-define-command, icicle-find-file*,
;;            select-frame-set-input-focus.
;;     Redefined using icicle-define-command:
;;       icicle-bookmark, icicle-buffer*, icicle-color-theme,
;;       icicle-delete-file, icicle-find-file*, icicle-font,
;;       icicle-frame-*, icicle-recent-file*.
;;     icicle-all-candidates-action:
;;       Report failures, not successes.  Use error msg.
;;     Added Commentary sections: Special-Character Conflicts,
;;                                Defining Icicles Commands.
;;     Commentary section Act on All Candidates:
;;       Added delete-one-or-more-files example.
;;     Added icicle-find-file* to menu-bar menus.
;;     Inactivated top-level menu items when minibuffer is active.
;;     Renamed:
;;       icicle-delete-file-1 to icicle-delete-file-or-directory.
;; 2005/10/25 dadams
;;     Thx to Lennart Borgman for suggestion about
;;       select-frame-set-input-focus.
;; 2005/10/24 dadams
;;     icicle-search:
;;       1) Bug fix - need to have mouse-choose-completion set
;;          icicle-candidate-nb.
;;       2) Show error message.
;;     Default value of icicle-candidate-nb is now nil, not -1.
;;     Added: icicle-mouse-choose-completion,
;;            icicle-nb-of-candidate-in-*Completions*.
;;     icicle-move-to-(next|previous)-completion,
;;     icicle-increment-cand-nb+signal-end:
;;       Reset candidate number to 0 if nil.
;;     icicle-(redefine|restore)-std-completion-fns:
;;       Use icicle-mouse-choose-completion.
;; 2005/10/23 dadams
;;     Added: icicle-mode-map.
;;     icicle-(bind|restore)-completion-keys: Updated menu-bar menu.
;;     icicle-compilation-search:
;;       Error if not in a compilation buffer.
;; 2005/10/21 dadams
;;     icicle-remove-duplicates: redefined.
;; 2005/10/18 dadams
;;     icicle-file-name-input-p doc string:
;;       Mention why don't use minibuffer-completing-file-name.
;; 2005/10/16 dadams
;;     Added: icicle-compilation-search, icicle-search-hook.
;;     icicle-search: Run icicle-search-hook.
;;                    Added optional sit-for-period arg.
;;     icicle-mode: Added list of top-level commands to doc string.
;;     icicle-scroll-or-update-*Completions*:
;;       Added msg arg - only display msg if don't scroll.
;; 2005/10/14 dadams
;;     Allow for multisets of candidates.
;;     Added: icicle-search, icicle-completion-nospace-flag,
;;            icicle-candidate-nb, icicle-filter-alist,
;;            icicle-increment-cand-nb+signal-end.
;;     Commentary: Updated for icicle-search.
;;     icicle-next-candidate: Major rewrite.
;;       Uses icicle-candidate-nb,
;;         icicle-increment-cand-nb+signal-end,
;;         icicle-move-to-next-completion.
;;     Use icicle-completion-nospace-flag in calls to all-completions.
;;     icicle-previous-(apropos|prefix)-candidate,
;;     icicle-(next|previous)-(apropos|prefix)-candidate-action:
;;       Added optional arg.
;;     icicle-apropos-complete-1, icicle-next-candidate,
;;     icicle-recompute-candidates:
;;       Added *-action commands to memq test.
;;     icicle-move-to-next-completion:
;;       Added optional no-minibuffer-follow-p arg.
;;     icicle-scroll-or-update-*Completions*:
;;       Update display even if handle-switch-frame.
;; 2005/10/12 dadams
;;     Added: icicle-redefine-std-completion-fns,
;;            icicle-restore-std-completion-fns,
;;            icicle-delete-windows-on, icicle-frames-on.
;;     icicle-mode: Use icicle-redefine-std-completion-fns,
;;                  icicle-restore-std-completion-fns.
;;     Renamed to use icicle- prefix: choose-completion-string,
;;       completing-read, completion-setup-function, exit-minibuffer,
;;       minibuffer-complete-and-exit, read-file-name,
;;       switch-to-completions.  Added these and also old- versions.
;;     icicle-history: Treat file names also.
;;     remove-windows-on -> icicle-delete-windows-on.
;; 2005/10/11 dadams
;;     Added: icicle-history, icicle-scroll-or-update-*Completions*,
;;            icicle-undo-std-completion-faces.
;;     Minor bug fixes:
;;       icicle-remove-dots: Also match against "." and ".."
;;         (lack of slash in Emacs 21+).
;;       icicle-save-or-*: Don't reset to last input if
;;                         icicle-last-completion-candidate is "".
;;                         Update icicle-last-completion-candidate
;;                         also to use current input.
;;       Reset icicle-last-input in icicle-minibuffer-setup, not in
;;         completing-read and read-file-name.
;;       icicle-display-candidates-in-*Completions*,
;;       icicle-next-candidate:
;;         Put candidate in consp before applying predicate.
;;       icicle-recompute-candidates:
;;         Don't recompute unless icicle-last-completion-command.
;;       icicle-retrieve-last-input:
;;         Use icicle-current-input, not icicle-last-input.
;;       icicle-self-insert:
;;         Update icicle-current-input and set this-command to
;;         icicle-apropos-complete.
;;       icicle-apropos-complete: Use error-message-string.
;;       icicle-apropos-complete-1:
;;         Protect icicle-file-directory-p with
;;         icicle-file-name-input-p.  Unconditionally update
;;         icicle-last-completion-command.
;;     Moved undoing of std completion faces to icicle-mode.
;;     Use icicle-scroll-or-update-*Completions* in
;;         icicle-candidate-set-1.
;; 2005/10/06 dadams
;;     icicle-prefix-complete, icicle-apropos-complete-1:
;;       Removed vestigial slash cruft - should have been removed in
;;         2005/09/01 fix.
;;     Added: icicle-remove-dots.
;;            Use in icicle-save-or-restore-input.
;; 2005/10/05 dadams
;;     icicle-msg-maybe-in-minibuffer: use format-string arg.
;; 2005/10/04 dadams
;;     Replace use of minibuffer-completion-help by
;;       icicle-apropos-complete.
;;     Added: icicle-recent-file*, icicle-toggle-ignored-extensions,
;;            icicle-update-completions,
;;            icicle-msg-maybe-in-minibuffer,
;;            icicle-saved-ignored-extensions.
;;     Bound icicle-toggle-*.
;;     icicle-toggle-sorting:
;;       Use icicle-update-completions, icicle-msg-maybe-in-minibuffer
;;     icicle-sort-and-strip-ignored:
;;       icicle-ignored-extensions-regexp nil => nothing is ignored.
;;     Reorder key bindings, so prompt shows S-tab, not S-iso-lefttab.
;;     icicle-next-candidate: Fixed code to highlight candidate in
;;       *Completions*: restriction.
;; 2005/10/03 dadams
;;     Regexps can now use backslash (it is no longer a directory
;;       separator on MS Windows).
;;       icicle-minibuffer-contents-from-minibuffer,
;;       icicle-file-name-directory-w-default:
;;         Escape backslash, so not used as directory separator on
;;         MS Windows.
;;       Added: icicle-apropos-complete-1,
;;              icicle-file-name-nondirectory.
;;       icicle-apropos-complete: Use icicle-apropos-complete-1.
;;                                Treat regexp error via message.
;;       Use icicle-file-name-nondirectory everywhere, instead of
;;         file-name-nondirectory.
;;     Can now use "?" for regexps; it no longer shows completion list
;;     Do icicle-update-ignored-extensions-regexp inside
;;       icicle-minibuffer-setup.
;;     Added and bound: icicle-retrieve-last-input.
;;     Updated icicle-completion-help-string with recent bindings.
;;     Renamed: icicle-last-command to icicle-last-completion-command.
;;              icicle-candidate-set-restore to
;;              icicle-candidate-set-retrieve.
;; 2005/10/01 dadams
;;     Added: icicle-candidate-set-(define|restore|swap).
;;     Changed binding of icicle-candidate-set-save to C->.
;;     Bound new commands.
;; 2005/10/01 dadams
;;     Major rewrite to add set operations: complement, difference,
;;                                          union, intersection.
;;       Added: icicle-completion-candidates, icicle-current-input,
;;              icicle-candidate-set-*, icicle-set-*,
;;              icicle-save-or-restore-input,
;;              icicle-recompute-candidates.
;;       Bound icicle-candidate-set*.
;;       Added Commentary for Sets of Completion Candidates.
;;       icicle-(apropos|prefix)-complete:
;;         Update icicle-completion-candidates, only as needed.
;;       icicle-next-candidate:
;;         Reverse candidates only if switched to opposite-direction
;;           command of same type.
;;         Likewise, for refresh of *Completions*.
;;         Protect put-text-property for root (e.g. no match for
;;           complement).
;;       icicle-(apropos|prefix)-complete,
;;       icicle-prefix-word-complete, icicle-next-candidate:
;;         use icicle-completion-candidates.
;;       icicle-all-candidates-action:
;;         Use icicle-completion-candidates, not
;;         icicle-apropos-complete.
;;       icicle-display-candidates-in-*Completions*:
;;         Removed first arg (candidates).
;;         Update icicle-completion-candidates.
;;    icicle-all-candidates-action:
;;      Use icicle-completion-candidates, so act on completions of
;;      either kind.
;; 2005/09/30 dadams
;;     Commented out resetting of minibuffer-completion-table to nil
;;     for icompletion.
;;     Thx to Andrey for bug report on M-x M-r problem.
;; 2005/09/27 dadams
;;     icicle-(bind|restore)-completion-keys:
;;       Bound [S-iso-lefttab] like [S-tab].
;; 2005/09/26 dadams
;;     Bug fix: Changed "\C-!"  to [(control ?!)] (others similarly).
;;     Bound [S-iso-lefttab] like [S-tab].
;; 2005/09/16 dadams
;;     Added: icicle-all-candidates-action, icicle-delete-file*,
;;     icicle-rebind-completion-maps:
;;       Bound icicle-all-candidates-action to C-!.
;;     icicle-(apropos|prefix)-complete: Return candidates list.
;;     icicle-bookmark, icicle-buffer*, icicle-color-theme,
;;     icicle-font, icicle-frame*:
;;       Return t for success, nil for failure.
;;     Commentary: Added section Choose All Completion Candidates.
;; 2005/09/14 dadams
;;     icicle-rebind-completion-maps:
;;       Bound TAB and S-TAB for navigation.
;;     icicle-move-to-(next|previous)-completion,
;;     icicle-(next|previous)-line: Wrap around.
;; 2005/09/13 dadams
;;     Major rewrite of file treatment, to treat directory candidates
;;       similarly to files.
;;     Added: icicle-default-directory, icicle-file-directory-p,
;;            icicle-sort-function, icicle-toggle-sorting,
;;            toggle-icicle-sorting.
;;     Use icicle-file-directory-p everywhere, except
;;       choose-completion-string.
;;     Removed: icicle-nondirectory-*.
;;     icicle-next-candidate:
;;       If not icicle-cycle-into-subdirs-flag, then use relative
;;       file/dir name, not nondirectory part.
;;     icicle-(apropos|prefix)-complete:
;;       Set icicle-default-directory if sole completion is a
;;       subdirectory.
;;     icicle-sort-and-strip-ignored:
;;       Removed optional arg and treatment of subdirs.
;;     icicle-next-(apropos|prefix)-candidate,
;;     icicle-(apropos|prefix)-complete:
;;       Don't treat icicle-cycle-into-subdirs-flag here.
;;     icicle-(apropos|prefix)-complete, icicle-next-candidate:
;;       Set icicle-default-directory, if directory candidate.
;;     icicle-minibuffer-setup: Set icicle-default-directory.
;;     icicle-apropos-complete:
;;       Different message if icicle-apropos-icompleting-p.
;;     icicle-sort-dirs-last:
;;       Treat other kinds of candidates, besides files and dirs.
;;     Commentary and doc strings: Updated for icicle-sort-function,
;;                                 icicle-cycle-into-subdirs.
;;     Let delete-selection mode work with icicle-self-insert.
;;     icicle-display-candidates-in-*Completions*:
;;       Make *Completions* read-only.
;; 2005/09/09 dadams
;;     choose-completion-string:
;;       bug fix for Emacs 21.3.1 - use Emacs 20 version for 21.3.1.
;; 2005/09/08 dadams
;;     completion-setup-function:
;;       bug fix for Emacs 21.3.1 - use Emacs 20 version for 21.3.1.
;;     Added: icicle-remap, icicle-unmap,
;;            icicle-(bind|restore)-completion-keys.
;;     completing-read: Do not append suffix if not in Icicle mode.
;;     icicle-rebind-completion-maps:
;;       Clean-up.  Use icicle-(bind|restore)-completion-keys.
;;       Don't (suppress-keymap completion-list-mode-map).
;; 2005/09/06 dadams
;;     Provided apropos icompletion.
;;     Added: icicle-self-insert, icicle-incremental-completion-flag,
;;            icicle-apropos-icompleting-p.
;;     icicle-apropos-complete: Respect icicle-apropos-icompleting-p.
;;     Commentary: Updated Icompletion and Customization sections.
;;                 Added Apropos Icompletion.
;;     Changed default value of icicle-word-completion-key to M-SPC.
;;     icicle-rebind-completion-maps:
;;       Bind icicle-self-insert. Use self-insert for SPC.
;;       Updated icicle-completion-help-string.
;;       Treat menu-bar menu for the minibuffer.
;;     completion-setup-function:
;;       Only add instruction2 when icicle-mode.
;;     icicle-display-candidates-in-*Completions*:
;;       Use save-restriction.
;;     icicle-minibuffer-contents-from-minibuffer:
;;       Allow for mixing $ of environment vars with $ of regexps.
;; 2005/09/02 dadams
;;     Added: icicle-bookmark, icicle-buffer(-other-window),
;;            icicle-candidate-action, icicle-candidate-action-fn,
;;            icicle-color-theme(s), icicle-font, icicle-frame-(b|f)g.
;;     Renamed: icicle-(next|previous)-(apropos|prefix)-*-help to
;;              icicle-(next|previous)-(apropos|prefix)-*-action.
;;     icicle-(apropos|prefix)-complete:
;;       Set icicle-last-completion-candidate.
;;     In renamed functions:
;;       Use icicle-candidate-action, not icicle-help-on-candidate.
;;     icicle-rebind-completion-maps:
;;       Bound C-o to icicle-candidate-action.
;;     Added Commentary section on actions on candidates.
;;     icicle-move-to-next-completion:
;;       Test line num, not char position (fix).
;;     icicle-previous-line: 3 or 4, not 4 or 5 (fix).
;; 2005/09/01 dadams
;;     Fixed major bug: file-name completion did not work at all in
;;       non-MS Windows!
;;       icicle-file-name-(apropos|prefix)-candidates:
;;         Removed code for case where input starts with "/".
;;       icicle-nondirectory-file-name-(apropos|prefix)-candidates:
;;         Removed code for case where input starts with "/".
;;         Bind default-directory.
;;       icicle-(apropos|prefix)-complete:
;;         Treat case when icicle-cycle-into-subdirs-flag = nil.
;;     icicle-next-candidate:
;;       Took out code that moved point when line is too long.
;;     icicle-minibuffer-setup: Reset icicle-prompt.
;; 2005/08/23 dadams
;;     Added: icicle-help-on-candidate,
;;            icicle-cancel-*Help*-redirection,
;;            icicle-(previous|next)-(prefix|apropos)-candidate-help.
;;            Bound them all.
;;     icicle-rebind-completion-maps:
;;       Bound icicle-help-on-candidate,
;;       icicle-(previous|next)-(prefix|apropos)-candidate-help.
;; 2005/08/22 dadams
;;     Unconditionally require cl.el when compile (because of case).
;; 2005/08/19 dadams
;;     Renamed icicle-cursor-position-in-candidate to
;;             icicle-point-position-in-candidate.
;;     Added: icicle-mark-position-in-candidate,
;;            icicle-minibuffer-prompt-end.
;;     icicle-place-cursor: Position both point and mark.
;;     icicle-point-position-in-candidate:
;;       Change values from bob, eob to input-start/end.
;;     Removed: icicle-select-rest-of-completion-flag.
;;              Use inequality test on point and mark.
;;     Updated commentary.
;; 2005/08/16 dadams
;;     Minbuffer messages:
;;       Differentiate prefix from apropos completion.
;;     completing-read, read-file-name:
;;       Append icicle-prompt-suffix for Emacs 20 (oversight).
;; 2005/08/15 dadams
;;     Bug fix: Only use face-spec-reset-face if target faces defined.
;;     read-file-name: bug fix:
;;       Use condition-case to get the correct number of args for
;;       old-read-file-name. Thx to Mathias Dahl for both bug reports.
;; 2005/08/14 dadams
;;     icicle-place-cursor: Narrow region to exclude minibuffer-prompt
;; 2005/08/13 dadams
;;     Add regexp support (removed it when introduced highlighting).
;;       icicle-next-candidate:
;;         Added regexp-p arg.  Use in icicle-next-apropos-candidate.
;;       icicle-place-cursor:
;;         Use regexp search.  For root-start, go to match-beginning.
;;       icicle-unsorted-file-name-apropos-candidates:
;;         Don't use regexp-quote.
;;     icicle-switch-to-*Completions*:
;;       Search in restriction of mouse-face zone; repeat.
;;       Treat file case (use nondirectory part).
;;       Bind case-fold-search.
;;     Protect (aref <input> 0) against empty string.
;;     member -> memq, for symbols.
;; 2005/08/12 dadams
;;     Added: icicle-word-completion-key, icy-mode,
;;            icicle-insert-a-space.
;;     icicle-rebind-completion-maps:
;;       Use icicle-word-completion-key and icicle-insert-a-space.
;;     completing-read, icicle-rebind-completion-maps:
;;       Corrected bindings in doc string.
;; 2005/07/29 dadams
;;     Added: icicle-change-region-background-flag,
;;            icicle-increment-color-value, icicle-region-background,
;;            icicle-saved-region-background,
;;            icicle-restore-region-face.
;;     Added icicle-restore-region-face to minibuffer-exit-hook.
;;     Require hexrgb.el.
;;     Removed: icicle-select-rest-of-completion.
;;     icicle-minibuffer-setup: Save icicle-saved-region-background
;;                              and use icicle-region-background.
;; 2005/07/28 dadams
;;     Added: icicle-*Completions*-instruction-*.
;;     completion-setup-function:
;;       Use icicle-*Completions*-instruction-*.
;;       Remove ? from instruction2.  Put both instr on same line.
;;       Use put-text-property, not *-w-face*.
;;     ------
;;     Move all completion stuff here, from simple+.el:
;;       choose-completion-string, completion-setup-function,
;;       switch-to-completions.
;;     Wrap *Completions* window deletion in save-selected-window.
;;     Added icicle-prefix-word-complete, and bound it to SPC.
;;     completion-setup-function: Renamed
;;       icicle-completing-read-prompt-suffix to icicle-prompt-suffix.
;; 2005/07/27 dadams
;;     Renamed: icicle-completing-read-prompt* to icicle-prompt*.
;;     Added: read-file-name, face
;;            icicle-completing-read-prompt-suffix,
;;            icicle-remove-property,
;;            icicle-select-rest-of-completion (simple, for now).
;;     completing-read: Apply faces to prompt.
;;     icicle-place-cursor: Use icicle-select-rest-of-completion.
;;     Added (if icicle-mode (icicle-mode 1)) at end.
;;     Reworded Commentary in terms of "input completion", not just
;;       completing-read.
;; 2005/07/26 dadams
;;     rebind-minibuffer-completion-maps: Minor bug fix.
;;     icicle-mode: Added " Icy" to mode line.
;;     Wrapped Emacs 21 version of icicle-mode (with
;;       define-minor-mode) in (eval (quote...)), so byte-compiling
;;       in Emacs 20 will produce a *.elc that works in Emacs 21.
;; 2005/07/25 dadams
;;     Added: icicle-mode, icicle-*-hook, icicle-minibuffer-setup,
;;            icicle-activate-mark.
;;     rebind-minibuffer-completion-maps:
;;       Restore bindings when exit Icicle mode.
;;       Added argument.  Pick up everything bound to help-command.
;;                        Updated doc string.
;;       Message only when mode is turned on.
;; 2005/07/24 dadams
;;     Now leave region from end of root to end of completion, so you
;;       can easily replace it, especially if you use
;;       delete-selection mode.  (Suggestion by Lennart Borgman.)
;;     Added: icicle-select-rest-of-completion-flag.
;;     icicle-place-cursor:
;;       Create active region if icicle-select-rest-of-completion-flag
;;     icicle-completion-help: Removed icicle-abort-minibuffer-input.
;;     icicle-abort-minibuffer-input:
;;       Removed obsolete code & comment on icomplete-inhibit.
;; 2005/07/22 dadams
;;     Major fixup: Treat file and directory names well, respect std
;;                  user options, more.
;;     Renamed:
;;       icicle-(next|previous)?-completion-candidate to
;;         icicle-*-prefix-candidate(s),
;;       icicle*filename* to icicle*file-name*,
;;       icicle-descend-into-subdirs to
;;         icicle-cycle-into-subdirs-flag.
;;     Added: icicle-file-name-apropos-candidates,
;;            icicle-file-name-directory-w-default,
;;            icicle-file-name-input-p,
;;            icicle-file-name-prefix-candidates,
;;            icicle-nondirectory-file-name-apropos-candidates,
;;            icicle-nondirectory-file-name-prefix-candidates,
;;            icicle-sort-dirs-last,
;;            icicle-unsorted-apropos-candidates,
;;            icicle-unsorted-file-name-apropos-candidates,
;;            icicle-unsorted-file-name-prefix-candidates,
;;            icicle-unsorted-prefix-candidates, icicle-last-command.
;;     Respect insert-default-directory and completion-auto-help.
;;     Use minibuffer-message instead of message.
;;     Commentary: Added Customization & Tips section.
;;     completing-read: Updated doc string.  Save icicle-last-input.
;;                      Reset icicle-nb-of-other-cycle-candidates.
;;     icicle-next-*-candidate: Branch to file-specific functions.
;;     icicle-*-candidates: Use icicle-unsorted-*-candidates.
;;     icicle-next-candidate:
;;       Delete *Completions* window if no candidates.
;;       Use icicle-file-name-directory, not file-name-directory.
;;     icicle-minibuffer-contents-from-minibuffer:
;;       Use substitute-in-file-name.
;;     icicle-*-complete:
;;       Treat slashed file names (e.g. "/foo").
;;       Use icicle-file-name-*-candidates,
;;         icicle-file-name-directory-w-default for files.
;;       Added messages [No completion], [Sole completion],
;;         [Complete, but not unique].
;;       Use icicle-last-command for repetition test. And set it.
;;     icicle-rebind-completion-maps:
;;       Updated icicle-completion-help-string and message.
;; 2005/07/21 dadams
;;     icicle-apropos-candidates:
;;       Use, not apropos, but delete-if-not on string-match.
;;       Treat files too.
;;     Removed icicle-intersection.
;;     Added: icicle-descend-into-subdirs.
;;     icicle-sort-and-strip-ignored: Use icicle-descend-into-subdirs.
;;                                    Don't use "." and "..".
;;     icicle-next-candidate:
;;       File names w/o dir.
;;       Use regexp-quote on root for underlining file-name root.
;;       Insert directory name for file.
;;     icicle-place-cursor:
;;       Search past dir, then search for file-name w/o dir.
;;     icicle-prefix-complete, icicle-apropos-complete,
;;     icicle-switch-to-*Completions*:
;;       Use icicle-minibuffer-contents-from-minibuffer.
;;     icicle-prefix-complete, icicle-apropos-complete:
;;       Insert dir when single candidate.
;;     icicle-display-candidates-in-*Completions*:
;;       Underline file-name w/o dir.
;; 2005/07/20 dadams
;;     icicle-next-candidate,
;;     icicle-display-candidates-in-*Completions*:
;;       Use set-buffer-modified-p.
;;     icicle-next-candidate: Use ding when hit end of cycle.
;;     Added: icicle-cursor-position-in-candidate,
;;            icicle-place-cursor.
;;            Use in icicle-next-candidate to position cursor.
;;     Added: defgroup icicles.
;; 2005/07/19 dadams
;;     Initialize icicle-ignored-*.
;;     Added: icicle-nb-of-other-cycle-candidates,
;;            icicle-minibuffer-contents-from-minibuffer.
;;     completing-read: Reset icicle-last-completion-candidate to nil.
;;     icicle-next-candidate:
;;       Use icicle-minibuffer-contents-from-minibuffer.
;;       Save icicle-nb-of-other-cycle-candidates for
;;         icomplete-completions (icomplete+).
;;       Use copy of "next" string since change its text properties.
;;       Use regexp-quote for underlined root.
;;       Use put-text-property, so works in Emacs 20.
;;       Update *Completions*, even if last command is repeated.
;;     icicle-*-complete: Complete rewrite.
;;     icicle-display-candidates-in-*Completions*:
;;       Do even if last command is repeated.
;; 2005/07/18 dadams
;;     icicle-display-*:
;;       Highlight only first occurrence in each candidate.
;;     icicle-next-candidate: Use completion-ignore-case.
;; 2005/07/17 dadams
;;     Treat file names also.
;;     Added: icicle-delete-if*, and use instead of delete-if-*.
;;            Removed require cl.el.
;;     Added: icicle-ignored-extensions*,
;;            icicle-sort-and-strip-ignored,
;;            icicle-filename-input-p,
;;            icicle-update-ignored-extensions-regexp,
;;            icicle-prefix-complete.  Bound icicle-prefix-complete.
;;     Use icicle-update-ignored-extensions-regexp as
;;       minibuffer-setup-hook.
;;     icicle-*-candidates: Use icicle-sort-and-strip-ignored.
;;     icicle-next-candidate,
;;     icicle-display-candidates-in-*Completions*:
;;       Don't use predicate on file-name candidates
;;       (icicle-filename-input-p).
;;     icicle-next-candidate:
;;       Use read-file-name-completion-ignore-case (Emacs 22) and
;;       file-name-nondirectory.
;;     icicle-apropos-complete:
;;       Return t/nil. Treat single candidate as no-op.
;;     Reset std completions-* faces, so they don't interfere with
;;       apropos highlighting.
;; 2005/07/16 dadams
;;     Added: icicle-display-*, icicle-apropos-complete.
;;     Use icicle-display-* in icicle-next-candidate and
;;       icicle-apropos-complete.
;;     Bound icicle-apropos-complete to S-tab in completion maps.
;;     icicle-switch-to-*Completions*:
;;       Move to start of candidate.  Highlight candidate, not regexp.
;;     icicle-next-candidate: Underline the root that was completed.
;;     Added: faces icicle-root-highlight-*.
;;     Removed: faces: icicle-completion-help*.
;;     Removed (not used): require of strings.el.
;;     Commentary: Added Nutshell View.
;; 2005/07/15 dadams
;;     Renamed: icicle-completion-help+ to icicle-completion-help.
;;     Replaced: icicle-delete-lines by icicle-erase-minibuffer.
;;     icicle-next-candidate:
;;       Wrapped display-* and re-search-forward in condition-case.
;;       Use icicle-place-overlay.
;;     Changed icicle-completion-help bindings to [f1].
;;     Added: icicle-*-line, icicle-switch-to-*,
;;            icicle-move-to-*-completion,
;;            icicle-current-completion-in-*Completions*,
;;            icicle-place-overlay.
;;     Added bindings for icicle-*-line, icicle-switch-to-*,
;;                        icicle-move-to-*.
;;     Bound q to icicle-abort-minibuffer-input in
;;       completion-list-mode-map.
;;     icicle-completing-read-prompt-suffix: Mention both [f1] and ?.
;;     Removed: icicle-fit-frame.
;;     Commentary: Added How...Improves...(5).  Updated Key Bindings.
;; 2005/07/14 dadams
;;     icicle-next-candidate:
;;       Update *Completions*, if displayed, to reflect current
;;       candidates, but don't do it if this-command = last-command.
;;       Reverse list as needed, to keep same order.   Ensure current
;;       *Completions* choice shows in window (recenter as needed).
;;       For highlighting: Search with re-search-forward to be sure
;;                         to get the right one.
;;       Took test for presence of predicate out of loop.
;;     Commentary: Added Note on pop-up-frames = t.
;; 2005/07/13 dadams
;;     Rewrote icicle-apropos-candidates.
;;     Added: icicle-intersection.
;; 2005/07/12 dadams
;;     Added: icicle-(next|previous)-apropos-candidate,
;;            icicle-next-candidate, icicle-apropos-candidates,
;;            icicle-completion-candidates.
;;     Bound: icicle-(next|previous)-apropos-candidate.
;;     Renamed: icicle-completion-help-(title-)face: Removed "-face".
;;     icicle-next-completion-candidate:
;;       Redefined to use icicle-next-candidate.
;;     icicle-rebind-completion-maps:
;;       Updated text to mention apropos completion.
;;     icicle-completion-help+:
;;       Use icicle-abort-minibuffer-input, not abort-recursive-edit.
;; 2005/07/10 dadams
;;     First version of icicles.el (previously called elect-mbuf.el).
;;     Renamed: minibuffer-completion-help-string to
;;              icicle-completion-help-string,
;;       completing-read-prompt to icicle-completing-read-prompt,
;;       completing-read-prompt-suffix to
;;        icicle-completing-read-prompt-suffix,
;;       mbuf-completion-help-face to icicle-completion-help-face,
;;       mbuf-completion-help-title-face to
;;         icicle-completion-help-title-face,
;;       minibuffer-last-default to icicle-last-completion-candidate,
;;       command-calling-for-completion to
;;         icicle-cmd-calling-for-completion,
;;       minibuffer-completion-help+ to icicle-completion-help+,
;;       abort-minibuffer-input to icicle-abort-minibuffer-input,
;;       next-default-input to icicle-next-completion-candidate,
;;       previous-default-input to
;;         icicle-previous-completion-candidate,
;;       rebind-minibuffer-completion-maps to
;;         icicle-rebind-completion-maps,
;;     Added: minibuffer-complete-and-exit, icicle-fit-frame,
;;            icicle-last-input.
;;     Moved delete-lines here from and renamed to
;;       icicle-delete-lines.
;;     Removed: mod+ (unused).
;;     icicle-completion-help+:
;;       Use *Help*, not *Completions*.  Don't show completions.
;;     icicle-next-completion-candidate:
;;       Use insert, not insert-string.
;;     icicle-rebind-completion-maps: Made it interactive.
;; 2005/07/09 dadams
;;     Removed: buffer-alist (not used).
;; 2005/05/15 dadams
;;     Renamed: flash-ding-minibuffer-frame to
;;              1on1-flash-ding-minibuffer-frame.
;; 2005/05/10 dadams
;;     Hacked completing-read to remove *Completions* window at end
;;       if require-match is non-nil.  (Don't know why/when this
;;       became a problem.)
;; 2004/09/21 dadams
;;     Updated to work in Emacs 21 (and 20):
;;       next-default-input uses delete-minibuffer-contents for 21,
;;       but erase-buffer for 20.
;;       minibuffer-completion-help+:
;;         bind inhibit-read-only to t around erase-buffer.
;; 2001/01/10 dadams
;;     Protected remove-windows-on via fboundp.
;; 1999/09/03 dadams
;;     Added: mbuf-completion-help-face,
;;            mbuf-completion-help-title-face.
;;     minibuffer-completion-help+:
;;       Use mbuf-*-face's instead of hard-coding.
;;     minibuffer-completion-help-string,
;;     completing-read-prompt-suffix: defconst -> defvar.
;; 1999/08/26 dadams
;;     Protected faces via boundp.
;; 1999/04/13 dadams
;;     Bound delete-lines to M-S-DEL and M-S-backspace.
;; 1999/03/17 dadams
;;     protect calls with test fboundp.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/03/26 dadams
;;     minibuffer-completion-help+: concat -> concat-w-faces (color).
;; 1995/12/20 dadams
;;     exit-minibuffer: Iconify *Completion* frame.
;; 1995/12/15 dadams
;;     abort-minibuffer-input:
;;       Reset minibuffer-completion-table to avoid icompletion.
;;     Defined replacement exit-minibuffer to do the same as #1.
;; 1995/12/01 dadams
;;     abort-minibuffer-input: Incorporated delete-selection-mode code
;;     rebind-minibuffer-completion-maps: Added C-g bindings for
;;       minibuffer-local-map, minibuffer-local-ns-map,
;;       minibuffer-local-isearch-map.
;; 1995/10/25 dadams
;;     Put defvar of minibuffer-completion-help-string after do
;;       rebind-minibuffer-completion-maps, so its doc string gives
;;       bindings.
;; 1995/10/24 dadams
;;     Mention ESC-TAB completion in completing-read.
;; 1995/10/17 dadams
;;     Let minibuffer use ESC-TAB for completion (Lisp symbols etc.)
;;     completing-read:
;;       Minibuffer adopts current buffer's ESC-TAB binding.
;;     Added command-calling-for-completion to memorize current
;;       command (done in completion-setup-hook).
;; 1995/09/12 dadams
;;     Added abort-minibuffer-input.
;;     Define C-g as abort-minibuffer-input in
;;       completion-list-mode-map and minibuffer-local-* maps.
;;     No self-insertion for completion-list-mode-map.
;; 1995/08/16 dadams
;;     next-default-input: Fixed bug - skip repeated alist entries.
;; 1995/08/10 dadams
;;     Rewrote minibuffer-completion-help+:
;;       Provide help even if no completions.
;;     So, added minibuffer-completion-help-string.
;;     `?' defined correctly for minibuffer-local-must-match-map.
;; 1995/08/08 dadams
;;     next-default-input: error msg: no hard coding of key seq.
;; 1995/08/02 dadams
;;     Major rewrite.
;;       No reminders in prompts.  Added minibuffer-completion-help+
;;       to provide help info for *Completions*.
;;     Log for functions that were previously in simple+.el:
;;       choose-completion-string, completion-setup-function,
;;       switch-to-completions.
;; 2005/07/28 dadams
;;     completion-setup-function:
;;       Renamed icicle-completing-read-prompt-suffix to
;;         icicle-prompt-suffix.
;; 2005/07/15 dadams
;;     choose-completion-string, completion-setup-function:
;;       Updated for Emacs 21+.
;; 2005/07/10 dadams
;;     Renamed: command-calling-for-completion to
;;              icicle-cmd-calling-for-completion.
;; 2004/09/21 dadams
;;     Only redefine choose-completion-string if prior to Emacs 21.
;; 1999/03/17 dadams
;;     choose-completion-string:
;;       Added doc string.  Updated to correspond to Emacs 34.1.
;;     completion-setup-function: diff prompt setups.
;;       face1 & face2 tests.
;;     Added: switch-to-completions.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(when (< emacs-major-version 20)
  (eval-when-compile (require 'cl))) ;; when, unless

;;;;;;;;;;;;;


;;; Load other Icicles files -------------------------------

(require 'icicles-mode)
(require 'icicles-face)
(require 'icicles-opt)
(require 'icicles-var)
(require 'icicles-mac)
(require 'icicles-cmd)
(require 'icicles-fn)
(when icicle-bind-top-level-commands-flag (require 'icicles-keys))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles.el ends here
