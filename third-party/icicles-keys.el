;;; icicles-keys.el --- Icicles key bindings for top-level commands
;;
;; Filename: icicles-keys.el
;; Description: Icicles key bindings for top-level commands
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2006, Drew Adams, all rights reserved.
;; Created: Tue May 16 09:22:59 2006
;; Version: 22.0
;; Last-Updated: Wed May 17 07:16:39 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 43
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-keys.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `icicles-menu', `subr-21'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It is loaded
;;  automatically if user option `icicle-bind-top-level-commands-flag'
;;  is non-nil (which it is, by default).  This library binds key
;;  sequences in the global map.  It changes several built-in default
;;  key bindings, replacing them with bindings to top-level Icicles
;;  commands.
;;
;;  If you want some of the top-level Icicles commands bound to other
;;  keys instead, you can rebind them after this library is loaded.
;;  Alternatively, you can set `icicle-bind-top-level-commands-flag'
;;  to nil to prevent loading of this library, and just bind the
;;  commands as you like in your init file (~/.emacs).
;;
;;  Besides the global bindings defined here, Icicles binds keys in
;;  the minibuffer keymaps, and it binds keys in the Icicle-mode
;;  keymap (`icicle-mode-map').  Those bindings are made in other
;;  Icicles libraries.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/05/17 dadams
;;     Bind C-c C-S in compilation modes via hooks.
;; 2006/05/16 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'icicles-menu nil t) ;; (no error if not found): icicle-execute-menu-command

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\C-c\C-s" 'icicle-search)
(global-set-key "\C-c/"    'icicle-complete-thesaurus-entry)

;; Replace some standard bindings - use Icicles multi-commands instead.
(global-set-key "\M-x"     'icicle-execute-extended-command)
(substitute-key-definition 'switch-to-buffer 'icicle-buffer global-map)
(substitute-key-definition 'switch-to-buffer-other-window 'icicle-buffer-other-window global-map)
(substitute-key-definition 'find-file 'icicle-find-file global-map)
(substitute-key-definition 'find-file-other-window 'icicle-find-file-other-window global-map)
(substitute-key-definition 'kill-buffer 'icicle-kill-buffer global-map)
(substitute-key-definition 'kill-buffer-and-its-windows 'icicle-kill-buffer global-map)

;; This is not a global binding, but it's convenient to do this here.
(add-hook 'compilation-minor-mode-hook
  (lambda () (define-key compilation-minor-mode-map "\C-c\C-s" 'icicle-compilation-search)))
(add-hook 'compilation-mode-hook
  (lambda () (define-key compilation-mode-map "\C-c\C-s" 'icicle-compilation-search)))

;; This is for Icicles Menu, not Icicles, but it's convenient to do this here.
(when (fboundp 'icicle-execute-menu-command) ; Defined in `icicles-menu.el'.
  (global-set-key [?\e ?\M-x] 'icicle-execute-menu-command)
  (global-set-key [?\M-`] 'icicle-execute-menu-command)) ; Replaces `tmm-menu'.

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-keys.el ends here
