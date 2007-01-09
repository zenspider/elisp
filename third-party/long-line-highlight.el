;;; long-line-highlight.el - Highlight long lines, first green, 
;;                           then yellow, 
;;                           then red behind the 80th column
;;          
;; Copyright (C) 2006 Stefan Schimanski <sts@1stein.org>

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Install:
;; 
;;  (require 'long-line-highlight)
;;

;;; Usage:
;;
;;  A new minor mode long-line-highlight-mode is defined. You can toggle
;;  it by calling long-line-highlight-mode.
;;  To enable it directly, do something like that:
;;
;;  (add-hook 'text-mode-hook (lambda () (long-line-highlight-mode 1)))
;;  (add-hook 'tex-mode-hook (lambda () (long-line-highlight-mode 1)))
;;  (add-hook 'latex-mode-hook (lambda () (long-line-highlight-mode 1)))
;;  (add-hook 'emacs-lisp-mode-hook (lambda () (long-line-highlight-mode 1)))
;;  (add-hook 'shell-script-mode-hook (lambda () (long-line-highlight-mode 1)))
;;  (add-hook 'sh-mode-hook (lambda () (long-line-highlight-mode 1)))
;;  (add-hook 'muse-mode-hook (lambda () (long-line-highlight-mode 1)))
;;  (add-hook 'c-mode-hook (lambda () (long-line-highlight-mode 1)))
;;  (add-hook 'message-mode-hook (lambda () (long-line-highlight-mode 1)))
;;  (add-hook 'scheme-mode-hook (lambda () (long-line-highlight-mode 1)))

;;; Code:

(require 'easy-mmode)

(defgroup long-line-highlight nil
  "Long-line-highlight minor mode"
  :group 'tools)

(defface long-line-highlight-green-face 
  '((((class color)) (:foreground "green")))
  "Face for column 72"
  :group 'long-line-highlight)

(defface long-line-highlight-yellow-face 
  '((((class color)) (:foreground "yellow")))
  "Face for column 73-80"
  :group 'long-line-highlight)

(defface long-line-highlight-red-face 
  '((((class color)) (:weight bold :foreground "red")))
  "Face for columns 81+"
  :group 'long-line-highlight)

(defvar long-line-highlight-keywords
  '(("^.\\{71\\}\\(.\\)" (1 'long-line-highlight-green-face append))
    ("^.\\{72\\}\\(.\\{,8\\}\\)" (1 'long-line-highlight-yellow-face append))
    ("^.\\{80\\}\\(.*\\)$" (1 'long-line-highlight-red-face append)))
  "Faces to highlight characters right of the 72th column" )

(easy-mmode-define-minor-mode
 long-line-highlight-mode 
 "Highlight long lines, first green, then yellow, then red
  behind the 80th column" 
 nil
 " LLH")

(add-hook 'long-line-highlight-mode-on-hook
	  (lambda () 
	    (font-lock-add-keywords nil long-line-highlight-keywords)
	    (when font-lock-fontified (font-lock-fontify-buffer))))

(add-hook 'long-line-highlight-mode-off-hook
	  (lambda () 
	    (font-lock-remove-keywords nil long-line-highlight-keywords)
	    (when font-lock-fontified (font-lock-fontify-buffer))))

(provide 'long-line-highlight)
