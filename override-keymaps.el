;;; override-keymaps.el --- for when you want a global binding to really be global

;; Copyright (C) 2005-2008 by Ryan Davis

;; Author: Ryan Davis <ryand-ruby@zenspider.com>
;; Version 1.0.0
;; Keywords: no-freakin-clue
;; Created: 2006-03-22
;; Compatibility: Emacs 22, 21?
;; URL(en): http://seattlerb.rubyforge.org/

;;; Posted using:
;; (emacswiki-post "toggle.el")

;;; The MIT License:

;; http://en.wikipedia.org/wiki/MIT_License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Provides a means of overriding major-modes' keymaps when you want a
;; global key binding to really be global.

;;; History:

;; TODO: fix this:
;; 1.3.1 2008-09-25 Fixed doco & typo in rspec patterns.
;; 1.3.0 2007-05-10 Added tab completion to toggle-style. Suggested by TingWang.
;; 1.2.0 2007-04-06 Interleave bidirectional mappings. Fixed interactive setter.
;;                  Added rspec mappings.
;; 1.1.0 2007-03-30 Initial release to emacswiki.org. Added named styles and bidi.
;; 1.0.0 2006-03-22 Birfday.

(eval-when-compile (require 'cl))

;; TODO:
;; (require 'bind-key)
;; (defmacro bind-key* (key-name command)
;;   `(progn
;;      (bind-key ,key-name ,command)
;;      (define-key override-global-map ,(read-kbd-macro key-name) ,command)))

(defcustom override-keymap-rules
  '(("\M-\C-x" bury-buffer (ruby python emacs-lisp)))
  "A list of rules for overriding various modes that set their keymaps on your favorite keys"
  :group 'override-keymaps
  :type '(repeat (list string symbol (repeat symbol))))

(defun concat-symbols (&rest symbols)
  "Concatinate a list of symbols, returning a new interned symbol."
  (intern (apply 'concat (mapcar 'symbol-name symbols))))

(defun override-keymap (mode key fn)
  "Appends a hook for mode to set key for fn. This allows you to override a mode's default keys if you really really want a given key to invoke a function.

eg: \(override-keymap 'ruby \"\\M-\\C-x\" 'bury-buffer)"
  (add-hook (concat-symbols mode '-mode-hook)
            `(lambda ()
               (define-key (symbol-value (concat-symbols ',mode '-mode-map))
                 ,key ',fn)) t))

(defun override-keymaps (&optional rules)
  "Processes a list of rules of the form '(key fn (modes)) and calls override-keymap on each set of key/fn for each mode specified."
  (loop for (key fn modes) in (or rules override-keymap-rules)
        do (loop for mode in modes
                 do (override-keymap mode key fn))))

(provide 'override-keymaps)
