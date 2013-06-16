;;; multi-replace-regexp.el --- find and replace multiple regexps throughout a document.

;; Copyright (C) by Ryan Davis

;; Author: Ryan Davis <ryand-ruby@zenspider.com>
;; Version 1.0.0
;; Keywords: buffers, no-clue
;; Created: 2010-03-17
;; Compatibility: Emacs 23

;;; Posted using:
;; (emacswiki-post "multi-replace-regexp.el")

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

;; This package provides two functions:
;;
;; (list-to-pairs a b c d ...) # => '((a . b) (c . d) ...) +
;;
;; (multi-replace-regexp a b c d ...) - converts all occurrances in
;;     the buffer of a to b, c to d, etc. Regexps are supported and
;;     the resultant values may be lambdas.

;;; History:

;; 1.0.0 2010-03-17 Birfday.

(require 'cl)

;;;###autoload
(defun list-to-pairs (list)
  "(list-to-pairs a b c d ...) # => '((a . b) (c . d) ...)"
  (loop for (a b c) on list by #'cddr collect
        (cons a b)))

;;;###autoload
(defun multi-replace-regexp (&rest list)
  "converts all occurrances in the buffer of a to b, c to d, etc.
Regexps are supported and the resultant values may be lambdas."
  (save-excursion
    (dolist (pair (list-to-pairs list))
      (let ((from (car pair))
            (to   (cdr pair)))
        (goto-char (point-min))
        (while (re-search-forward from nil t)

          (replace-match (if (stringp to) to (funcall to))))))))

(provide 'multi-replace-regexp)
