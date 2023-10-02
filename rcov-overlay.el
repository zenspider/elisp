;;; rcov-overlay.el --- Colorize untested ruby code

;; Copyright (C) 2008-2022 by Ryan Davis

;; Author: Ryan Davis <ryand-ruby@zenspider.com>
;; URL: https://github.org/zenspider/elisp
;; Keywords: tools, languages
;; Version: 1.3
;; Package-Requires: ((a "1.0") (cl-lib "0.5") (emacs "24.1"))

;;; Posted using:
;; (emacswiki-post "rcov-overlay.el")

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

;; This package provides the ability to highlight untested code
;; according to rcov by using rcov's coverage.info serialized
;; data. Also provided are two rake tasks to help generate the needed
;; data.

;; See function `show-coverage'.

;; The function `overlay-current-buffer-with-command` is actually
;; quite flexible as it will execute an external command that returns
;; json data specifying regions and colors. It could be used for all
;; sorts of mischief.

;; Special thanks to Phil Hagelberg for writing augment, a much more
;; complete package for doing similar things (but still alpha enough
;; that I needed something working now).

;;; History:

;; 1.3 2022-10-17 Updated for modern emacs & simplecov, requires the 'a package.
;; 1.2 2009-10-21 Added customizable overlay background color.
;; 1.1 2008-12-01 Added find-project-dir to fix path generation issues.
;; 1.0 2008-01-14 Birfday.

;;; Code:

(require 'cl)
(require 'json)                         ; built into emacs now
(require 'a)                            ; from melpa
(require 'dash)

(defcustom rcov-overlay-fg-color
  "#ffcccc"
  "The default background color."
  :group 'rcov-overlay
  :type 'color)

;; ruby-mode-map

;;;###autoload
(defun show-coverage ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (remove-overlays)
    (->> (buffer-name)
         (coverage-for-buffer)
         (coverage-to-lines)
         (lines-to-regions)
         (make-overlays-for-regions))))

;;;###autoload
(defun rcov-clear ()
  (interactive)
  (remove-overlays))

;;; Utilities:

(defun find-coverage-file-for-buffer (buffer-name)
 (with-current-buffer buffer-name
   (let* ((cov-file "coverage/.resultset.json")
          (base-dir (find-project-dir cov-file))
          (cov-path (concat base-dir cov-file)))
     cov-path)))

(defun coverage-read-json (cov-path)
  (let ((json-array-type 'list)
        (json-key-type   'string))
    (json-read-file cov-path)))

(defun coverage-for-buffer (buffer-name)
  (let* ((buffer (get-buffer buffer-name))
         (source-path (buffer-file-name buffer))
         (cov-path (find-coverage-file-for-buffer buffer-name))
         (coverage (coverage-read-json cov-path)))
    (a-get* coverage "Minitest" "coverage" source-path "lines")))

(defun coverage-to-lines (coverage)
  (->> coverage
       (-map-indexed #'cons)
       (--filter (eq 0 (cdr it)))
       (-map #'car)
       (-map #'1+)))

(defun lines-to-regions (lines)
  (--map (cons (point-at-bol it)
               (point-at-eol it))
         lines))

(defun make-overlays-for-regions (regions)
  (--each regions
    (overlay-put (make-overlay (car it) (cdr it))
                 'face (cons 'background-color "#ffcccc"))))

(defun find-project-dir (file &optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir file))
      dir
    (if (equal dir "/")
        nil
      (find-project-dir file (expand-file-name (concat dir "../"))))))

(provide 'rcov-overlay)

;;; rcov-overlay.el ends here
