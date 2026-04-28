;;; rcov-overlay.el --- Colorize untested ruby code  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2026 by Ryan Davis

;; Author: Ryan Davis <ryand-ruby@zenspider.com>
;; URL: https://github.org/zenspider/elisp
;; Keywords: tools, languages
;; Version: 2.0
;; Package-Requires: ((a "1.0") (json "1.5") (dash) (cl-lib "0.5") (emacs "29"))

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
;; according to rcov by using simplecov's serialized data. Also
;; provided are two rake tasks to help generate the needed data.

;; This library used to be tooled to work with rcov, but that library
;; has died.

;; See function `coverage-update' and `coverage-clear'.

;; Special thanks to Phil Hagelberg for writing augment, a much more
;; complete package for doing similar things (but still alpha enough
;; that I needed something working now).

;;; History:

;; 2.0 2026-04-19 Overhaul for simplecov, added summary, normalize naming.
;; 1.3 2022-10-17 Updated for modern emacs & simplecov, requires the 'a package.
;; 1.2 2009-10-21 Added customizable overlay background color.
;; 1.1 2008-12-01 Added find-project-dir to fix path generation issues.
;; 1.0 2008-01-14 Birfday.

;;; Code:

(require 'cl)
(require 'json)                         ; built into emacs now
(require 'a)                            ; from melpa
(require 'dash)

(defcustom coverage-fg-color
  "#ffcccc"
  "The default background color."
  :group 'coverage
  :type 'color)

;; ruby-mode-map

;;;###autoload
(defun coverage-update ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (coverage-clear)
    (->> (current-buffer)
         (coverage--for-buffer)
         (coverage--to-lines)
         (coverage--lines-to-regions)
         (coverage--regions-to-overlays))))

(define-obsolete-function-alias 'show-coverage 'coverage-update "2026-04-15")

;;;###autoload
(defun coverage-clear ()
  (interactive)
  (remove-overlays))

;;;###autoload
(defun coverage-summary ()
  (interactive)
  (let* ((buffer   (current-buffer))
         (cov-path (coverage--find-file-for-buffer buffer))
         (coverage (coverage--read-json cov-path))
         (top-key  (caar coverage))
         (paths    (mapcar #'car (a-get* coverage top-key "coverage"))) ; FIX
         (projpath (f-dirname (f-dirname cov-path))))
    (with-output-to-temp-buffer "*Coverage Summary*"
      (setq default-directory projpath)

      (with-current-buffer standard-output
        (button-mode)
        (insert "Test Coverage Summary:\n\n")
        (insert (format "%-40s %6s%% %4s %4s %4s %4s %6s\n" "Path" "cov" "line" "relv" "cov" "miss" "avghit"))
        (mapc (lambda (path)
                (let* ((data (a-get* coverage top-key "coverage" path "lines"))
                       (args (coverage--summary-data data)))
                  (insert
                   (apply #'format
                          "%-40s %6.2f%% %4d %4d %4d %4d %6.2f\n"
                          (buttonize (f-relative path projpath) #'find-file path)
                          args))))
              (sort paths))
        (pop-to-buffer "*Coverage Summary*")))))

;;; Utilities:

(defun coverage--find-project-dir (file &optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir file))
      dir
    (if (equal dir "/")
        nil
      (coverage--find-project-dir file (expand-file-name (concat dir "../"))))))

(defun coverage--find-file-for-buffer (buffer-name)
  (with-current-buffer buffer-name
    (let* ((cov-file "coverage/.resultset.json")
           (base-dir (coverage--find-project-dir cov-file))
           (cov-path (concat base-dir cov-file)))
      cov-path)))

(defun coverage--read-json (cov-path)
  (let ((json-array-type 'list)
        (json-key-type   'string))
    (json-read-file cov-path)))

(defun coverage--for-buffer (buffer-name)
  (let* ((buffer      (get-buffer buffer-name))
         (source-path (buffer-file-name buffer))
         (cov-path    (coverage--find-file-for-buffer buffer-name))
         (coverage    (coverage--read-json cov-path))
         (top-key     (caar coverage)))
    (a-get* coverage top-key "coverage" source-path "lines")))

(defun coverage--summary-data (data)
  (let* ((size    (length data))
         (counts  (seq-keep  #'identity data))
         (relv    (length counts))
         (cov     (seq-count #'plusp counts))
         (miss    (seq-count #'zerop counts))
         (sum     (apply #'+ counts)))
    (list (/ (* 100.0 cov) relv) size relv cov miss (/ (float sum) relv))))

(defun coverage--to-lines (coverage)
  (->> coverage
       (-map-indexed #'cons)
       (--filter (eq 0 (cdr it)))
       (-map #'car)
       (-map #'1+)))

(defun coverage--lines-to-regions (lines)
  (--map (cons (line-beginning-position it)
               (line-end-position it))
         lines))

(defun coverage--regions-to-overlays (regions)
  (--each regions
    (overlay-put (make-overlay (car it) (cdr it))
                 'face (cons 'background-color "#ffcccc"))))

(provide 'rcov-overlay)

;;; coverage.el ends here
