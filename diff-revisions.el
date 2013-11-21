;;; diff-revisions.el --- Show the diffs of all the revisions in time machine.

;; Copyright (C) Ryan Davis

;; Author: Ryan Davis <ryand-ruby@zenspider.com>
;; Keywords: files, vc
;; License: MIT

;;; Commentary:

;; diff-revisions finds all time machine backups for a given buffer
;; displays the rolling diffs in a single diff buffer.
;;
;; This was originally inspired by:
;;     http://www.emacswiki.org/emacs/ediff-trees.el
;;
;; NOTE: this currently ONLY works with local time machine backups.
;; I'm sure it has problems and limitations, but I got it working for
;; my initial need and decided it should be out in the wild.

;;; Code:

(require 'dash)
(require 'f)
(require 'ediff)
(require 'diff)

(defun file-name-all-versions (buffer)
  (with-current-buffer buffer
    (let* ((mobile-backups "/Volumes/MobileBackups/Backups.backupdb")
           (backup-dirs    (directory-files mobile-backups t "^[^.]"))
           (backup-dir     (first backup-dirs))
           (machine-name   (file-name-nondirectory backup-dir))
           (glob           (concat (file-name-as-directory backup-dir)
                                   "*" "/"
                                   machine-name
                                   (buffer-file-name))))
      (-filter 'f-file?
               (nconc (butlast (file-expand-wildcards glob t) 1)
                      (list (buffer-file-name)))))))

(defun each-cons (xs)
  (cond ((null xs) '())
        ((null (cdr xs)) '())
        (t (cons (cons (car xs) (cadr xs))
                 (each-cons (cdr xs))))))

(defun ediff-multifile-collect-changed-files (pairs)
  (-remove (lambda (pair) (ediff-same-file-contents (car pair) (cdr pair)))
           (each-cons pairs)))

(defun diff-revisions-sort-files (files)
  (sort files (lambda (a b) (string< (car a) (car b)))))

;;;###autoload
(defun diff-revisions (buffer)
  (interactive "bBuffer to compare:")
  (let* ((files   (file-name-all-versions buffer))
         (changes (diff-revisions-sort-files
                   (ediff-multifile-collect-changed-files files)))
         (buf     (get-buffer-create "**Diff**"))
         (diffbuf (get-buffer-create "*Diff*")))
    (with-current-buffer diffbuf
      (diff-mode)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (display-buffer diffbuf)
    (mapc (lambda (pair)
            (message "wtf %s" pair)
            (diff-no-select (car pair) (cdr pair) "-u" t buf)
            (save-excursion
              (with-current-buffer diffbuf
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert-buffer-substring buf)))))
          changes)))

(provide 'diff-revisions)

;;; diff-revisions.el ends here
