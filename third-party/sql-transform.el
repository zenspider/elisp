;;; sql-transform.el --- transform SQL statements

;; Copyright (C) 1998, 1999  Alexander Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 2.1.1
;; Keywords: SQL

;; This file is part of GNU Emacs.

;; sql-transform.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; sql-transform.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Please send me bug reports and bug fixes.

;; This is usefull if you are developping code that needs a set of
;; INSERT, SELECT, UPDATE, and DELETE statements for the same table.
;; This set of functions will parse any of these four SQL statements and
;; rewrite them for you (if they are simple, eg. no joins to other
;; tables).  Just mark a SQL statement and call sql-to-insert,
;; sql-to-select, sql-to-update, or sql-to-delete.

;; Obviously this works best if you start with a SELECT or UPDATE
;; statement, because DELETE statements don't have columns and
;; bindvariables, and INSERT statements don't have where clauses.

;; Check out my Emacs page located here:
;; <URL:http://www.geocities.com/TimesSquare/6120/emacs.html>.

;; A possible way to customize your .emacs would be to use the following
;; (assuming you are using sql-mode.el from the page mentioned above):

;; (add-hook 'sql-mode-hook
;;        (function (lambda ()
;;                    (local-set-key "\C-cu" 'sql-to-update))))

;;; Change Log:

;; 1998-10-02 Wrote the Perl script and added the functions.
;; 1999-09-13 Abandoned Perl script and rewrote sql-to-update in elisp.
;; 1999-09-14 Added code to indent according to a target column.



;;; Code:

(defun sql-to-update (start end)
  "Converts SQL statement in region to an UPDATE statement."
  (interactive "r")
  (let* ((statement (sql-parse-statement start end))
         (table (car statement))
         (where (nth 1 statement))
         (columns (nth 2 statement))
         (bindvars (nth 3 statement))
         (target-column))
    (goto-char start)
    (setq target-column (current-column))
    (kill-region start end)
    (insert "UPDATE\t" table)
    (while (and columns)
      (newline 1)
      (indent-to target-column)
      (insert (format "SET\t%s = %s" (car columns) (car bindvars)))
      (setq columns (cdr columns)
            bindvars (cdr bindvars)))
    (if where
        (progn
          (newline 1)
          (indent-to target-column)
          (insert "WHERE\t" where)))))

(defun sql-to-insert (start end)
  "Converts SQL statement in region to an INSERT statement."
  (interactive "r")
  (let* ((statement (sql-parse-statement start end))
         (table (car statement))
         (where (nth 1 statement))
         (columns (nth 2 statement))
         (bindvars (nth 3 statement))
         (no-newlines (< (length columns) 6))
         (target-column))
    (goto-char start)
    (setq target-column (current-column))
    (kill-region start end)
    (insert "INSERT INTO " table " (")
      (if no-newlines
          (insert " ")
        (newline 1)
        (indent-to target-column)
        (insert "\t"))
    (insert (car columns))
    (setq columns (cdr columns))
    (while columns
      (insert ",")
      (if no-newlines
          (insert " ")
        (newline 1)
        (indent-to target-column)
        (insert "\t"))
      (insert (car columns))
      (setq columns (cdr columns)))
    (insert " )")
    (newline 1)
    (indent-to target-column)
    (insert "VALUES ( " (car bindvars))
    (setq bindvars (cdr bindvars))
    (while bindvars
      (insert ",")
      (if no-newlines
          (insert " ")
        (newline 1)
        (indent-to target-column)
        (insert "\t"))
      (insert (car bindvars))
      (setq bindvars (cdr bindvars)))
    (insert " )")))

(defun sql-to-select (start end)
  "Converts SQL statement in region to a SELECT statement."
  (interactive "r")
  (let* ((statement (sql-parse-statement start end))
         (table (car statement))
         (where (nth 1 statement))
         (columns (nth 2 statement))
         (bindvars (nth 3 statement))
         (no-newlines (< (length columns) 6))
         (target-column))
    (goto-char start)
    (setq target-column (current-column))
    (kill-region start end)
    (insert (format "SELECT\t%s" (car columns)))
    (setq columns (cdr columns))
    (while columns
      (insert ",")
      (if no-newlines
          (insert " ")
        (newline 1)
        (indent-to target-column)
        (insert "\t"))
      (insert (car columns))
      (setq columns (cdr columns)))
    (newline 1)
    (indent-to target-column)
    (insert (format "INTO\t%s" (car bindvars)))
    (setq bindvars (cdr bindvars))
    (while bindvars
      (insert ",")
      (if no-newlines
          (insert " ")
        (newline 1)
        (indent-to target-column)
        (insert "\t"))
      (insert (car bindvars))
      (setq bindvars (cdr bindvars)))
    (newline 1)
    (indent-to target-column)
    (insert "FROM\t" table)
    (if where
        (progn
          (newline 1)
          (indent-to target-column)
          (insert "WHERE\t" where)))))

(defun sql-to-insert (start end)
  "Converts SQL statement in region to an INSERT statement."
  (interactive "r")
  (let* ((statement (sql-parse-statement start end))
         (table (car statement))
         (where (nth 1 statement))
         (columns (nth 2 statement))
         (bindvars (nth 3 statement))
         (no-newlines (< (length columns) 6))
         (target-column))
    (goto-char start)
    (setq target-column (current-column))
    (kill-region start end)
    (insert "INSERT INTO " table " (")
    (if no-newlines
        (insert " ")
      (newline 1)
      (indent-to target-column)
      (insert "\t"))
    (insert (format "%s" (car columns)))
    (setq columns (cdr columns))
    (while columns
      (insert ",")
      (if no-newlines
          (insert " ")
        (newline 1)
        (indent-to target-column)
        (insert "\t"))
      (insert (car columns))
      (setq columns (cdr columns)))
    (insert " )")
    (newline 1)
    (indent-to target-column)
    (insert "VALUES (")
    (if no-newlines
        (insert " ")
      (newline 1)
      (indent-to target-column)
      (insert "\t"))
    (insert (format "%s" (car bindvars)))
    (setq bindvars (cdr bindvars))
    (while bindvars
      (insert ",")
      (if no-newlines
          (insert " ")
        (newline 1)
        (indent-to target-column)
        (insert "\t"))
      (insert (car bindvars))
      (setq bindvars (cdr bindvars)))
    (insert " )")
    (if (and where
             (string-match "\\(;\\(.\\|\n\\)*\\)" where))
        (insert (match-string 1 where)))))

(defun sql-to-delete (start end)
  "Converts SQL statement in region to a DELETE statement."
  (interactive "r")
  (let* ((statement (sql-parse-statement start end))
         (table (car statement))
         (where (nth 1 statement))
         (target-column))
    (goto-char start)
    (setq target-column (current-column))
    (kill-region start end)
    (insert "DELETE FROM " table)
    (newline 1)
    (indent-to target-column)
    (if where
        (insert "WHERE\t" where))))



(defun sql-parse-statement (start end)
  "Parses the region for a SQL statement and returns the parsed statement.
The value returned is a list (TABLE WHERE COLUMNS BINDVARS).  TABLE is
the name of a table (the statement may only use one table), WHERE is the
where clause of the statement (verbatim), COLUMNS is a list of column
names, and BINDVARS is a list of bindvariables.  BINDVARS may be an
empty list.

A possible result would be
\(\"EMP\" \"WHERE EMPNO = 1\" (\"EMPNO\" \"NAME\") (\"v_empno\", \"v_name\"))"
  (interactive "r")
  (goto-char start)
  (re-search-forward "\\b\\(select\\|insert\\|update\\|delete\\)\\b" end t)
  (cond ((null (match-string 1))
         (message "No SQL statement found"))
        ((string-equal  (upcase (match-string 1)) "SELECT")
         (sql-parse-select start end))
        ((string-equal  (upcase (match-string 1)) "UPDATE")
         (sql-parse-update start end))
        ((string-equal  (upcase (match-string 1)) "INSERT")
         (sql-parse-insert start end))
        ((string-equal  (upcase (match-string 1)) "DELETE")
         (sql-parse-delete start end))
        (t (message "%s statements cannot be parsed, yet" (upcase (match-string 1))))))

(defun sql-test-parse-statement (start end)
  "Test `sql-parse-statement'."
  (interactive "r")
  (message "%S" (sql-parse-statement start end)))



(defun sql-parse-select  (start end)
  "Parses the region for a SELECT statement and returns the parsed statement.
See `sql-parse-statement' for more information."
  (interactive "r")
  (let ((columns)
        (bindvars)
        (table)
        (where))
    ;; columns: get comma separated list after SELECT keyword.  Use
    ;; (point) instead of START as a parameter to `sql-parse-csv' in
    ;; order to exclude the SELECT keyword.
    (goto-char start)
    (if (re-search-forward "\\bselect\\b" end t)
        (setq columns (sql-parse-csv (point) end)))
    ;; bindvars: get comma separated list after INTO keyword.
    (goto-char start)
    (if (re-search-forward "\\binto\\b" end t)
        (setq bindvars (sql-parse-csv (point) end)))
    ;; test for equal numbers
    (if (and bindvars
             (not (= (length columns)
                     (length bindvars))))
        (message "Parser warning: not the same number of columns and bindvars"))
    ;; table: get string after FROM keyword.
    (goto-char start)
    (if (re-search-forward "\\bfrom[ \t\n]+\\(\\(\\sw\\|\\s_\\)+\\)" end t)
        (setq table (match-string 1))
      (error "FROM clause missing"))
    ;; where clause: get everything after the WHERE keyword.
    (goto-char start)
    (if (re-search-forward "\\bwhere[ \t\n]+\\(\\(.\\|\n\\)+\\)" end t)
        (setq where (match-string 1)))
    (list table where columns bindvars)))

(defun sql-parse-insert  (start end)
  "Parses the region for an INSERT statement and returns the parsed statement.
See `sql-parse-statement' for more information."
  (interactive "r")
  (let ((columns)
        (bindvars)
        (table))
    ;; table: get string after INSERT INTO keywords.
    (goto-char start)
    (if (re-search-forward "\\binsert[ \t\n]+into[ \t\n]+\\(\\(\\sw\\|\\s_\\)+\\)" end t)
        (setq table (match-string 1))
      (error "INTO clause missing"))
    ;; columns: get comma separated list after the opening bracket.  Use
    ;; (point) instead of START as a parameter to `sql-parse-csv' in
    ;; order to exclude the table name.
    (if (re-search-forward "\\=[ \t\n]*(" end t)
        (setq columns (sql-parse-csv (point) end)))
    ;; bindvars: get comma separated list after "VALUES (" keyword.
    (goto-char start)
    (if (re-search-forward "\\bvalues[ \t\n]*(" end t)
        (setq bindvars (sql-parse-csv (point) end)))
    ;; test for equal numbers
    (if (not (= (length columns)
                (length bindvars)))
        (message "Parser warning: not the same number of columns and bindvars"))
    (list table nil columns bindvars)))

(defun sql-parse-update  (start end)
  "Parses the region for an UPDATE statement and returns the parsed statement.
See `sql-parse-statement' for more information."
  (interactive "r")
  (let ((columns)
        (bindvars)
        (table)
        (where))
    ;; table: get string after UPDATE keyword.
    (goto-char start)
    (if (re-search-forward "\\bupdate[ \t\n]+\\(\\(\\sw\\|\\s_\\)+\\)" end t)
        (setq table (match-string 1))
      (error "table missing"))
    ;; columns/bindvars: parse list of assignments starting after the
    ;; table.
    (let ((assignments (sql-parse-assignments (point) end)))
      (setq columns (car assignments)
            bindvars (cdr assignments)))
    ;; where clause: get everything after the WHERE keyword.
    (goto-char start)
    (if (re-search-forward "\\bwhere[ \t\n]+\\(\\(.\\|\n\\)+\\)" end t)
        (setq where (match-string 1)))
    (list table where columns bindvars)))

(defun sql-parse-delete  (start end)
  "Parses the region for a DELETE statement and returns the parsed statement.
See `sql-parse-statement' for more information."
  (interactive "r")
  (let ((table)
        (where))
    ;; table: get string after DELETE FROM keywords.
    (goto-char start)
    (if (re-search-forward "\\bdelete[ \t\n]+from[ \t\n]+\\(\\(\\sw\\|\\s_\\)+\\)" end t)
        (setq table (match-string 1))
      (error "table missing"))
    ;; where clause: get everything after the WHERE keyword.
    (goto-char start)
    (if (re-search-forward "\\bwhere[ \t\n]+\\(\\(.\\|\n\\)+\\)" end t)
        (setq where (match-string 1)))
    (list table where nil nil)))



(defun sql-parse-csv (start end)
  "Parse a list of comma separated words and return them as a list.
Point must be placed at the start of the list.  A word is a string
consisting of word and symbol characters according to the current syntax
table, possibly prefixed by a punctuation character (usually a colon
used for bind variables)."
  (interactive "r")
  (goto-char start)
  (let ((words)
        (word_regexp "[ \t\n]*\\(\\s.?\\(\\sw\\|\\s_\\)+\\)"))
    (if (re-search-forward word_regexp end t)
        (progn
          (setq words (list (match-string 1)))
          (while (re-search-forward (concat "\\=[ \t\n]*," word_regexp) end t)
            (setq words (nconc words (list (match-string 1)))))))
    words))

(defun sql-test-parse-csv (start end)
  "Test `sql-parse-csv'."
  (interactive "r")
  (message "%S" (sql-parse-csv start end)))


(defun sql-parse-assignments (start end)
  "Parse a list of assignments.
The region between START and END is parsed and returned as two lists in
a cons cell.  Point must be placed at the start of the list.  An
assignment consists of the keyword \"SET\" followed by a word, an equal
sign, and another word.  A word is a string consisting of word and
symbol characters according to the current syntax table.  The word on
the right side of the equal sign may be prefixed by a punctuation
character \(usually a colon used for bind variables\)."
  (interactive "r")
  (goto-char start)
  (let* ((columns)
         (bindvars)
         (left_regexp "[ \t\n]*\\(\\(\\sw\\|\\s_\\)+\\)")
         (right_regexp "[ \t\n]*\\(\\s.?\\(\\sw\\|\\s_\\)+\\)")
        (assign_regexp (concat "SET" left_regexp "[ \t\n]*=" right_regexp)))
    (if (re-search-forward assign_regexp end t)
        (progn
          (setq columns (list (match-string 1))
                bindvars (list (match-string 3)))
          (while (re-search-forward (concat "\\=[ \t\n]*" assign_regexp) end t)
            (setq columns (nconc columns (list (match-string 1)))
                  bindvars (nconc bindvars (list (match-string 3)))))))
    (cons columns bindvars)))

(defun sql-test-parse-assignments (start end)
  "Test `sql-parse-assignments'."
  (interactive "r")
  (message "%S" (sql-parse-assignments start end)))

;; sql-transform.el ends here