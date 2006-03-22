;; toggle.el
;
; blah.rb <-> test_blah.rb
; app/model/blah.rb <-> test/unit/blah_test.rb
; lib/blah.rb <-> test/test_blah.rb

(defvar toggle-mappings
  (list
   (cons "app/model/\\(.*\\).rb" "test/unit/\\1_test.rb")
   (cons "test/unit/\\(.*\\)_test.rb" "app/model/\\1.rb")
   (cons "lib/\\(.*\\).rb" "test/test_\\1.rb")
   (cons "test/test_\\(.*\\).rb" "lib/\\1.rb")
   (cons "test_\\(.*\\).rb" "\\1.rb")
   (cons "\\([^/]+\\).rb" "test_\\1.rb")))

; this was stolen from toggle-source.el, I don't think it is very "lisp-like"
(defun toggle-filename (filename)
  (let ((list nil)
	(entry nil)
	(found nil))
    (setq list toggle-mappings)
    (while (and list (not found) filename)
      (setq entry (car list))
      (setq list  (cdr list))
      (if (string-match (car entry) filename)
	  (setq found (replace-match (cdr entry) nil nil filename))))
    found))

(defun toggle-buffer ()
  (interactive)
  (let ((new-name (toggle-filename (buffer-file-name))))
    (if new-name
	(find-file new-name)
      (message (concat "Match not found for " (buffer-file-name))))))

(provide 'toggle)
