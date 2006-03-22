;; toggle.el
;
; blah.rb <-> test_blah.rb
; app/model/blah.rb <-> test/unit/blah_test.rb
; lib/blah.rb <-> test/test_blah.rb

(defcustom toggle-mappings
  '(("app/model/\\(.*\\).rb" . "test/unit/\\1_test.rb")
    ("test/unit/\\(.*\\)_test.rb" . "app/model/\\1.rb")
    ("lib/\\(.*\\).rb" . "test/test_\\1.rb")
    ("test/test_\\(.*\\).rb" . "lib/\\1.rb")
    ("test_\\(.*\\).rb" . "\\1.rb")
    ("\\([^/]+\\).rb" . "test_\\1.rb"))
  "A list of 'from -> to' regex mappings used by toggle-filename")

(defun toggle-filename (path paths)
  "Finds the first entry in paths that matches path and returns
path after applying substitutions, or nil if not matched."
  (cond ((null paths) '())
	((string-match (caar paths) path)
	 (replace-match (cdar paths) nil nil path))
	(t (toggle-filename path (cdr paths)))))

(defun toggle-buffer ()
  "Matches the current buffer file name against the patterns in
toggle-mappings. If a match is found, switches to that buffer."
  (interactive)
  (let ((new-name (toggle-filename (buffer-file-name) toggle-mappings)))
    (if new-name
	(find-file new-name)
      (message (concat "Match not found for " (buffer-file-name))))))

(provide 'toggle)

