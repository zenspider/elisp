;; toggle.el
;
; DONE:
; blah.rb <-> test_blah.rb
;
; TODO:
; app/model/blah.rb <-> test/unit/blah_test.rb
; lib/blah.rb <-> test/test_blah.rb

(defun impl-to-test (bfn)
  (let ((dir-name (file-name-directory bfn))
	(file-name (file-name-nondirectory bfn)))
    (concat dir-name "test_" file-name)))

(defun test-to-impl (bfn)
  (let ((dir-name (file-name-directory bfn))
	(file-name (substring (file-name-nondirectory bfn) 5)))
    (concat dir-name file-name)))

(defun filename-test-p (bfn)
  (string-match "test_" bfn))

(defun toggle-filename (bfn)
  (cond ((filename-test-p bfn) (test-to-impl bfn))
	('t (impl-to-test bfn))))

(defun toggle-buffer ()
  (interactive)
  (find-file (toggle-filename (buffer-file-name))))

(provide 'toggle)
