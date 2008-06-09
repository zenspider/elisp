;;;
;; pastie.el -- Emacs integration for pastie.caboo.se.
;; Copyright (C) 2006  Christian Neukirchen <purl.org/net/chneukirchen>
;; Licensed under the GPL.
;; 2007-12-27 Updated to work with more recent changes to the pastie API. (Rob Christie)
;; 2007-12-30 Added more major mode sniffs (Ryan McGeary)
;; 2007-12-31 Added some minor mode sniffs that are Rails specific. (Rob Christie)
;; 2008-01-07 Added pastie-browse (Dan McKinley)

(defcustom *pastie-last-url* ""
  "The last url pasted.")

(defun pastie-language ()
  "Sniffs for the language of the region that is being pasted"
  (or (when (boundp 'rails-view-minor-mode) (if rails-view-minor-mode "html_rails"))
      (when (boundp 'rails-minor-mode) (if rails-minor-mode "ruby_on_rails"))
      (cdr (assoc major-mode '((c-mode . "c++")
                               (c++-mode . "c++")
                               (css-mode . "css")
                               (diff-mode . "diff")
                               (html-mode . "html")
                               (java-mode . "java")
			       (python-mode . "python")
                               (javascript-mode . "javascript")
                               (jde-mode . "java")
                               (php-mode . "php")
                               (ruby-mode . "ruby")
                               (text-mode . "plain_text")
                               (sql-mode . "sql")
                               (sh-mode . "shell-unix-generic"))))
      "plain_text"))

(defun pastie-region (begin end)
  "Post the current region as a new paste at pastie.caboo.se.
Copies the URL into the kill ring."
  (interactive "r")

  (let* ((body-raw (buffer-substring begin end))
         (body (replace-regexp-in-string
                "[<>&]"
                (lambda (match)
                  (case (string-to-char match)
                    (?< "&lt;")
                    (?> "&gt;")
                    (?& "&amp;")))
                body-raw))

         ;; Adjust as needed.
         (mode (pastie-language))

         (url-request-method "POST")
         (url-mime-accept-string "application/xml")
         (url-request-extra-headers '(("Content-Type" . "application/xml")))
         (url (url-generic-parse-url "http://pastie.caboo.se/pastes"))

         (url-request-data
          (concat "<paste>"
                  "<parser>" mode "</parser>"
                  "<authorization>burger</authorization>"
                  "<body>" body "</body>"
                  "</paste>")))

    (let ((pastie-buffer (url-retrieve-synchronously url)))
      (with-current-buffer pastie-buffer
        (goto-char (point-min))
        (search-forward-regexp "^Status: \\([0-9]+.*\\)")
        (let ((status (match-string 1)))
          (if (string-match "^20[01]" status)
              (progn
                (goto-char (point-max))
                (beginning-of-line)
                (let ((id (buffer-substring (point) (point-max))))
		  (let ((url (format "http://pastie.caboo.se/paste/%s" id)))
		    (message "Paste created: %s" url)
		    (setq *pastie-last-url* url)
		    (kill-new url))))
            (message "Error occured: %s" status))))
      (kill-buffer pastie-buffer))))

(defun pastie-buffer ()
  "Post the current buffer as a new paste at pastie.caboo.se.
Copies the URL into the kill ring."
  (interactive)
  (pastie-region (point-min) (point-max)))

(defun pastie-get (id)
  "Fetch the contents of the paste from pastie.caboo.se into a new buffer."
  (interactive "nPastie #: ")

  (let ((url-request-method "GET")
        (url-request-extra-headers nil)
        (url-mime-accept-string "*/*")
        (url (url-generic-parse-url
              (format "http://pastie.caboo.se/pastes/%s/download" id))))
    (setq pastie-buffer (url-retrieve-synchronously url))

    (with-current-buffer pastie-buffer
      (goto-char (point-min))
      (search-forward-regexp "^Status: \\([0-9]+.*\\)")
      (let ((status (match-string 1)))
        (if (string-match "^200" status)
            (progn
              (search-forward-regexp
               "^Content-Disposition: attachment; filename=\"\\(.*\\)\"")
              (set-visited-file-name (match-string 1))
              (search-forward-regexp "\n\n")
              (delete-region (point-min) (point))
              (normal-mode)
              (set-buffer-modified-p nil)
              (switch-to-buffer pastie-buffer))
          (message "Error occured: %s" status)
          (kill-buffer pastie-buffer))))))

(defun pastie-browse ()
  (interactive)
  (browse-url *pastie-last-url*))

(provide 'pastie)
