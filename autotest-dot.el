;;; autotest-dot.el --- Shows the current ruby test status as a colored dot in the minibuffer.

;; Copyright (C) Ryan Davis

;; Author: Ryan Davis <ryand-ruby@zenspider.com>
;; URL: https://github.com/zenspider/elisp/blob/master/autotest-dot.el
;; Version 1.1.0
;; Keywords: convenience, tools

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

;; This is an add-on to autotest.el. I don't use this functionality
;; anymore, but I know some people do, so I'm splitting it out for
;; separate maintenance.

;;; History:

;; 1.1.0 - 2014-07-13 - Split out autotest-dot from autotest.

;;; Code:

(require 'autotest)

(eval-when-compile
  (require 'unit-test nil t))

(if (and autotest-use-ui (require 'unit-test nil t))
    (progn
      (message "starting emacs server for autotest")
      (setq unit-test-colours (acons "gray" "#999999" unit-test-colours))
      (setq unit-test-colours (acons "dark-gray" "#666666" unit-test-colours))
      (setq unit-test-running-xpm (unit-test-dot "gray"))
      (server-start)
      (defun autotest-update (status)
        "Updates all buffer's modeline with the current test status."
        (interactive "S")
        (let ((autotest-map (make-sparse-keymap)))
          (define-key autotest-map [mode-line mouse-1] 'autotest-switch)
          (mapcar (lambda (buffer)
                    (with-current-buffer buffer
                      (if (eq status 'quit)
                          (show-test-none)
                        (progn
                          (show-test-status status)
                          (put-text-property
                           0 3
                           'keymap autotest-map
                           (car mode-line-buffer-identification))))))
                  (remove-if 'minibufferp (buffer-list))))
        status))
  (message "unit-test not found, not starting autotest/emacs integration"))

(provide 'autotest-dot)

;;; autotest-dot.el ends here
