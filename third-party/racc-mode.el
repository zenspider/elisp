;;; racc-mode.el --- A mode for editing racc (like yacc/bison for ruby) files.
;;
;; Author: sheepman
;; Maintainer: Ryan Davis
;; URL: http://github.com/zenspider/elisp
;; Copyright: (c) 2001 sheepman
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This is a fork of the original from "sheepman", who has been
;; incommunicado for years. It is largely unchanged from the 2001
;; version but I've added some changes to make it work better with
;; emacs changes over the years.
;;
;; Add the following to your emacs setup:
;;
;;   (autoload 'racc-mode "racc-mode" "alternate mode for editing racc files" t)
;;   (add-to-list 'load-path "/path/to/racc-mode-directory")
;;   (add-to-list 'auto-mode-alist '("\\.ry$" . racc-mode))
;;   (add-to-list 'auto-mode-alist '("\\.yy$" . racc-mode))

(require 'derived)

;;; Code:

(defcustom racc-token-length-max 11
  "The maximum length of a racc-token. Helps with indentation."
  :type 'integer
  :safe #'integerp
  :group 'racc)

(define-derived-mode racc-mode ruby-mode "Racc"
  "Major mode for Racc editing."
  (interactive)
  (setq indent-line-function 'racc-indent-line)
  (define-key racc-mode-map "\t" 'racc-indent-command)
  (define-key racc-mode-map "\C-j" 'racc-reindent-newline-and-indent)
  (run-hooks 'racc-mode-hook))

(defun racc-indent-command ()
  (interactive)
  (racc-indent-line t))

(defun racc-reindent-newline-and-indent ()
  (interactive)
  (racc-indent-command)
  (newline)
  (racc-indent-command))

(defun racc-indent-line (&optional flag)
  (if (string-match "^[ \t]*rule[ \t]*$" (current-line))
      (racc-indent-to 0)
    (if (point-in-rule)
        (racc-indent-to (racc-calculate-indent))
      (if (point-before-rule)
          (racc-indent-to (racc-calculate-indent-before-rule))
        (ruby-indent-line t)))))


(defun beginning-of-rule ()
  (beginning-of-buffer)
  (re-search-forward "^rule")
  (beginning-of-line))

(defun end-of-rule ()
  (beginning-of-buffer)
  (re-search-forward "^end")
  (end-of-line))

(defun point-in-rule ()
  (save-excursion
    (and (re-search-backward-without-moving-point "^rule")
         (or (not (re-search-backward-without-moving-point "^end"))
             (< (re-search-backward-without-moving-point "^end")
                (re-search-backward-without-moving-point "^rule"))))))

(defun point-before-rule ()
  (save-excursion
    (and (re-search-backward-without-moving-point "^class"))
    (re-search-forward-without-moving-point "^rule")))

(defun point-in-prec ()
  (save-excursion
    (and (point-before-rule)
         (or (and (re-search-backward-without-moving-point "^prechigh")
                  (re-search-forward-without-moving-point "^preclow"))
             (and (re-search-forward-without-moving-point "^prechigh")
                  (re-search-backward-without-moving-point "^preclow"))
             ))))

(defun point-in-convert ()
  (save-excursion
    (point-before-rule)
    (and (re-search-backward-without-moving-point "^convert")
         (re-search-forward-without-moving-point "^end"))))

(defun point-in-token-statement ()
  (save-excursion
    (defun match-token-line (str)
      (or (string-match "^[ \t]*|[ \t]*"
                        str)
          (string-match "^[ \t]*[^ \t\n]+[ \t]*:"
                        str)))
    (and (point-in-rule)
         (or (match-token-line (current-line))
             (and (not (string-match "^[ \t]*[{}][ \t]*$" (current-line)))
                  (save-excursion
                    (forward-line -1)
                    (match-token-line (current-line)))
                  (save-excursion
                    (forward-line 1)
                    (match-token-line (current-line))))))))

(defun point-in-action-statement ()
  (and (point-in-rule)
       (not (point-in-token-statement))))

(defun beginning-of-action ()
  (if (point-in-action-statement)
      (re-search-backward "^[ \t]*{" nil t)
    nil))

(defvar racc-indent-level 2)

(defun racc-indent-to (x)
  (ruby-indent-to x))

(defun racc-calculate-indent ()
  (save-excursion
    (if (point-in-token-statement)
        (if (string-match "^[ \t]*|[ \t]*" (current-line))
            (+ racc-indent-level
               racc-token-length-max)
          (if (string-match "\\([^ \t\n]+[ \t]*\\):" (current-line))
              (let ((strlen (string-length
                             (match-string 1 (current-line))))
                    (ind (+ racc-indent-level
                            racc-token-length-max)))
                (if (> strlen ind)
                    0
                  (- ind strlen)))
            (+ racc-indent-level
               racc-token-length-max
               (* 2 racc-indent-level))))
      (if (string-match "^[ \t]*[{}][ \t]*$" (current-line))
          (+ racc-indent-level
             racc-token-length-max
             (* 2 racc-indent-level))
        (ruby-calculate-indent (max (save-excursion
                                      (ruby-beginning-of-indent)
                                      (point))
                                    (save-excursion
                                      (beginning-of-action)
                                      (point))))))))

(defun racc-calculate-indent-before-rule ()
  (cond ((or (string-match "^[ \t]*prechigh" (current-line))
             (string-match "^[ \t]*preclow" (current-line)))
         0)
        ((point-in-prec)
         racc-indent-level)
        ((string-match "^[ \t]*expect" (current-line))
         0)
        ((or (string-match "^[ \t]*convert" (current-line))
             (string-match "^[ \t]*end" (current-line)))
         0)
        ((point-in-convert)
         racc-indent-level)
        ((string-match "^[ \t]*token" (current-line))
         0)
        ((string-match "^[ \t]*start" (current-line))
         0)
        (t 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; general functions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-line ()
  (save-excursion
    (buffer-substring (save-excursion (beginning-of-line)
                                      (point))
                      (save-excursion (end-of-line)
                                      (1+ (point))))))
(defun string-length (str)
  (- (string-match "$" str) (string-match "^" str)))

(defun re-search-forward-without-moving-point (str)
  (save-excursion
    (re-search-forward str nil t)))

(defun re-search-backward-without-moving-point (str)
  (save-excursion
    (re-search-backward str nil t)))

(provide 'racc-mode)

;;; racc-mode.el ends here
