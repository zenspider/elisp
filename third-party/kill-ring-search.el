;;; kill-ring-search.el --- Incremental search for the kill ring

;; Copyright (C) 2006 Nikolaj Schumacher <bugs * nschum , de>

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Usage

;; Just call kill-ring-search and enter your search.  M-y and C-y work as usual.
;; To load the function add this the following to your .emacs file:

;; (autoload 'kill-ring-search "kill-ring-search"
;;  "Search the kill ring in the minibuffer."
;;  (interactive))

;; (global-set-key "\M-\C-y" 'kill-ring-search)

(require 'cl)

(defvar kill-ring-search-keymap
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map "\C-r" 'kill-ring-search-prev)
    (define-key map "\M-y" 'kill-ring-search-prev)
    (define-key map "\C-y" 'exit-minibuffer)
    map)
  "Keymap used inside the minibuffer by `kill-ring-search'.")

(defun kill-ring-search-prev ()
  "Return the previous match also matching the current `kill-ring-search'"
  (interactive)
  (let ((new (get-next-match (cdr (get-next-match kill-ring-search-pos)))))
    (if new
        (setq kill-ring-search-pos new)
      (beep)
      )))

(defvar kill-ring-search-pos nil
  "The remaining parts of the kill-ring to be searched by `search-kill-ring'.")

(defvar kill-ring-search-string nil
  "The current string searched for by `search-kill-ring'.")

(defvar kill-ring-search-calling-buffer nil
  "The buffer from which the current `search-kill-ring' originated.")

(defun get-next-match (search-list)
  "Search SEARCH-LIST for a match on `kill-ring-search-string'."
  (let ((ring search-list)
        (res nil))
    (while (and ring (null res))
      (if (search kill-ring-search-string (car ring))
          (setq res (car ring))
        (setq ring (cdr ring))))
    ring))

(defvar kill-ring-search-eoinput 1
  "Point where minibuffer input ends and completion info begins.")
(make-variable-buffer-local 'kill-ring-search-eoinput)

(defun kill-ring-search-pre-command ()
  "Remove the current `kill-ring-search' match before minibuffer input."
  (delete-region kill-ring-search-eoinput (point-max)))

(defun kill-ring-search-create-highlighted-match (string)
  "Return a copy of STRING that highlights the the `kill-ring-search'."
  (let ((res (copy-sequence string))
        (pos (search kill-ring-search-string string)))
    (when pos
      (add-text-properties pos (+ pos (length kill-ring-search-string))
                           (list 'face 'highlight) res))
    (or res "NO MATCH")))

(defun kill-ring-search-post-command ()
  "Display the current `kill-ring-search' match after minibuffer input occured."
  (let ((contents (buffer-substring (minibuffer-prompt-end) (point-max))))
    (setq kill-ring-search-eoinput (point-max))
    (save-excursion
      (goto-char (point-max))
      (setq kill-ring-search-string contents)
      (let ((match (get-next-match kill-ring-search-pos)))
        (unless match
          ;; reset, if nothing was found
          (setq kill-ring-search-pos kill-ring)
          (setq match (get-next-match kill-ring-search-pos)))
        (insert "\n" (kill-ring-search-create-highlighted-match
                      (car-safe match)))))))

(defun kill-ring-search-minibuffer-setup ()
  "Set up the minibuffer for `kill-ring-search' completions."
  (add-hook 'post-command-hook 'kill-ring-search-post-command nil t)
  (add-hook 'pre-command-hook 'kill-ring-search-pre-command nil t)
  (with-current-buffer kill-ring-search-calling-buffer
    (remove-hook 'minibuffer-setup-hook 'kill-ring-search-minibuffer-setup))
  (setq kill-ring-calling-buffer nil))

(defun kill-ring-search ()
  "Search the kill ring in the minibuffer."
  (interactive)
  (let ((minibuffer-local-completion-map kill-ring-search-keymap)
        (iswitchb-require-match t))
    (setq kill-ring-search-calling-buffer (current-buffer))
    (setq kill-ring-search-pos kill-ring)
    (setq kill-ring-search-string "")
    (add-hook 'minibuffer-setup-hook 'kill-ring-search-minibuffer-setup)
    (completing-read "Kill ring search: " '(("dummy" . 1)) nil nil nil nil)
    (let ((result (car-safe (get-next-match kill-ring-search-pos))))
      (when result (insert result)))
    (setq kill-ring-search-pos kill-ring)
    (setq kill-ring-search-string "")))

(provide 'kill-ring-search)