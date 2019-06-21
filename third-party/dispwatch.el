;;; dispwatch.el --- watch displays

;; Copyright (C) 2018 Mitchell Perilstein

;; Author: Mitchell Perilstein <mitchell.perilstein@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package watches the current display geometry (pixel width and height) and gives your
;; hook a call if it changes.  Intended use case is plugging/unplugging a monitor.
;;
;; Usage
;;
;; Require or use-package this. Make a hook function which takes one argument, a new display
;; string like "1024x768". Add your hook to `dispwatch-display-change-hooks'. You will get
;; called when that changes, eg by removing or adding a monitor.  Then call `dispwatch-enable'
;; to get started and `dispwatch-disable' to stop.
;;
;; Example
;;
;; (defun my-display-changed-hook (disp)
;;   (cond ((equalp disp "3840x1080")   ; laptop + ext monitor
;;       (setq font-size-pt 10))
;;      ((equalp disp "1920x1080")      ; just laptop
;;       (setq font-size-pt 12))))
;;
;; (add-to-list 'load-path (expand-file-name "~/prj/dispwatch/"))
;;
;; (use-package dispwatch
;;   :config (progn
;;        (add-hook 'dispwatch-display-change-hooks #'my-display-changed-hook)
;;        (dispwatch-enable)))

;;; Code:

(defconst dispwatch-interval 5
  "Frequency to check dispaly, in seconds. Checking operation does not shell out of Emacs so
there isn't much penalty.")

(defvar dispwatch-timer nil)

(defvar dispwatch-current-display nil)

(defvar dispwatch-display-change-hooks nil
  "List of functions which take one argument: the new display geometry string (WIDTHxHEIGHT).
These hooks are run when a display change is detected.")

(defun dispwatch-enable ()
  "Enable display reconfiguration detection."
  (interactive)
  (setq dispwatch-current-display (dispwatch--get-display))
  (setq dispwatch-timer (run-at-time dispwatch-interval dispwatch-interval #'dispwatch--check-display))
  (message "dispwatch enabled"))

(defun dispwatch-disable ()
  "Disable display reconfiguration detection."
  (interactive)
  (cancel-timer dispwatch-timer)
  (setq dispwatch-timer nil)
  (message "dispwatch disabled"))

(defun dispwatch--get-display()
  "Current display, as a string WxH. It's a string so we can use it as
an alist key elsewhere."
  (format "%dx%d" (display-pixel-width) (display-pixel-height)))

(defun dispwatch--check-display()
  "Did it change? Run hooks if so."
  (let ((new (dispwatch--get-display)))
    (if (not (equal new dispwatch-current-display))
        (progn
          (setq dispwatch-current-display new)
          (run-hook-with-args 'dispwatch-display-change-hooks dispwatch-current-display)))))

(provide 'dispwatch)
