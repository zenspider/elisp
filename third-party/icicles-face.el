;;; icicles-face.el --- Faces for Icicles
;; 
;; Filename: icicles-face.el
;; Description: Faces for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:19:43 2006
;; Version: 22.0
;; Last-Updated: Fri Jun 30 07:12:08 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 89
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-face.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  This is a helper library for library `icicles.el'.  It defines
;;  faces.  See `icicles.el' for documentation.
;; 
;;  Faces defined here (in Custom group `icicles'):
;;
;;    `icicle-common-match-highlight-Completions',
;;    `icicle-complete-input', `icicle-current-candidate-highlight',
;;    `icicle-historical-candidate',
;;    `icicle-match-highlight-Completions',
;;    `icicle-match-highlight-minibuffer', `icicle-prompt-suffix',
;;    `icicle-search-current-input',
;;    `icicle-search-main-regexp-current',
;;    `icicle-search-main-regexp-others',
;;    `icicle-whitespace-highlight', `minibuffer-prompt'.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 2006/06/30 dadams
;;     Added: minibuffer-prompt for Emacs < 22 (e.g. Emacs 21.4 has propertize).
;; 2006/04/28 dadams
;;     Added: icicle-whitespace-highlight.
;; 2006/04/14 dadams
;;     Renamed icicle-search-refined-regexp to icicle-search-current-input.
;; 2006/04/07 dadams
;;     Added: icicle-search-main-regexp-others.
;;     Renamed: icicle-search-main-regexp to icicle-search-main-regexp-current.
;; 2006/03/27 dadams
;;     Added: icicle-search-*-regexp.
;; 2006/03/22 dadams
;;     Renamed: icicle-root-highlight-* to icicle-match-highlight-*.
;; 2006/03/21 dadams
;;     Added: icicle-common-match-highlight-Completions.
;;     icicle-root-highlight-Completions: Changed default face.
;; 2006/03/08 dadams
;;     Added: icicle-current-candidate-highlight.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Faces (alphabetical) -----------------------------------

(defgroup icicles nil
  "Minibuffer input completion and cycling of completion candidates."
  :prefix "icicle-"
  :group 'completion :group 'convenience :group 'help :group 'apropos
  :group 'dabbrev :group 'matching :group 'minibuffer :group 'recentf
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Commentary" "icicles")
  )

(defface icicle-common-match-highlight-Completions '((t (:foreground "magenta3")))
  "*Face used to highlight candiates common match, in *Completions*."
  :group 'icicles :group 'faces)

(defface icicle-complete-input '((t (:foreground "dark green")))
  "*Face used to highlight input when it is complete."
  :group 'icicles :group 'faces)

(defface icicle-Completions-instruction-1 '((t (:foreground "Blue")))
  "*Face used to highlight first line of *Completions* buffer."
  :group 'icicles :group 'faces)

(defface icicle-Completions-instruction-2 '((t (:foreground "Red")))
  "*Face used to highlight second line of *Completions* buffer."
  :group 'icicles :group 'faces)

(defface icicle-current-candidate-highlight '((t (:background "CadetBlue1")))
  "*Face used to highlight the current candidate, in *Completions*."
  :group 'icicles :group 'faces)

(defface icicle-historical-candidate '((t (:foreground "Blue")))
  "*Face used to highlight *Completions* candidates that have been used."
  :group 'icicles :group 'faces)

; Value is from `custom-button-pressed-face', with :foreground from `minibuffer-prompt'.
(defface icicle-prompt-suffix
    '((((type x w32 mac) (class color))
       (:box (:line-width 2 :style pressed-button) :foreground "dark blue"))
        ;;; :background "lightgrey" :foreground "black"
      (t (:inverse-video t)))
  "*Face used to highlight `icicle-prompt-suffix'."
  :group 'icicles :group 'faces)

(defface icicle-match-highlight-Completions '((t (:foreground "Red3")))
  "*Face used to highlight root that was completed, in *Completions*."
  :group 'icicles :group 'faces)

(defface icicle-match-highlight-minibuffer '((t (:underline t)))
  "*Face used to highlight root that was completed, in minibuffer."
  :group 'icicles :group 'faces)

(defface icicle-search-main-regexp-current '((t (:background "misty rose")))
  "*Face used to highlight current match of your original regexp."
  :group 'icicles :group 'faces)

(defface icicle-search-main-regexp-others '((t (:background "CadetBlue1")))
  "*Face used to highlight other matches of your original regexp.
If user option `icicle-search-highlight-all-flag' is nil, then this
face is not used."
  :group 'icicles :group 'faces)

(defface icicle-search-current-input '((t (:foreground "Black" :background "Green")))
  "*Face used to highlight what your current input matches."
  :group 'icicles :group 'faces)

(defface icicle-whitespace-highlight '((t (:background "Magenta")))
  "*Face used to highlight initial whitespace in minibuffer input."
  :group 'icicles :group 'faces)

;; This is defined in `faces.el', Emacs 22.  This is for Emacs < 22.  This is used
;; only for versions of Emacs that have `propertize' but don't have this face.
(unless (facep 'minibuffer-prompt)
  (defface minibuffer-prompt '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
    "Face for minibuffer prompts."
    :group 'basic-faces))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-face.el ends here
