;; -*- truncate-lines: t -*-

;;                                                                                                 1                                                                                                   2
;;       1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7         8         9         0
;;34567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012

(defun interactive-split-current-window ()
  "Interactively split the current window, either horizontally or
vertically. The rule of thumb is that this function favors a
horizontal split, unless it would result in windows narrower than
the current fill-column."
  (interactive)
  (if (> (window-width) (* 2 fill-column))
      (split-window-horizontally)
    (split-window-vertically)))

(defun rwd/window-columns ()
  (/ (frame-width) 80))

;; TODO: keep poking at using split-width-threshold and friends on the
;; large monitor and decide if I need to override
;; split-window-sensibly to favor vertical splits. This is a start:

;; https://emacs.stackexchange.com/questions/20492/how-can-i-get-a-sensible-split-window-policy
(defun rwd/split-window-sensibly (&optional window)
    "replacement `split-window-sensibly' function which prefers vertical splits"
    (interactive)
    (let ((window (or window (selected-window))))
        (or (and (window-splittable-p window t)
                 (with-selected-window window
                     (split-window-right)))
            (and (window-splittable-p window)
                 (with-selected-window window
                     (split-window-below))))))


;; (require 'shackle)
;;
;; ;; (setq shackle-lighter "")
;; (setq shackle-select-reused-windows nil) ; default nil
;; (setq shackle-default-alignment 'below) ; default below
;; (setq shackle-default-size 0.4) ; default 0.5
;;
;; (setq shackle-rules
;;       ;; CONDITION(:regexp) :select :inhibit-window-quit :size+:align|:other :same|:popup
;;       '(
;;         ;; (compilation-mode :select nil)
;;         ;; ("*undo-tree*" :size 0.25 :align right)
;;         ;; ("*eshell*" :select t :other t)
;;         ;; ("*Shell Command Output*" :select nil)
;;         ;; ("\\*Async Shell.*\\*" :regexp t :ignore t)
;;         ;; (occur-mode :select nil :align t)
;;         ;; ("*Help*" :select t :inhibit-window-quit t :other t)
;;         ;; ("*Completions*" :size 0.3 :align t)
;;         ;; ("*Messages*" :select nil :inhibit-window-quit t :other t)
;;         ;; ("\\*[Wo]*Man.*\\*" :regexp t :select t :inhibit-window-quit t :other t)
;;         ;; ("\\*poporg.*\\*" :regexp t :select t :other t)
;;         ;; ("\\`\\*helm.*?\\*\\'" :regexp t :size 0.3 :align t)
;;         ;; ("*Calendar*" :select t :size 0.3 :align below)
;;         ;; ("*info*" :select t :inhibit-window-quit t :same t)
;;         ;; (magit-status-mode :select t :inhibit-window-quit t :same t)
;;         ;; (magit-log-mode :select t :inhibit-window-quit t :same t)
;;         ;; ("\\`\\*magit.*?\\*\\'" :regexp t :align t :size 0.4)
;;         )
;;       )
;;
;; (shackle-mode -1)

(require 'window-purpose)
(purpose-mode)

(setq rwd/purpose/3-col
      '(
        (comint-mode              . left)
        (eshell-mode              . left)
        (shell-mode               . left)
        (term-mode                . left)
        (emacs-lisp-mode          . middle)
        (enh-ruby-mode            . middle)
        (prog-mode                . middle)
        (text-mode                . middle)
        (Info-mode                . right)
        (compilation-mode         . right)
        (dired-mode               . right)
        (grep-mode                . right)
        (helpful-mode             . right)
        (magit-diff-mode          . right)
        (magit-log-mode           . right)
        (magit-status-mode        . right)
        (occur-mode               . right)
        ))

;; purpose-user-mode-purposes
;; purpose-user-name-purposes
;; purpose-user-regexp-purposes
;; purpose-use-default-configuration

(defun rwd/purpose/reset ()
  (interactive)

  (setq rwd/purpose/2-col
        (-map (-lambda ((mode . place))
                (cons mode (if (eq 'middle place) 'right place)))
              rwd/purpose/3-col))

  (setq purpose-user-mode-purposes rwd/purpose/3-col)

  (purpose-compile-user-configuration))

(rwd/purpose/reset)
