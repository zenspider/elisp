;; -*- lexical-binding: t; -*-

;; (add-to-list 'load-path "~/Work/git/dakra/ghostel/lisp")

(eval-when-compile
  (require 'ghostel))

(defun ghostel--keymap-set (key fn)
  ;; Input modes (`ghostel-semi-char-mode-map', `ghostel-char-mode-map',
  ;; `ghostel-readonly-mode-map', `ghostel-readonly-fast-exit-mode-map',
  ;; `ghostel-line-mode-map') inherit or extend this map."
  (dolist (map (list ghostel-mode-map ghostel-semi-char-mode-map))
    (message "setting %s to %s" key fn)
    (keymap-set map key fn)))

(defun ghostel-send-C-u ()
  "Send C-u, geneally clearing from point to the prompt."
  (interactive)
  (ghostel--ensure-ghostel-buffer)
  (ghostel--on-user-input)
  (ghostel-send-key "u" "ctrl"))

(defun ghostel-bob ()
  (interactive)
  (ghostel--ensure-ghostel-buffer)
  (ghostel-emacs-mode)                  ; you're triggering this to WATCH it run
  (goto-char (point-min)))

(defun ghostel-eob ()
  (interactive)
  (ghostel--ensure-ghostel-buffer)
  (ghostel-readonly-exit))

(defun ghostel-kill-line ()
  (interactive)
  (ghostel--ensure-ghostel-buffer)
  (ghostel--on-user-input)
  (kill-ring-save (ghostel-cursor-point) (line-end-position)) ; to emacs
  (ghostel-send-key "k" "ctrl"))        ; to shell kill ring

(defun ghostel-send-up ()
  (interactive)
  (ghostel--ensure-ghostel-buffer)
  (ghostel--on-user-input)
  (ghostel-send-key "up"))

(defun ghostel-send-down ()
  (interactive)
  (ghostel--ensure-ghostel-buffer)
  (ghostel--on-user-input)
  (ghostel-send-key "down"))

(with-eval-after-load 'ghostel
  ;; (add-to-list 'ghostel-keymap-exceptions "C-k" t)
  ;; (keymap-set ghostel-mode-map "C-k" #'kill-line)
  (ghostel--keymap-set         "C-k"     #'ghostel-kill-line)
  (ghostel--keymap-set         "M-<"     #'ghostel-bob)
  (ghostel--keymap-set         "M->"     #'ghostel-eob)
  (ghostel--keymap-set         "M-p"     #'ghostel-send-up)
  (ghostel--keymap-set         "M-n"     #'ghostel-send-down)
  (keymap-set ghostel-mode-map "C-c e"   #'ghostel-clear-scrollback)
  (keymap-set ghostel-mode-map "C-c C-u" #'ghostel-send-C-u)
  (keymap-set ghostel-mode-map "C-c u"   #'ghostel-send-C-u)
  (keymap-set ghostel-mode-map "C-x C-p" #'ghostel-find-file-at-point)

  (defalias 'ghostel-write-file #'comint-write-output))
