;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tab-width 4
      indent-tabs-mode t
      truncate-partial-width-windows nil
      visible-bell t
      find-file-visit-truename t
)

(if (string= (getenv "DOMAIN") "amazon.com")
    (setq lpr-switches '("-Pcow")))

(if (featurep 'xemacs)
    (setq isearch-highlight t)
  (setq search-highlight t))

; HACK (set-glyph-image modeline-pointer-glyph "leftbutton")

