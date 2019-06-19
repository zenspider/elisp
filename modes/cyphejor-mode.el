(require 'cyphejor)

(setq cyphejor-rules '(:downcase
                       ("emacs"       "ε")
                       ("inferior"    "i"  :prefix)
                       ("interaction" "i"  :prefix)
                       ("interactive" "i"  :prefix)
                       ("lisp"        "λ"  :postfix)
                       ("mode"        "")
                       ("shell"       "sh" :postfix)
                       ))

(cyphejor-mode 1)

;; TODO: I hate that I need two modes to do one simple thing.
;; I MUCH prefer the approach above, but it doesn't do minor-modes. Extend?

(require 'delight)
(delight '((abbrev-mode            nil  "abbrev")
           (eldoc-mode             nil  "eldoc")
           (elisp-slime-nav-mode   nil  "elisp-slime-nav")
           (flyspell-mode          nil  "flyspell")
           (global-whitespace-mode nil  "whitespace")
           (outline-minor-mode     nil  "outline")
           (paredit-mode           nil  "paredit")
           (projectile-mode        nil  "projectile")
           ))
