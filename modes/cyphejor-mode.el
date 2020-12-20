(setq cyphejor-rules '(:downcase
                       ("emacs"       "ε")
                       ("inferior"    "i"  :prefix)
                       ("interaction" "i"  :prefix)
                       ("interactive" "i"  :prefix)
                       ("lisp"        "λ"  :postfix)
                       ("mode"        "")
                       ("shell"       "sh" :postfix)
                       ))

(with-eval-after-load 'cyphejor
  (cyphejor-mode 1))

;; TODO: I hate that I need two modes to do one simple thing.
;; I MUCH prefer the approach above, but it doesn't do minor-modes. Extend?

(setq delight-rules '((abbrev-mode            nil  "abbrev")
                      (dispwatch-mode         nil  "dispwatch")
                      (eldoc-mode             nil  "eldoc")
                      (elisp-slime-nav-mode   nil  "elisp-slime-nav")
                      (flyspell-mode          nil  "flyspell")
                      (gcmh-mode              nil  "gcmh")
                      (global-whitespace-mode nil  "whitespace")
                      (outline-minor-mode     nil  "outline")
                      (paredit-mode           nil  "paredit")
                      (projectile-mode        nil  "projectile")
                      ))

(with-eval-after-load 'delight
  (delight delight-rules))

;; TODO: remove delight and replace it with this:?
;; (cyphejor--cypher "elisp-slime-nav-mode" cyphejor-rules)

(when-idle rwd-idle-time
  (rwd-require 'cyphejor))
(when-idle rwd-idle-time
  (rwd-require 'delight))
