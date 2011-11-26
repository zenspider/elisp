;;;###autoload
(progn
  (autoload 'blank-mode      "blank-mode" "doco" t)
  (autoload 'pastebin        "pastebin"   "doco" t)
  (autoload 'pastebin-buffer "pastebin"   "doco" t)
  (autoload 'align-cols (expand-file-name "~/Sites/emacs/elisp/align.el") "doco" t)

  (autoload 'lua-mode        "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
)
