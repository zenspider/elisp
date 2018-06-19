(require 'ag)
(unless (equalp (symbol-function 'ag) 'ag-regexp) ; don't let this run twice
  (message "rewiring ag")
  (fset 'ag-literal (symbol-function 'ag))
  (defalias 'ag 'ag-regexp))
