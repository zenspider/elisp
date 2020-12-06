(unless (cdr command-line-args)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(setq garbage-collection-messages t          ; indicator of thrashing
      gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6                 ; blah
      load-prefer-newer t                    ; ignore stale elc files
      ;; TODO: package-enable-at-startup nil       ; check w/ package--initialized
      ;; TODO: package-load-list '()
      ;; TODO: package--init-file-ensured t
      )
