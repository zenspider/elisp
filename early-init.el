(unless (cdr command-line-args)
  (add-to-list 'default-frame-alist '(fullscreen . fullboth)))

(setq garbage-collection-messages t          ; indicator of thrashing
      gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6                 ; blah
      load-prefer-newer t                    ; ignore stale elc files
      package-quickstart t                   ; package+ to write quickstart
      )
