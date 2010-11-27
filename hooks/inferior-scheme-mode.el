(set (make-local-variable 'comint-output-filter-functions)
     '(comint-truncate-buffer comint-postoutput-scroll-to-bottom))

(set (make-local-variable 'comint-scroll-show-maximum-output) t)

(set (make-local-variable 'comint-scroll-to-bottom-on-output) t)
