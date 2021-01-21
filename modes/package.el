(add-to-list 'load-path
             (expand-file-name "~/Work/git/zenspider/package+"))

(require 'package+advice)               ; TODO: remove once published

(package+-disable-package-selected-packages)
