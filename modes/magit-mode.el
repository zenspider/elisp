;; from http://www.ogre.com/node/447

(defun git-grep (search)
  "git-grep the entire current repo"
  (interactive
   (list (completing-read "Search for: " nil nil nil (current-word))))
  (grep-find (concat "git --no-pager grep -P -n "
                     search
                     " `git rev-parse --show-toplevel`")))
