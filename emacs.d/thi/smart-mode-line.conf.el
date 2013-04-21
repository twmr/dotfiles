;; Added in the right order, they even work sequentially:
(add-to-list 'sml/replacer-regexp-list '("^:DB:Documents" ":DDocs:"))
(add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:"))
(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/" ":Git:"))
(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/POC-scripts/trunk" ":pst:"))
(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/POC-scripts/branches/POC4IMO" ":psb"))
(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/dotfiles/emacs.d" ":ED:"))
(add-to-list 'sml/replacer-regexp-list '("^~/.emacs.d" ":ED:"))

(add-to-list 'sml/replacer-regexp-list '("^:Git:\\(.*\\)/src/main/java/" ":G/\\1/SMJ:"))
