;; Added in the right order, they even work sequentially:

;; TODO merge this list with the list in projects.el!!!
(add-to-list 'sml/replacer-regexp-list '("^:DB:Documents" ":DDocs:"))
(add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:"))
(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/" ":Git:"))

(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/netgen/netgen" ":ng:"))
(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/ngsolve/ngsolve" ":ns:"))

(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/dotfiles/emacs.d" ":ED:"))
(add-to-list 'sml/replacer-regexp-list '("^~/.emacs.d" ":ED:"))

(add-to-list 'sml/replacer-regexp-list '("^:Git:\\(.*\\)/src/main/java/" ":G/\\1/SMJ:"))
