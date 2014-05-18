;; Added in the right order, they even work sequentially:

;; TODO merge this list with the list in projects.el!!!
(add-to-list 'sml/replacer-regexp-list '("^:DB:Documents" ":DDocs:"))
(add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:"))
(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/" ":Git:"))

(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/POC-scripts/branches/version2.0" ":psc:"))
(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/tools/alpha" ":A:"))
(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/tools/beta1" ":B1:"))
(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/tools/beta2" ":B2:"))
(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/tools/future" ":F:"))

(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/HWSimuEnv" ":HWSIM:"))
(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/imshelperscripts" ":IH:"))

(add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/dotfiles/emacs.d" ":ED:"))
(add-to-list 'sml/replacer-regexp-list '("^~/.emacs.d" ":ED:"))

(add-to-list 'sml/replacer-regexp-list '("^:Git:\\(.*\\)/src/main/java/" ":G/\\1/SMJ:"))
