;:::::::::::::::::::::::::::::::::::::::::::::::
;: Git Stuff
;:::::::::::::::::::::::::::::::::::::::::::::::

;; follow sylinks to source files in version controlled systems
(setq vc-follow-symlinks t)

(add-to-list 'load-path ".../git/contrib/emacs")
(require 'git)
;;(require 'git-blame)

(provide 'init-git-vc)
