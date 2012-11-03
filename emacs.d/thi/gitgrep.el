;; There's something similar (but fancier) in vc-git.el: vc-git-grep
;; -I means don't search through binary files
(defcustom git-grep-switches "--extended-regexp -I -n --ignore-case"
  "Switches to pass to `git grep'."
  :type 'string)

(defun git-grep (command-args)
  (interactive
   (list (read-shell-command "Run git-grep (like this): "
                             (format "git grep %s -e "
                                     git-grep-switches)
                             'git-grep-history)))
  (let ((grep-use-null-device nil))
    ;; (grep (concat "PAGER=cat " command-args))))
    (grep command-args)))
