(defvar thi::directory-list '(
                              "~/gitrepos/dotfiles/emacs.d"
                              "~/gitrepos/dotfiles/emacs.d/thi"
                              ))

(require 'cl-lib)
(delete-if-not 'file-exists-p thi::directory-list)
