(defvar thi::directory-list '(
                              "/opt/sandboxes"
                              "/opt/sandboxes/current_branch"
                              "/opt/sandboxes/poc4imo"
                              "/opt/sandboxes/trunk"

                              "~/gitrepos/POC-scripts/trunk"

                              "~/gitrepos/tools"
                              "~/gitrepos/tools/future-tools"

                              "~/gitrepos/imshelperscripts"
                              "~/gitrepos/HWSimuEnv"

                              "~/gitrepos/dotfiles/emacs.d"
                              "~/gitrepos/dotfiles/emacs.d/thi"
                              "~/gitrepos/dotfiles/emacs.d/el-get"
                              ))

(require 'cl)
(delete-if-not 'file-exists-p thi::directory-list)
