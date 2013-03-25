(defvar thi::directory-list '(
                              "/opt/sandboxes"
                              "/opt/sandboxes/current-branch"
                              "/opt/sandboxes/poc4imo"

                              "~/gitrepos/POC-scripts/trunk/poclib"
                              "~/gitrepos/POC-scripts/trunk/tests"
                              "~/gitrepos/POC-scripts/trunk/integrationtests"

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
