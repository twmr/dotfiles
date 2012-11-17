(defvar thi::directory-list '("~/gitrepos/dotfiles/emacs.d"
                              "~/gitrepos/dotfiles/emacs.d/thi"
                              "~/gitrepos/dotfiles/emacs.d/el-get"
                              "/opt/sandboxes"
                              "/opt/sandboxes/trunk"
                              "/opt/sandboxes/poc4imo"
                              "~/gitrepos/imshelperscripts"
                              "~/gitrepos/HWSimuEnv"
                              "~/gitrepos/tools"
                              "~/gitrepos/POC-scripts/branches/POC4IMO"
                              "~/gitrepos/pocscripts/branches/POC4IMO"
                              "/opt/imsrepos"
                              "/opt/imsblobs"
                              ))

(require 'cl)
(delete-if-not 'file-exists-p thi::directory-list)
