(projectile-global-mode)
(setq projectile-completion-system 'helm)

;; if project-persist-mode is not installed install it using package.el
(project-persist-mode 1) ;; C-c P n; C-c P f

(define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)

(defvar thi::directory-list '(
                              "~/gitrepos/POC-scripts/branches/version2.0"
                              "~/gitrepos/pyspu/branches/version0.8"

                              "~/gitrepos/tools"
                              "~/gitrepos/tools/future"
                              "~/gitrepos/tools/future/server_configs/coldfire"
                              "~/gitrepos/tools/alpha"
                              "~/gitrepos/tools/alpha/server_configs/coldfire"
                              "~/gitrepos/tools/beta1"
                              "~/gitrepos/tools/beta1/server_configs/coldfire"
                              "~/gitrepos/tools/beta2"
                              "~/gitrepos/tools/beta2/server_configs/coldfire"

                              "~/gitrepos/imshelperscripts"
                              "~/gitrepos/hwsimuenv"

                              "~/gitrepos/dotfiles/emacs.d"
                              "~/gitrepos/dotfiles/emacs.d/thi"
                              "~/gitrepos/dotfiles/emacs.d/el-get"
                              ))

(require 'cl)
(delete-if-not 'file-exists-p thi::directory-list)
