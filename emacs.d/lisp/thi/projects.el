(projectile-global-mode)
(setq projectile-completion-system 'helm)

;; if project-persist-mode is not installed install it using package.el
(project-persist-mode 1) ;; C-c P n; C-c P f

(define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)

(defvar thi::directory-list '(
                              "~/gitrepos/dotfiles/emacs.d"
                              "~/gitrepos/dotfiles/emacs.d/thi"
                              "~/gitrepos/dotfiles/emacs.d/el-get"
                              ))

(require 'cl)
(delete-if-not 'file-exists-p thi::directory-list)
