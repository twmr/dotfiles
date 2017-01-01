(projectile-global-mode)

;; if project-persist-mode is not installed install it using package.el
(project-persist-mode 1) ;; C-c P n; C-c P f

(define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)

;; (setq projectile-switch-project-action 'projectile-find-dir)
;; With this setting, once you have selected your project, you will remain in Projectile's completion system to select a sub-directory of your project, and then that sub-directory is opened for you in a dired buffer. If you use this setting, then you will probably also want to set
(setq projectile-find-dir-includes-top-level t)


(defvar thi::directory-list '(
                              "~/gitrepos/dotfiles/emacs.d"
                              "~/gitrepos/dotfiles/emacs.d/thi"
                              ))

(require 'cl-lib)
(delete-if-not 'file-exists-p thi::directory-list)
