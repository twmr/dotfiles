;;; hydra that runs helm-projectile-find-file in specified sandbox

(require 'hydra)
(require 'helm)
(require 'cl-lib) ;; cl-delete-if-not


(defun thi::hydra-project-find-file--generic (project-name)
  (interactive)
  (let ((default-directory project-name))
    (condition-case error
        ;; (helm-projectile-find-file) -- problems with emacs repo
        ;; TODO I need a projetile-find-dir version as well (sometimes I know the dirname but not the filename)
        ;; TODO if possible use ripgrep for projectile-find-file
        ;; -> (defun projectile-files-via-ext-command (root command)
        ;; -> customize projectile-generic-command


        (let ((projectile-generic-command
               ;; -0 is needed for the \0 line termination.
               (concat
                "rg --files -0 "
                "-g '!site_scons' "
                "-g '!site_python' "
                "-g '!node_modules' "
                "-g '!ijscore' "
                "-g '!local_configdb' ")))
          (projectile-find-file))
      (error
       (message "%s: %s" project-name (error-message-string error))))))


;; TODO use projectile-switch-project instead?
(defhydra thi::hydra-project-find-file (:color blue)
  "find file in one of the selected projects"
  ("D" (lambda ()
         (interactive)
         (thi::hydra-project-find-file--generic "~/gitrepos/dotfiles")) "dotf")
  ("E" (lambda ()
         (interactive)
         (thi::hydra-project-find-file--generic "~/gitrepos/emacs")) "emacs")

  ;; NOTE: All sandboxes need to have a .projectile file!!
  ;;       maybe add a check?
  ;; TODO caching of file list for the following projects? Maybe it is not needed
  ("d" (lambda ()
         (interactive)
         (thi::hydra-project-find-file--generic "~/sandbox/drina")) "drina")
  ("f" (lambda ()
         (interactive)
         (thi::hydra-project-find-file--generic "~/sandbox/fiora")) "fiora")
  ("g" (lambda ()
         (interactive)
         (thi::hydra-project-find-file--generic "~/sandbox/gaia")) "gaia")
  ("h" (lambda ()
         (interactive)
         (thi::hydra-project-find-file--generic "~/sandbox/huxley")) "huxley")
)

(defvar thi::directory-list
  (cl-delete-if-not
   'file-exists-p
   (mapcar (lambda (path)
             (replace-regexp-in-string "~" (getenv "HOME") path))
           '("~/.zsh.d"
             "~/.zsh"
             "~/gitrepos/dotfiles/emacs.d"
             "~/gitrepos/dotfiles/emacs.d/user-lisp"
             ))))

(defun thi::directorychooser ()
  "Use helm to select a recently used directory from the `thi::directory-list'."
  (interactive)
  (dired
   (helm-comp-read "Directory open:" thi::directory-list :fuzzy t)))


(provide 'thi-projects)
;;; thi-projects.el ends here
