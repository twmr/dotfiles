;;; hydra that runs helm-projectile-find-file in specified sandbox

(require 'hydra)
(require 'helm)
(require 'cl-lib) ;; cl-delete-if-not


(defun thi::hydra-project-find-file--generic (project-name)
  (interactive)
  (let ((default-directory project-name))
    (condition-case error
        ;; (helm-projectile-find-file) -- problems with emacs repo
        (projectile-find-file)
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

  ;; TODO caching of file list for the following projects
  ;; TODO speed up
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
