;;; hydra that runs helm-projectile-find-file in specified sandbox

(require 'hydra)
(require 'helm)
(require 'cl-lib) ;; cl-delete-if-not, cl-loop

;; TODO sort results (using rg --sort), evaluate performance loss
(setq projectile-generic-command
      ;; -0 is needed for the \0 line termination.
      (concat
       "rg --files -0 "
       "-g '!site_scons' "
       "-g '!site_python' "
       "-g '!node_modules' "
       "-g '!ijscore' "
       "-g '!local_configdb' "))


(defun thi::hydra-project-find-file--generic (project-name)
  (interactive)
  (let ((default-directory project-name))
    (condition-case error
        ;; (helm-projectile-find-file) -- problems with emacs repo
        ;; TODO I need a projetile-find-dir version as well (sometimes I know the dirname but not the filename)
        ;; TODO if possible use ripgrep for projectile-find-file
        ;; -> (defun projectile-files-via-ext-command (root command)
        ;; -> customize projectile-generic-command
        (projectile-find-file)
      (error
       (message "%s: %s" project-name (error-message-string error))))))


(defun thi::sandbox-find-file (projectname)
  (interactive)
  (thi::hydra-project-find-file--generic (concat "~/sandbox/" projectname)))

(defun thi::temporary-find-file (shortname projectname)
  (interactive)
  `(,shortname (lambda ()
                 (interactive)
                 (thi::hydra-project-find-file--generic (concat "~/gitrepos/" ,projectname)))
               ,projectname))


(defmacro define-sandbox-hydra (name body docstring hexpr &rest extra-heads)
  "Define a custom hydra.
NAME, BODY and DOCSTRING are as in `defhydra'.  HEXPR is an
expression that is evaluated and should yield a list of hydra
heads.  EXTRA-HEADS are additional heads, which are not
evaluated."
  (let ((heads (eval hexpr)))
    `(defhydra ,name ,body
       ,docstring
       ,@heads
       ,@extra-heads)))


(defvar thi::project-hydra-mapping
  '(("d" . "drina")
    ("e". "erenik")
    ("f". "fiora")
    ("e". "gaia")
    ("3". "gaia_py3")
    ("S". "gaia_py3_stable")
    ("h". "huxley")
    ))


(defun thi::temporary-find-file (shortname projectname)
  (interactive)
  (let* ((projectdir (concat "~/sandbox/" projectname))
         (projectiledotfile (concat projectdir "/.projectile")))
    ;; TODO add .projectile files and dumbjump files
    (if (not (file-exists-p projectdir))
        (message "directory %s does not exists - skipping" projectdir)
      (unless (not (file-exists-p projectiledotfile))
        (message "projectilefile %s does not exist - Creating...!"
                 projectiledotfile)
        (write-regtion "" nil projectiledotfile)
      ))
    `(,shortname (lambda ()
                   (interactive)
                   (thi::hydra-project-find-file--generic projectdir))
                 ,projectname)))


(define-sandbox-hydra thi::hydra-project-find-file (:color blue)
  "find file in one of the selected projects"

  (cl-loop for (shortname . projectname) in thi::project-hydra-mapping
           collect
           (thi::temporary-find-file shortname projectname)))


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
   ;; (ivy-completing-read "Directory open: " thi::directory-list)))


(provide 'thi-projects)
;;; thi-projects.el ends here
