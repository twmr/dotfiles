;;; thi-projects.el --- project related stuff -*- lexical-binding:t -*-

;;; Commentary:

;;; hydra that runs helm-projectile-find-file in specified sandbox

;;; Code:

(require 'hydra)
(require 'helm)
(require 'cl-lib) ;; cl-delete-if-not, cl-loop
(require 'projectile)
(require 'f)
(require 's)
(require 'dash)

;; TODO sort results (using rg --sort), evaluate performance loss
(setq projectile-generic-command
      ;; -0 is needed for the \0 line termination.
      (concat
       "rg --files -0 "
       "-g '!netlib-plugins/Libs/devlib' "
       "-g '!tools/aps_pattern' "
       "-g '!config/aps_pattern' "
       "-g '!tools/structure_files' "
       "-g '!config/structure_files' "
       "-g '!node_modules' "
       "-g '!ijscore' "
       "-g '!local_configdb' "))

(defun thi::get-project-root ()
  "Parse find root of the current sandbox."
  (let* ((sandboxcfgname ".sandbox.cfg")
         (path (locate-dominating-file default-directory
                                       (lambda (dir)
                                         (f-exists? (f-join dir sandboxcfgname))))))
    path))

(defun thi::project-info ()
  ;; output number of files matched by projectile-generic-command
  (interactive)
  ;; TODO
  ;; output project root
  ;; output ...
  (message "Number of (tracked) files in the project: %s"
           (let ((default-directory (thi::get-project-root)))
                 (shell-command-to-string
                  (concat
                   (s-replace " -0" "" projectile-generic-command)
                   " | wc -l")))))

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
    ("f". "fiora")
    ("g". "gaia")
    ("h". "huxley")
    ("i". "isar")
    ("j". "jackson")
    ("k". "kalix")
    ("p". "pltb_simu")
    ))


(defun thi::temporary-find-file (shortname projectname)
  (interactive)
  (let* ((projectdir (concat "~/sandbox/" projectname))
         (projectiledotfile (concat projectdir "/.projectile")))
    ;; add .projectile files and dumbjump files
    (if (not (file-exists-p projectdir))
        (message "directory %s does not exist - skipping" projectdir)
      (message "directory %s exists" projectdir)
      (unless (file-exists-p projectiledotfile)
        (message "projectilefile %s does not exist - Creating...!"
                 projectiledotfile)
        (write-region "" nil projectiledotfile))

      ;; loop over all directories in projectdir that contain a .git directory
      (let ((git-repos (-filter (lambda (repo)
                                  (f-directory? (concat repo "/.git")))
                                (f-directories projectdir))))
        ;; create a .dumbjumpignore file in all git repos in the sandbox
        (mapc (lambda (repo)
                (unless (file-exists-p (concat repo "/.dumbjumpignore"))
                  (message "creating .dumbjump file in %s" repo)
                  (write-region "" nil (concat repo "/.dumbjumpignore"))))
              git-repos))
      )
    `(,shortname (lambda ()
                   (interactive)
                   (thi::hydra-project-find-file--generic ,projectdir))
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
             "~/gitrepos/dotfiles"
             "~/gitrepos/dotfiles_private"
             "~/gitrepos/dev"
             "~/gitrepos/software-tests"
             "~/gitrepos/dotfiles/emacs.d"
             "~/gitrepos/dotfiles/emacs.d/user-lisp"
             ))))

(defun thi::directorychooser ()
  "Use helm to select a recently used directory from the `thi::directory-list'."
  (interactive)
  (dired
   (helm-comp-read "Directory open:" thi::directory-list :fuzzy t)))
;; (ivy-completing-read "Directory open: " thi::directory-list)))


(with-eval-after-load 'smart-mode-line
  (add-to-list 'sml/replacer-regexp-list '("^~/Nextcloud/" ":NC:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/" ":Git:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/gitrepos/dotfiles/emacs.d" ":ED:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/.emacs.d" ":ED:"))

  (dolist (setting thi::project-hydra-mapping)
    (add-to-list 'sml/replacer-regexp-list (list
                                            (concat "^~/sandbox/" (cdr setting))
                                            (concat ":" (car setting) ":"))))
)

(defun thi::dev-parse-containername ()
  "Parse sandbox.cfg file and return containername."
  (when-let ((path (thi::get-project-root)))
      ;; TODO error out if path is nil
      (with-temp-buffer
        (insert-file-contents (f-join path sandboxcfgname))
        (keep-lines "containername" (point-min) (point-max))
        (when (string-match "containername = \\(.*\\)" (buffer-string))
          (match-string 1 (buffer-string))))))


(defun thi::dev-find-file-in-docker-container ()
  (interactive)
  (let ((user "root"))
    ;; TODO check if container is running
    (counsel-find-file (format "/docker:%s@%s:/usr/"
                               user
                               (thi::dev-parse-containername)))))

(defun thi::temporary-reopen-file-different-sandbox (projectname)
  (interactive)
  (let ((newfname
         ;; TODO check if replace-regexp-in-string did a replace
         ;; TODO check if the file exists in the desired sandbox
         (replace-regexp-in-string "\\(.*\\)/sandbox/\\([-0-9a-zA-z]*\\)/\\(.*\\)"
                                   (format "\\1/sandbox/%s/\\3" projectname)
                                   (buffer-file-name))))
    (message "reopening %s as %s" (buffer-file-name) newfname)
    (find-file newfname)))

(define-sandbox-hydra thi::hydra-dev-reopen-file-different-sandbox (:color blue
                                                                    :hint nil ;; show hint in the echo area
                                                                    )
  "
open %(concat (buffer-file-name)) in different sandbox

"
  (cl-loop for (shortname . projectname) in thi::project-hydra-mapping
           collect
           `(,shortname (lambda ()
                          (interactive)
                          (thi::temporary-reopen-file-different-sandbox ,projectname))
                        ,projectname)))

(provide 'thi-projects)

;;; thi-projects.el ends here
