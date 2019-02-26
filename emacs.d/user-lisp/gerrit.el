(require 'magit)
(require 'hydra)
(require 'recentf)

(defvar git-review-upload-topic-history nil "List of recently used topic names.")
(defvar git-review-upload-reviewers-history nil "List of recently used reviewers.")
(defvar git-review-upload-args-history nil "List of recently used args for git-review cmd.")

;; these two vars are many needed for the hydra-based implementation because
;; I don't know how I can communicate between different heads of the hydra
(defvar git-review-last-reviewers nil "...")
(defvar git-review-last-topic nil "...")
(defvar git-review-upload-args nil "...")

(defalias 'git-review-dump-variable 'recentf-dump-variable)
(defalias 'git-review-save-file-modes 'recentf-save-file-modes)

(defgroup git-review nil
  "Maintain a menu of recently opened files."
  :version "26.1"
  ;; which group should be used?
  :group 'files)

(defcustom git-review-download-max-saved-items 200
  "Maximum number of items of the git-review lists that will be saved.
A nil value means to save the whole lists."
  :group 'git-review
  :type 'integer)

(defcustom git-review-save-file (locate-user-emacs-file ".git-review")
  "File to save the recent lists into."
  :group 'git-review
  :type 'file)

(defun git-review-save-lists ()
  "Save the recent lists.
Write data into the file specified by `git-review-save-file'."
  (interactive)
  (condition-case error
      (with-temp-buffer
        (erase-buffer)
        (set-buffer-file-coding-system 'utf-8-emacs)
        (insert (format-message ";;; Automatically generated on %s.\n"
                                (current-time-string)))
        (git-review-dump-variable 'git-review-upload-topic-history git-review-download-max-saved-items)
        (git-review-dump-variable 'git-review-upload-reviewers-history git-review-download-max-saved-items)
        (insert "\n\n;; Local Variables:\n"
                ";; coding: utf-8-emacs\n"
                ";; End:\n")
        (let ((inhibit-message t))
          (write-file (expand-file-name git-review-save-file)))
        (set-file-modes git-review-save-file #o600)
        nil)
    (error
     (warn "git-review: %s" (error-message-string error)))))

(defcustom git-review-upload-default-args ""
  "Default args used when calling 'git review' to upload a change."
  :group 'git-review
  :type 'string)

(defun git-review-load-lists ()
  "Load a previously saved recent list.
Read data from the file specified by `git-review-save-file'."
  (interactive)

  (let ((file (expand-file-name git-review-save-file))
        ;; We do not want Tramp asking for passwords.
        (non-essential t))
    (when (file-readable-p file)
      (load-file file))))

(defmacro git-review-upload-completing-set (msg history &optional last)
  ;;; what if I want to enter only a substring ?
  ;;; https://github.com/abo-abo/swiper/pull/1049/files
  `(let ((value (ivy-completing-read
                  ,msg
                  ,history
                  nil nil nil nil
                  ;; default value set to LRU reviewers value
                  (car ,history)
                  )))
     (unless (null ,last)
       (setq ,last value))
    (unless (equal "" value)
      ;; todo simplify the duplicate handling
      (push value ,history)
      (setq ,history (remove-duplicates ,history :test 'string=)))))

(defun git-review-upload-add-reviewers ()
  (interactive)
  (git-review-upload-completing-set "Reviewers (space separated): "
                                    git-review-upload-reviewers-history
                                    git-review-last-reviewers))

(defun git-review-upload-set-topic ()
  (interactive)
  (git-review-upload-completing-set "Topic: "
                                    git-review-upload-topic-history
                                    git-review-last-topic))

(defun git-review-upload-set-args ()
  (interactive)
  (git-review-upload-completing-set "Args (space separated): "
                                    git-review-upload-args-history
                                    git-review-upload-args))

(defun git-review-upload-current-cmd-args ()
  (interactive)
  (let ((reviewers git-review-last-reviewers)
        (topic git-review-last-topic)
        (args git-review-upload-args)
        (cmdstr "--yes"))
    (unless (equal "" topic)
      (setq cmdstr (concat cmdstr " -t " topic)))
    (unless (equal "" reviewers)
      (setq cmdstr (concat cmdstr " --reviewers " reviewers)))
    (unless (equal "" args)
      (setq cmdstr (concat cmdstr " " args)))
    cmdstr))

(defun git-review-upload-run ()
  (interactive)
  (let ((cmdstr (concat "git review " (git-review-upload-current-cmd-args))))
    ;; (message cmdstr)
    (magit-git-command cmdstr)))

(defhydra hydra-git-review-upload (:color amaranth ;; foreign-keys warning, blue heads exit hydra
                               :hint nil ;; show hint in the echo area
                               :columns 1
                               :body-pre (progn
                                           (setq git-review-last-topic "")
                                           (setq git-review-last-reviewers "")
                                           (setq git-review-upload-args git-review-upload-default-args)))
  "
gerrit-upload: (current args: %(concat (git-review-upload-current-cmd-args)))
"
  ("r" git-review-upload-add-reviewers "Add reviewers")
  ("t" git-review-upload-set-topic "Set topic")
  ("a" git-review-upload-set-args "Set additional args")
  ("RET" git-review-upload-run "Run git-reivew" :color blue))

(defun gerrit-download ()
  "Download change from the gerrit server."
  (interactive)
  ;; todo download all changenr for current project

  (let ((changenr (ivy-completing-read
                  "Change NR: " nil nil nil)))
    ;; (message changenr)))
    (magit-git-command (concat "git review -d " changenr))))


(add-hook 'after-init-hook #'git-review-load-lists)
(global-set-key (kbd "C-x i") 'hydra-git-review-upload/body)

(provide 'gerrit)
