;; my personal gerrit integration in emacs.
;;
;; (load 'gerrit)
;; (add-hook 'after-init-hook #'gerrit-load-lists)
;; (global-set-key (kbd "C-x i") 'hydra-upload)

;;; Code:

(require 'cl-lib)  ;; for remove-duplicates
(require 'magit)
(require 'hydra)
(require 'recentf)
(require 's)

(defvar gerrit-upload-topic-history nil "List of recently used topic names.")
(defvar gerrit-upload-reviewers-history nil "List of recently used reviewers.")
(defvar gerrit-upload-args-history nil "List of recently used args for git-review cmd.")

;; these two vars are many needed for the hydra-based implementation because
;; I don't know how I can communicate between different heads of the hydra
(defvar gerrit-last-reviewers nil "...")
(defvar gerrit-last-topic nil "...")
(defvar gerrit-upload-args nil "...")

(defalias 'gerrit-dump-variable 'recentf-dump-variable)
(defalias 'gerrit-save-file-modes 'recentf-save-file-modes)

(defgroup gerrit nil
  "Maintain a menu of recently opened files."
  :version "26.1"
  ;; which group should be used?
  :group 'files)

(defcustom gerrit-download-max-saved-items 200
  "Maximum number of items of the gerrit lists that will be saved.
A nil value means to save the whole lists."
  :group 'gerrit
  :type 'integer)

(defcustom gerrit-save-file (locate-user-emacs-file ".git-review")
  "File to save the recent lists into."
  :group 'gerrit
  :type 'file)

(defun gerrit-save-lists ()
  "Save the recent lists.
Write data into the file specified by `gerrit-save-file'."
  (interactive)
  (condition-case error
      (with-temp-buffer
        (erase-buffer)
        (set-buffer-file-coding-system 'utf-8-emacs)
        (insert (format-message ";;; Automatically generated on %s.\n"
                                (current-time-string)))
        (gerrit-dump-variable 'gerrit-upload-topic-history gerrit-download-max-saved-items)
        (gerrit-dump-variable 'gerrit-upload-reviewers-history gerrit-download-max-saved-items)
        (insert "\n\n;; Local Variables:\n"
                ";; coding: utf-8-emacs\n"
                ";; End:\n")
        (let ((inhibit-message t))
          (write-file (expand-file-name gerrit-save-file)))
        (set-file-modes gerrit-save-file #o600)
        nil)
    (error
     (warn "gerrit: %s" (error-message-string error)))))

(defcustom gerrit-upload-default-args ""
  "Default args used when calling 'git review' to upload a change."
  :group 'gerrit
  :type 'string)

(defun gerrit-load-lists ()
  "Load a previously saved recent list.
Read data from the file specified by `gerrit-save-file'."
  (interactive)

  (let ((file (expand-file-name gerrit-save-file))
        ;; We do not want Tramp asking for passwords.
        (non-essential t))
    (when (file-readable-p file)
      (load-file file))))

(defmacro gerrit-upload-completing-set (msg history &optional last)
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

(defun gerrit-upload-add-reviewers ()
  (interactive)
  (gerrit-upload-completing-set "Reviewers (space separated): "
                                    gerrit-upload-reviewers-history
                                    gerrit-last-reviewers))

(defun gerrit-upload-set-topic ()
  (interactive)
  (gerrit-upload-completing-set "Topic: "
                                    gerrit-upload-topic-history
                                    gerrit-last-topic))

(defun gerrit-upload-set-args ()
  (interactive)
  (gerrit-upload-completing-set "Args (space separated): "
                                    gerrit-upload-args-history
                                    gerrit-upload-args))

(defun gerrit-upload-current-cmd-args ()
  (interactive)
  (let ((reviewers gerrit-last-reviewers)
        (topic gerrit-last-topic)
        (args gerrit-upload-args)
        (cmdstr "--yes"))
    (unless (equal "" topic)
      (setq cmdstr (concat cmdstr " -t " topic)))
    (unless (equal "" reviewers)
      (setq cmdstr (concat cmdstr " --reviewers " reviewers)))
    (unless (equal "" args)
      (setq cmdstr (concat cmdstr " " args)))
    cmdstr))

(defun gerrit-upload-run ()
  (interactive)
  (let ((cmdstr (concat "git review " (gerrit-upload-current-cmd-args))))
    ;; (message cmdstr)
    (magit-git-command cmdstr)))

(defhydra hydra-gerrit-upload (:color amaranth ;; foreign-keys warning, blue heads exit hydra
                               :hint nil ;; show hint in the echo area
                               :columns 1
                               :body-pre (progn
                                           (setq gerrit-last-topic "")
                                           (setq gerrit-last-reviewers "")
                                           (setq gerrit-upload-args gerrit-upload-default-args)))
  "
gerrit-upload: (current args: %(concat (gerrit-upload-current-cmd-args)))
"
  ("r" gerrit-upload-add-reviewers "Add reviewers")
  ("t" gerrit-upload-set-topic "Set topic")
  ("a" gerrit-upload-set-args "Set additional args")
  ("RET" gerrit-upload-run "Run git-reivew" :color blue))

(defalias 'gerrit-upload 'hydra-gerrit-upload/body)

(defun gerrit-download ()
  "Download change from the gerrit server."
  (interactive)
  (let ((open-changes (shell-command-to-string "git review -l")))

    ;; remove last two lines
    (setq open-changes (nbutlast (s-lines open-changes) 2))
    ;; (message (s-join "\n" open-changes))
    (let ((changenr (ivy-completing-read
                     "Download Change: " open-changes nil nil)))
      (magit-git-command (concat "git review -d "
                                 (car (s-split " " (s-trim changenr))))))))

(provide 'gerrit)
