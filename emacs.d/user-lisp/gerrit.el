;;; gerrit.el --- Gerrit integration @ IMS -*- lexical-binding: t; -*-
;; my personal gerrit integration in emacs.
;;
;; (load 'gerrit)
;; (add-hook 'after-init-hook #'gerrit-load-lists)
;; (global-set-key (kbd "C-x i") 'gerrit-upload)
;; (global-set-key (kbd "C-x o") 'gerrit-download)

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

(defcustom gerrit-upload-max-saved-items 200
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
        (gerrit-dump-variable 'gerrit-upload-topic-history gerrit-upload-max-saved-items)
        (gerrit-dump-variable 'gerrit-upload-reviewers-history gerrit-upload-max-saved-items)
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
  "Interactively ask for space separated reviewers."
  (interactive)
  (gerrit-upload-completing-set "Reviewers (space separated): "
                                    gerrit-upload-reviewers-history
                                    gerrit-last-reviewers))

(defun gerrit-upload-set-topic ()
  "Interactively ask for a topic name."
  (interactive)
  (gerrit-upload-completing-set "Topic: "
                                    gerrit-upload-topic-history
                                    gerrit-last-topic))

(defun gerrit-upload-set-args ()
  "Interactively ask for arguments that are passed to git-review."
  (interactive)
  (gerrit-upload-completing-set "Args (space separated): "
                                    gerrit-upload-args-history
                                    gerrit-upload-args))

(defun gerrit-upload-create-git-review-cmd ()
  "Created cmdstr for git-review."
  (interactive)
  (let ((reviewers gerrit-last-reviewers)
        (topic gerrit-last-topic)
        (args gerrit-upload-args)
        (cmdstr "git review --yes"))
    (unless (equal "" topic)
      (setq cmdstr (concat cmdstr " -t " topic)))
    (unless (equal "" reviewers)
      (setq cmdstr (concat cmdstr " --reviewers " reviewers)))
    (unless (equal "" args)
      (setq cmdstr (concat cmdstr " " args)))
    cmdstr))

(defun gerrit-upload-run ()
  (interactive)
  (let ((cmdstr (gerrit-upload-create-git-review-cmd)))
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
gerrit-upload: (current cmd: %(concat (gerrit-upload-create-git-review-cmd)))
"
  ("r" gerrit-upload-add-reviewers "Add reviewers")
  ("t" gerrit-upload-set-topic "Set topic")
  ("a" gerrit-upload-set-args "Set additional args")
  ("RET" gerrit-upload-run "Run git-reivew" :color blue))

(defalias 'gerrit-upload 'hydra-gerrit-upload/body)

(defun gerrit-download ()
  "Download change from the gerrit server."
  (interactive)
  ;; TODO handle non-zero exit status (see https://stackoverflow.com/questions/23299314/finding-the-exit-code-of-a-shell-command-in-elisp)
  (let ((open-changes (shell-command-to-string "git review -l")))

    ;; remove last two lines
    (setq open-changes (nbutlast (s-lines open-changes) 2))
    ;; (message (s-join "\n" open-changes))
    (let ((changenr (ivy-completing-read
                     "Download Change: " open-changes nil nil)))
      (magit-git-command (concat "git review -d "
                                 (car (s-split " " (s-trim changenr))))))))

(defun foobar ()
  (interactive)
  (message "enterpressed"))

(defun foobar2 ()
  (interactive)
  (message "enterpressed (2)"))

;; only for heading
(defvar magit-open-reviews-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "jT" #'magit-todos-jump-to-todos)
    (define-key map "jl" #'magit-todos-list)
    (define-key map (kbd "RET") 'foobar)
    map)
  "Keymap for `magit-open-reviews' top-level section.")

;; for issues
(defvar magit-open-reviews-issue-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "jT" #'magit-todos-jump-to-todos)
    (define-key map "jl" #'magit-todos-list)
    (define-key map (kbd "RET") 'foobar2)
    map)
  "Keymap for `magit-open-reviews' top-level section.")

(defun magit-gerrit-insert-status ()
  (magit-insert-section (open-reviews)
    ;; the behavoir should be similar to "Recent commits"
    (magit-insert-heading "Open Gerrit Reviews")
    (dolist (loopvar `(1245 1249 1222 1234 0091))
      ;; (magit-insert-heading
      (progn
        (magit-insert-section (open-reviews-issue loopvar t)
          (magit-insert-heading
            (format (format "%%%ds (%%s) %%s\n" (1+ 4)) ;1+ accounts for #
                    (propertize (format "#%d" loopvar))
                    (propertize "version7.0" 'face '(:foreground "red"))
                    (propertize "foobar")))
          (insert "metadata1\n"))
        ;; (magit-insert-heading)
        ;; (insert "Metadata1: xxx\n")
        ;; (insert "Metadata2: yyy\n")
        ))
    (insert ?\n)
    ))


(defun magit-gerrit-insert-status2 ()
  "Insert information about current incremental merge."
  (when (magit-imerge-in-progress-p)
    (let* ((name (or (magit-imerge-current-name)
                     (error "No name, but in progress?")))
           (state (magit-imerge-state name))
           (format-with-overriding
            (lambda (option current)
              (let ((val (--some
                          (and (string-match (format "\\`%s=\\(.+\\)"
                                                     (regexp-quote option))
                                             it)
                               (match-string 1 it))
                          magit-imerge--arguments)))
                (if (and val (not (string= val current)))
                    (propertize val 'face 'magit-imerge-overriding-value)
                  current)))))
      (magit-insert-section (imerge)
        (magit-insert-heading "Incremental merge")
        (magit-insert-section (imerge-info)
          (insert (format "Name:   %s\n" name))
          (magit-insert-heading)
          (insert (format "Goal:   %s\n"
                          (funcall format-with-overriding
                                   "--goal"
                                   (cdr (assq 'goal state)))))
          (insert (format "Result: %s\n"
                          (funcall format-with-overriding
                                   "--branch"
                                   (cdr (assq 'branch state)))))
          (insert "Tips:   ")
          (magit-imerge--insert-tip (cdr (assq 'tip1 state)))
          (insert ", ")
          (magit-imerge--insert-tip (cdr (assq 'tip2 state)))
          (insert ?\n ?\n))
        (magit-insert-section (imerge-diagram)
          (magit-insert-heading
            (propertize "Diagram\n"
                        'face 'magit-section-secondary-heading))
          (insert
           (with-temp-buffer
             (magit-git-insert "imerge" "diagram" "--no-color" "--commits")
             (re-search-backward "^Key:")
             (delete-region (point) (point-max))
             (buffer-string))))))))



(defun magit-gerrit-insert-status3 (&optional type value)
  "Insert section showing recent commits.
Show the last `magit-log-section-commit-count' commits."
  (let* ((start (format "HEAD~%s" magit-log-section-commit-count))
         (range (and (magit-rev-verify start)
                     (concat start "..HEAD"))))

    (magit-insert-section (open-reviews)
      (magit-insert-heading "Open Gerrit Reviews")
      (magit-insert-section (longer)
        (insert-text-button
         (substitute-command-keys
          (format "Type \\<%s>\\[%s] to show more history"
                  'magit-log-mode-map
                  'magit-log-double-commit-limit))
         'action (lambda (_button)
                   (message "clicked"))
         ;; (magit-log-double-commit-limit))
         'follow-link t
         'mouse-face 'magit-section-highlight))

      (insert ?\n)
      (insert (format "l1\nl2 %s\n" range))

      (magit-insert-log range
                        (cons (format "-n%d" magit-log-section-commit-count)
                              (--remove (string-prefix-p "-n" it)
                                        magit-log-section-arguments))))))



;; (add-hook 'magit-status-sections-hook #'magit-gerrit-insert-status t)


(add-hook 'magit-status-sections-hook #'magit-gerrit-insert-status t)
(provide 'gerrit)
