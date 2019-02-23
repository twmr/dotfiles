;; my personal git-review integration in magit.
;; *) download a change
;;   - download change metadata (number, branch, msg; see output of git-review -l) using "git review -l" or REST API and make the number make it possible to select changes
;; *) update current change
;;   - if you simply want to download the change again
;; TODO handle interactive uploads (typing yes when calling git-review)
;; *) upload for review
;;   - option for reviewer-names (with completion, sorted by most recently used)
;;   - option for topic-name (with completion (my created topicnames))
;;   -


;;; TODO
;; set assignee for git-review (cli) app,
;; assignee can not be set via git push options (https://gerrit-review.googlesource.com/Documentation/user-upload.html). Those push options are used when uploading a change via git-review.
;; take a look at pygerrit2 (github) - write small script, which sets assignee instead

;; -f is probably needed by some devs (the default for some devs)
;; show selected values in hydra

;; https://gerrit-review.googlesource.com/Documentation/rest-api-changes.html#add-reviewer

;;; Code:

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


(defun git-review-upload-change (topic reviewers)
  "Upload a change to gerrit using the git-review command line tool."
  (interactive (list
                (ivy-completing-read "Topic: "
                                     git-review-upload-topic-history
                                     nil nil nil nil
                                     ;; default value set to LRU topic name
                                     (car git-review-upload-topic-history)
                                     )
                (ivy-completing-read "Reviewers: "
                                     git-review-upload-reviewers-history
                                     nil nil nil nil
                                     ;; default value set to LRU reviewers value
                                     (car git-review-upload-reviewers-history)
                                     )))


  (let ((cmdstr "git review --yes"))
    (unless (equal "" topic)
      (push topic git-review-upload-topic-history)
      (setq git-review-upload-topic-history (remove-duplicates git-review-upload-topic-history :test 'string=))
      (setq cmdstr (concat cmdstr " -t " topic)))
    (unless (equal "" reviewers)
      (push reviewers git-review-upload-reviewers-history)
      (setq git-review-upload-reviewers-history (remove-duplicates git-review-upload-reviewers-history :test 'string=))
      (setq cmdstr (concat cmdstr " --reviewers " reviewers)))

    (git-review-save-lists)

    ;; (message cmdstr)))
    ;; TODO does not work if git review is interactive
    (magit-git-command cmdstr)))


(defmacro gerrit-completing-set (msg history &optional last)
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

(defun gerrit-add-reviewers nil
  (interactive)
  (gerrit-completing-set "Reviewers (space separated): "
                         git-review-upload-reviewers-history
                         git-review-last-reviewers))

(defun gerrit-set-topic nil
  (interactive)
  (gerrit-completing-set "Topic: "
                         git-review-upload-topic-history
                         git-review-last-topic))

(defun gerrit-set-args nil
  (interactive)
  (gerrit-completing-set "Args (space separated): "
                         git-review-upload-args-history
                         git-review-upload-args))


(defun gerrit-do-upload nil
  (interactive)
  ;; todo handle empty history
  (let ((reviewers git-review-last-reviewers)
        (topic git-review-last-topic)
        (args git-review-upload-args)
        (cmdstr "git review --yes"))
    (unless (equal "" topic)
      (setq cmdstr (concat cmdstr " -t " topic)))
    (unless (equal "" reviewers)
      (setq cmdstr (concat cmdstr " --reviewers " reviewers)))
    (unless (equal "" args)
      (setq cmdstr (concat cmdstr " " args " ")))

    (message cmdstr)))


(defhydra hydra-gerrit-upload (:color amaranth ;; foreign-keys warning, blue heads exit hydra
                               :hint nil ;; show hint in the echo area
                               :body-pre (progn
                                           (setq git-review-last-topic "")
                                           (setq git-review-last-reviewers "")
                                           (setq git-review-upload-args git-review-upload-default-args)))
  "gerrit-upload"
  ;; TODO show currently selected reviewers/topic/... in the hint message
  ;;
  ("r" gerrit-add-reviewers "Add reviewers")
  ("t" gerrit-set-topic "Set topic")
  ("a" gerrit-set-args "Set additional args")
  ("RET" gerrit-do-upload "Run git-reivew" :color blue))


(add-hook 'after-init-hook #'git-review-load-lists)


;;;;;;;;;;;;;;;;;;;;; OLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (global-set-key (kbd "C-x i") 'magit-review-popup)


(defun magit-review-upload-change-pu (&optional args)
  (interactive (list (magit-review-upload-arguments)))
  ;; (let (
  ;;       (review-args (string-join (magit-review-upload-arguments) " ")))
  ;;   (message (format "review args: %s" review-args))))
  (let ((review-args (magit-review-upload-arguments)))
    (message (format "review args: %s" (car review-args)))))


(magit-define-popup magit-review-upload-popup
  "Popup console for uploading a change."
  :man-page "git-review"
  :options  '(
              ;; add the possibility to cycle through a list of list of reviewers
              (?r "reviewers"                        "--reviewers=")

              (?t "topic"                            "--topic="))
  :actions  '((?u "upload"  magit-review-upload-change-pu)))


 ;; (magit-define-popup magit-review-popup
;;   "Popup console for review commands."
;;   :man-page "git-review"
;;   :actions  '("Download"
;;               (?d "change"  magit-review-download-change-pu)
;;               "Upload"
;;               (?u "upload"  magit-review-upload-change-pu)))


;; ;; add "Gerrit" action to magit-dispatch popup
;; (magit-define-popup-action 'magit-dispatch-popup ?g "Gerrit" 'magit-review-popup)

;; ;; is a magit-popup the correct way to implement for this?
;; (magit-define-popup magit-review-upload-popup
;;   "Popup console for uploading a change."
;;   :man-page "git-review"
;;   ;; :options  '(
;;   ;;             ;; add the possibility to cycle through a list of list of reviewers
;;   ;;             (?r "reviewers"                        "--reviewer=")

;;   ;;             (?t "topic"                            "--topic="))
;;   :actions  '((?u "upload"  magit-review-upload-change)))


;; ;; https://magit.vc/manual/magit-popup/Defining-Prefix-and-Suffix-Commands.html#Defining-Prefix-and-Suffix-Commands
(defun magit-review-download-change (change)
  ;; todo ivy like completion
  (interactive (list
                ;; todo default value: most recent change in current repo
                ;; (see help text of ivy-completing-read)
                (ivy-completing-read "Change #: "
                                     magit-review-download-change-history
                                     nil
                                     nil)))
  (magit-git-command (format "git review -d %s" change)))

;; (defun magit-review-upload-change2 ()
;;   (let (
;;         (review-args (string-join (magit-review-upload-arguments) " ")))
;;     (message (format "uploading change (%s)" review-args))


;;     (magit-git-command (format "git review %s" review-args))))



;; (defvar magit-review-completion-topic-history nil)
;; (defvar magit-review-completion-reviewers-history nil)

;; (defun mystuff(topic-name reviewers)
;;   (interactive (list
;;                 (ivy-read "Enter the topic name (optional): "
;;                           magit-review-completion-topic-history :sort nil)
;;                 (ivy-read "Enter the reviewers (optional): "
;;                           magit-review-completion-reviewers-history :sort nil)))
;;   (message (format "t %s r %s" topic-name reviewers)))


  ;; (unless (null 'topic-name)
  ;;   (message "topic defined"))
  ;; (unless (null 'reviewers)
  ;;   (message (format "rev defined %s" reviewers))))


    ;; ;;   (push topic-name (cdr (last magit-review-download-change-history))))
    ;; (message (format "cr returned %s" topic-name)))


;; (defun magit-gitreview-test ()
;;   (interactive)
;;   ;; (let* ((approver (cdr-safe (assoc 'by approval)))
;;   ;;  (approvname (cdr-safe (assoc 'name approver)))
;;   ;;  (approvemail (cdr-safe (assoc 'email approver)))
;;   ;;  (type (cdr-safe (assoc 'type approval)))
;;   ;;  (verified (string= type "Verified"))
;;   ;;  (codereview (string= type "Code-Review"))
;;   ;;  (score (cdr-safe (assoc 'value approval))))

;;   (magit-insert-section (gitreview)
;;     (insert "Hello World" "\n")))
;;     ;; (insert (magit-gerrit-pretty-print-reviewer approvname approvemail
;;     ;;                                             (and codereview score)
;;     ;;                                             (and verified score))
;;     ;;           "\n")))
;; ;; )

;; (remove-hook 'magit-status-sections-hook 'magit-gitreview-test)
