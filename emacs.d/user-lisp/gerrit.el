;;; gerrit.el --- Gerrit integration @ IMS -*- lexical-binding: t; -*-
;; my personal gerrit integration in emacs.
;;
;; (load 'gerrit)
;; (add-hook 'after-init-hook #'gerrit-load-lists)
;; (global-set-key (kbd "C-x i") 'gerrit-upload)
;; (global-set-key (kbd "C-x o") 'gerrit-download)
;; (add-hook 'magit-status-sections-hook #'magit-gerrit-insert-status t)

;;; Code:

(require 'cl-lib)  ;; for remove-duplicates
(require 'hydra)
(require 'json)
(require 'magit)
(require 'recentf)
(require 's)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-vars)

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



(defun magit-open-reviews-open-gerrit-change()
  (interactive)
  (browse-url (format
               "https://%s/c/%s"
               ims-gerrit-host
               (s-chop-prefix "#"
                              ;; TOOD avoid using prin1-to-string?!?
                              (prin1-to-string (nth 0 (oref (magit-current-section) value)))))))
  ;; (message (prin1-to-string (nth 0 (oref (magit-current-section) value)))))

;;; TODOS:
;;; include votes in  open gerrit review lines
;;; press "ret" on line opens change in browser
;;; parse commit messages and show jira tickets (ret on jira tickets opens them)
;;;


;; for issues
(defvar magit-open-reviews-issue-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "jT" #'magit-todos-jump-to-todos)
    (define-key map "jl" #'magit-todos-list)
    (define-key map (kbd "RET") #'magit-open-reviews-open-gerrit-change)
    map)
  "Keymap for `magit-open-reviews' top-level section.")

(defun magit-gerrit--fetch-open-reviews ()
  "returns a sequence of (number branch topic subject)"
  (interactive)
  ;; we need the following information:
  ;; changenr, version, name, CR/V, assignee, topic, fixes/related ticket
  ;; sort by modification-date?
  (condition-case nil
      (mapcar (lambda (x) (seq-map (lambda (y) (cdr
                                  (assoc y (cdr x))))
                          (list '_number 'branch 'topic 'subject)))
              (gerrit-open-reviews-for-current-project))
    (error '())))

(defun magit-gerrit-insert-status ()
  (magit-insert-section (open-reviews)
    ;; the behavoir should be similar to "Recent commits"
    (magit-insert-heading "Open Gerrit Reviews")
    (dolist (loopvar (magit-gerrit--fetch-open-reviews))
      (progn
        (magit-insert-section (open-reviews-issue loopvar t)
          (magit-insert-heading
            (format (format "%%%ds %%%ds %%s" (1+ 4) 40)
                    (format "#%d" (nth 0 loopvar))
                    (concat
                     "("
                     (let ((topicname (nth 2 loopvar)))
                       (if (< 0 (length topicname))
                           (propertize (format "%s@" topicname) 'face '(:foreground "green"))
                         ""))
                     (propertize (nth 1 loopvar) 'face '(:foreground "red"))
                     ")")
                    (nth 3 loopvar)))
            ;; (propertize (format "#%d " loopvar)))

          ;; (insert (propertize "version7.0\n" 'face '(:foreground "red")))
          ;; (insert "metadata1\n")
          ;; (insert-button (format "fsf%d" loopvar)
          ;;                'action (lambda (x) (browse-url (button-get x 'url)))
          ;;                'url "http://www.fsf.org")

          ;; (insert ?\n)
          )

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

;; (let
;;     ((url "https://api.github.com/repos/thisch/pytest/contents/_pytest/warnings.py?ref=master")
;;      (url-request-extra-headers
;;       `(("Content-Type" . "application/json"))))
;;   (switch-to-buffer-other-window (url-retrieve-synchronously url))
;;   (goto-char url-http-end-of-headers)
;;   (when-let ((json-object-type 'hash-table)
;;            (json-key-type 'symbol)
;;            (result (json-read))
;;            (fields (map-elt result '_links))
;;            (self (map-elt fields 'self)))
;;     (erase-buffer)
;;     (insert self)))

(defcustom ims-gerrit-host "gerrit.rnd.ims.co.at"
  "hostname of the gerrit instance"
  :group 'ims-jira
  :type 'string)

(defun ims-gerrit-authentication ()
  "Return an encoded string with jira username and password."
  (let ((pass-entry (auth-source-user-and-password ims-gerrit-host)))

    (if-let ((username (nth 0 pass-entry))
             (password (nth 1 pass-entry)))
        (base64-encode-string
         (concat username ":" password)))))

(defun ims-gerrit-get-assignee (project version changeidnr)
  "Retrieve summary of TICKETID in jira."
  (let* ((url
          (concat "https://" ims-gerrit-host
                  "/config/server/version"))
                  ;; "/changes/?q=7730&o=ALL_REVISIONS"))
                  ;; "/changes/?q=7730&o=ALL_REVISIONS"))
         ;; (format "/changes/%s~%s~%d/assignee" project version changeidnr)))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Basic " (ims-gerrit-authentication)))))
         )

    ;; (message url)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (goto-char url-http-end-of-headers)))
    ;; (with-current-buffer (url-retrieve-synchronously url)
    ;;   (goto-char url-http-end-of-headers)
      ;; (if-let ((json-object-type 'hash-table)
      ;;          (json-key-type 'symbol)
      ;;          (result (json-read))
      ;;          (name (map-elt result 'name)))
      ;;     (message "Assignee is set to %s" name)))))

(defvar gerrit-rest-api-debug-flag nil
  "Flag needed for debugging problems with the rest API of gerrit.")

(defun gerrit-toggle-api-debug-flag ()
  "Toggle the internal debug flag."
  (interactive)
  (setq gerrit-rest-api-debug-flag (not gerrit-rest-api-debug-flag))
  (message "set debug flag to '%s'" gerrit-rest-api-debug-flag))

(defun gerrit-rest-sync (method data &optional path)
  "Interact with the API using method METHOD and data DATA.
Optional arg PATH may be provided to specify another location further
down the URL structure to send the request."
  (let ((url-request-method method)
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(concat "Basic " (ims-gerrit-authentication)))
           ))
        (url-request-data data)
        (target (concat "https://" ims-gerrit-host "/a" path)))

    (if (not gerrit-rest-api-debug-flag)
        (with-current-buffer (url-retrieve-synchronously target t)
          (let ((resp (json-read-from-string
                       (progn
                         (goto-char (point-min))
                         (buffer-substring (search-forward-regexp
                                            (concat "^" (regexp-quote ")]}'") "$"))
                                           (point-max))))))

            resp))
      (progn
        ;; TODO improve this, syntax highlight json code?
        (switch-to-buffer (url-retrieve-synchronously target))
        (goto-char (point-min))
        (insert target)
        (insert ?\n)))))


;; TODO write some testcases

(defun gerrit-get-server-version ()
  (interactive)
  (message (prin1-to-string (gerrit-rest-sync "GET" nil "/config/server/version"))))

(defun gerrit-get-topic-info (topicname)
  "Return information about an open topic"
  (interactive "sEnter a topic name: ")
  (let* ((fmtstr (concat "/changes/?q=is:open+topic:%s&"
                         "o=DOWNLOAD_COMMANDS&"
                         "o=CURRENT_REVISION&"
                         "o=CURRENT_COMMIT&"
                         "o=DETAILED_LABELS&"
                         "o=DETAILED_ACCOUNTS"))
         (req (format fmtstr topicname))
         ;; (req "/changes/software%2Fpocscripts~version7.0~I19b86aa77941d0301f2a836b8007a1a26c333090")
         (resp (gerrit-rest-sync "GET" nil req)))
    ;; (setq info-response resp)
    ;; (req "/changes/software%2Fpocscripts~version7.0~I19b86aa77941d0301f2a836b8007a1a26c333090"))
    ;; (req "/changes/?q=4900&o=ALL_REVISIONS"))
    (message "%s" (prin1-to-string resp))))

;; (ims-gerrit-get-assignee "software/elab" "version0.2" 7730)

;; (gerrit-get-topic-info "etssimuhwsimuenv")

(defun gerrit-get-current-project ()
  (interactive)
  (let* ((project (s-chop-suffix
                   ".git"
                   (nth 1 (s-split ":"
                                   (nth 0 (magit-config-get-from-cached-list "remote.origin.url")))))))
    project))


(defun gerrit-open-reviews-for-current-project ()
  (interactive)
  (let* ((project (gerrit-get-current-project))
         (json-array-type 'list)
         (req (format (concat "/changes/?q=is:open+project:%s&"
                              "o=DOWNLOAD_COMMANDS&"
                              "o=CURRENT_REVISION&"
                              "o=CURRENT_COMMIT&"
                              "o=DETAILED_LABELS&"
                              "o=DETAILED_ACCOUNTS")
                      (s-replace-all '(("/" . "%2F")) project)))
         ;; )
         (resp (gerrit-rest-sync "GET" nil req)))

    (setq open-reviews-response resp)
    resp))

    ;; (message "%s" (prin1-to-string (mapcar #'car resp)))))
    ;; (cdr (assoc 'subject (cdr (assoc 'commit (cdr (nth 0(cdr (assoc 'revisions (nth 0 open-reviews-response)))))))))
    ;; (message "%s" (prin1-to-string (nth 0 resp)))))

  ;; TODO get config from remote.origin.url and split string :
  ;; (message "%s" (nth 1 (s-split ":" (magit-config-get-from-cached-list "remote.origin.url")))))

;; (defun gerrit-open-reviews-for-current-project ()
;;   (interactive)
;;   (let* ((project (gerrit-get-current-project))
;;          (req (concat "/changes/?q=is:open+project:software%2Fhwsimuenv"))
;;          ;; )
;;          (resp (gerrit-rest-sync "GET" nil req)))

;;     (message "%s" (prin1-to-string req))))

;; (gerrit-get-info)

(provide 'gerrit)
