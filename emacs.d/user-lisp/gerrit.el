;;; gerrit.el --- Gerrit integration @ IMS -*- lexical-binding: t; -*-

;; Author: Thomas Hisch <t.hisch@gmail.com>
;; Maintainer: Thomas Hisch <t.hisch@gmail.com>
;; URL: https://github.com/thisch/gerrit.el
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; this program; see the file LICENSE. If not, write to the write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;;
;; This package contains
;;
;; * defuns for downloading and uploading a change
;;   git-review is used for this
;; * open-reviews section in magit
;;     The gerrit changes for the current project are queried using the rest API
;;
;;     section local keymap:
;;        RET - opens change in browser
;;
;; TODO
;; reviewers (cache each teammember seperately) -> store it in history file
;;            make it possible to have named groups of reviewers (e.g. pyeven, pyodd, cpp, web, jobdeck)
;;            allow to configure it via an *.el file
;;            add/remove single reviewers from selected group (using +/- key bindings)
;;            ...

;; set assignee for git-review (cli) app,
;; assignee can not be set via git push options (https://gerrit-review.googlesource.com/Documentation/user-upload.html). Those push options are used when uploading a change via git-review.
;; take a look at pygerrit2 (github) - write small script, which sets assignee instead
;;
;; https://gerrit-review.googlesource.com/Documentation/rest-api-changes.html#add-reviewer
;;
;; (load 'gerrit)
;; (add-hook 'after-init-hook #'gerrit-mode)
;; (global-set-key (kbd "C-x i") 'gerrit-upload)
;; (global-set-key (kbd "C-x o") 'gerrit-download)
;; (add-hook 'magit-status-sections-hook #'gerrit-magit-insert-status t)

;;; Code:

(require 'cl-lib)  ;; for cl-remove-duplicates
(require 'dash)
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
(defvar gerrit-upload-reviewer-history nil "List of recently used reviewers.")
(defvar gerrit-upload-args-history nil "List of recently used args for git-review cmd.")

;; these two vars are mainly needed for the hydra-based implementation because
;; I don't know how I can communicate between different heads of the hydra
(defvar gerrit-last-reviewers nil "...")
(defvar gerrit-last-topic nil "...")
(defvar gerrit-upload-args nil "...")

(defalias 'gerrit-dump-variable 'recentf-dump-variable)
(defalias 'gerrit-save-file-modes 'recentf-save-file-modes)

(defgroup gerrit nil
  "Maintain a menu of recently opened files."
  :version "25.1"
  ;; which group should be used?
  :group 'files)

(defcustom gerrit-upload-max-saved-items 200
  "Maximum number of items of the gerrit lists that will be saved.
A nil value means to save the whole lists."
  :group 'gerrit
  :type 'integer)

(defcustom gerrit-save-file (locate-user-emacs-file ".git-review")
  "File to save the recent lists into."
  ;; Persistency:
  ;; The save/load logic was copied from recentf.el
  ;; Other places in the emacs git repo, where settings are saved/loaded to/from disk are:
  ;;   savehist-mode
  ;;   ...
  ;; See http://mbork.pl/2018-09-10_Persisting_Emacs_variables
  ;; See https://lists.gnu.org/archive/html/help-gnu-emacs/2018-03/msg00120.html

  ;; TODO outsource this persistency code
  :group 'gerrit
  :type 'file)

(defcustom gerrit-host nil
  "Hostname of the gerrit instance (without the protocol prefix)."
  :group 'gerrit
  :type 'string)

(defun gerrit-authentication ()
  "Return an encoded string with gerrit username and password."
  (let ((pass-entry (auth-source-user-and-password gerrit-host)))
    (if-let ((username (nth 0 pass-entry))
             (password (nth 1 pass-entry)))
        (base64-encode-string
         (concat username ":" password)))))

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
        (gerrit-dump-variable 'gerrit-upload-reviewer-history gerrit-upload-max-saved-items)
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

(defmacro gerrit-upload-completing-set (msg history &optional history-excludes)
  ;;; what if I want to enter only a substring ?
  ;;; C-M-j:  (exits with the current input instead of the current
  ;;;          candidate (like other commands).)

  `(let* ((reduced-history (-difference ,history ,history-excludes))
          (value (ivy-completing-read
                 ,msg
                 reduced-history
                 nil nil nil nil
                 ;; default value set to LRU reviewers value
                 (car reduced-history))))
     (unless (equal "" value)
       ;; todo simplify the duplicate handling
       (push value ,history)
       (setq ,history (cl-remove-duplicates ,history :test 'string=)))
     value))

(defun gerrit-upload-add-reviewer ()
  "Interactively ask for to-be-added reviewer name."
  (interactive)
  ;; exclude the ones from the history that have already been added
  (push (gerrit-upload-completing-set
         "Reviewer: "
         gerrit-upload-reviewer-history
         gerrit-last-reviewers)
        gerrit-last-reviewers))

(defun gerrit-upload-remove-reviewer ()
  "Interactively ask for to-be-removed reviewer name."
  (interactive)
  (setq gerrit-last-reviewers
        (delete (gerrit-upload-completing-set
                 "Reviewer: "
                 gerrit-last-reviewers)
                gerrit-last-reviewers)))

(defun gerrit-upload-set-topic ()
  "Interactively ask for a topic name."
  (interactive)
  (setq gerrit-last-topic (gerrit-upload-completing-set
                           "Topic: "
                           gerrit-upload-topic-history)))

(defun gerrit-upload-set-args ()
  "Interactively ask for arguments that are passed to git-review."
  (interactive)
  (setq gerrit-upload-args (gerrit-upload-completing-set
                            "Args (space separated): "
                            gerrit-upload-args-history)))

(defun gerrit-upload-create-git-review-cmd ()
  "Create cmdstr for git-review."
  (interactive)
  (let ((reviewers (s-join " " gerrit-last-reviewers)) ;;(sort gerrit-last-reviewers #'string<)))
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
                                           (gerrit-load-lists)
                                           (setq gerrit-last-topic "")
                                           (setq gerrit-last-reviewers '())
                                           (setq gerrit-upload-args gerrit-upload-default-args))
                               :body-before-exit (gerrit-save-lists))
  "
gerrit-upload: (current cmd: %(concat (gerrit-upload-create-git-review-cmd)))
"
  ("r" gerrit-upload-add-reviewer "Add reviewer")
  ("R" gerrit-upload-remove-reviewer "Remove reviewer")
  ;; ("g" gerrit-upload-add-review-group "Add review group")
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO move this to gerrit-rest.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gerrit-rest-api-debug-flag nil
  "Non-nil means enable debugging of problems with the rest API of gerrit.")

(defun gerrit-rest-toggle-api-debug-flag ()
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
           ("Authorization" . ,(concat "Basic " (gerrit-authentication)))))
        (url-request-data data)
        (target (concat "https://" gerrit-host "/a" path)))

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

(defun gerrit-rest-escape-project (project)
  (s-replace-all '(("/" . "%2F")) project))

(defun gerrit-rest-get-server-version ()
  (interactive)
  (message (prin1-to-string (gerrit-rest-sync "GET" nil "/config/server/version"))))

(defun gerrit-rest-get-topic-info (topicname)
  "Return information about an open topic"
  ;; TODO create new buffer and insert stuff there
  ;; TODO query open topics
  (interactive "sEnter a topic name: ")
  (let* ((fmtstr (concat "/changes/?q=is:open+topic:%s&"
                         "o=DOWNLOAD_COMMANDS&"
                         "o=CURRENT_REVISION&"
                         "o=CURRENT_COMMIT&"
                         "o=DETAILED_LABELS&"
                         "o=DETAILED_ACCOUNTS"))
         (req (format fmtstr topicname))
         (resp (gerrit-rest-sync "GET" nil req)))
    (message "%s" (prin1-to-string resp))))

(defun gerrit-magit--fetch-open-reviews ()
  "Return a sequence of (number branch topic subject)."
  (interactive)
  ;; we need the following information:
  ;; changenr, version, name, CR/V, assignee, topic, fixes/related ticket
  ;; sort by modification-date?
  (condition-case nil
      (mapcar (lambda (change) (seq-map (lambda (fieldname) (cdr
                                  (assoc fieldname (cdr change))))
                          (list '_number 'branch 'topic 'subject)))
              (gerrit-magit-open-reviews-for-current-project))
    (error '())))

(defun gerrit-magit-insert-status ()
  (magit-insert-section (open-reviews)
    ;; the behavoir should be similar to "Recent commits"
    (magit-insert-heading "Open Gerrit Reviews")
    (dolist (loopvar (gerrit-magit--fetch-open-reviews))
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
    (insert ?\n)))

(defvar gerrit-magit-open-reviews-issue-section-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "jT" #'magit-todos-jump-to-todos)
    ;; (define-key map "jl" #'magit-todos-list)
    (define-key map (kbd "RET") #'gerrit-magit-open-reviews--open-gerrit-change)
    map)
  "Keymap for `gerrit-magit-open-reviews' top-level section.")

(defun gerrit-magit-open-reviews--open-gerrit-change()
  (interactive)
  (browse-url (format
               "https://%s/c/%s"
               gerrit-host
               ;; TODO change code s.t. s-chop-prefix is not needed
               (s-chop-prefix "#"
                              ;; TOOD avoid using prin1-to-string?!?
                              (prin1-to-string (nth 0 (oref (magit-current-section) value)))))))
  ;; (message (prin1-to-string (nth 0 (oref (magit-current-section) value)))))

(defun gerrit-magit--get-current-project ()
  "Return the gerrit project name, e.g., 'software/jobdeck'."
  (s-chop-suffix
   ".git"
   (nth 1 (s-split ":" (nth 0
                            (magit-config-get-from-cached-list
                             "remote.origin.url"))))))

(defun gerrit-magit-open-reviews-for-current-project ()
  (interactive)
  (let* ((project (funcall 'gerrit-magit--get-current-project))
         (json-array-type 'list)
         (req (format (concat "/changes/?q=is:open+project:%s&"
                              "o=DOWNLOAD_COMMANDS&"
                              "o=CURRENT_REVISION&"
                              "o=CURRENT_COMMIT&"
                              "o=DETAILED_LABELS&"
                              "o=DETAILED_ACCOUNTS")
                      (funcall 'gerrit-rest-escape-project project)))
         (resp (gerrit-rest-sync "GET" nil req)))
    ;; (setq open-reviews-response resp) ;; for debugging only (use M-x ielm)
    resp))

(defun gerrit-magit--get-gerrit-usernames ()
  (interactive)
  (condition-case nil
      (mapcar (lambda (account-info) (seq-map (lambda (fieldname) (cdr
                                  (assoc fieldname (cdr account-info))))
                              (list 'username)))
              (let ((json-array-type 'list))
                ;; see https://gerrit-review.googlesource.com/Documentation/rest-api-accounts.html
                (gerrit-rest-sync "GET" nil "/accounts/")))
    (error '())))


;;; TODOS:
;;; remove ivy-dependency
;;; when uploading a new patchset for a change (via `gerrit-upload`) show votes
;;; include votes in  open gerrit review lines
;;; press "ret" on line opens change in browser
;;; parse commit messages and show jira tickets (ret on jira tickets opens them)
;;;

;; (defun gerrit-magit-insert-status2 ()
;;   "Insert information about current incremental merge."
;;   (when (magit-imerge-in-progress-p)
;;     (let* ((name (or (magit-imerge-current-name)
;;                      (error "No name, but in progress?")))
;;            (state (magit-imerge-state name))
;;            (format-with-overriding
;;             (lambda (option current)
;;               (let ((val (--some
;;                           (and (string-match (format "\\`%s=\\(.+\\)"
;;                                                      (regexp-quote option))
;;                                              it)
;;                                (match-string 1 it))
;;                           magit-imerge--arguments)))
;;                 (if (and val (not (string= val current)))
;;                     (propertize val 'face 'magit-imerge-overriding-value)
;;                   current)))))
;;       (magit-insert-section (imerge)
;;         (magit-insert-heading "Incremental merge")
;;         (magit-insert-section (imerge-info)
;;           (insert (format "Name:   %s\n" name))
;;           (magit-insert-heading)
;;           (insert (format "Goal:   %s\n"
;;                           (funcall format-with-overriding
;;                                    "--goal"
;;                                    (cdr (assq 'goal state)))))
;;           (insert (format "Result: %s\n"
;;                           (funcall format-with-overriding
;;                                    "--branch"
;;                                    (cdr (assq 'branch state)))))
;;           (insert "Tips:   ")
;;           (magit-imerge--insert-tip (cdr (assq 'tip1 state)))
;;           (insert ", ")
;;           (magit-imerge--insert-tip (cdr (assq 'tip2 state)))
;;           (insert ?\n ?\n))
;;         (magit-insert-section (imerge-diagram)
;;           (magit-insert-heading
;;             (propertize "Diagram\n"
;;                         'face 'magit-section-secondary-heading))
;;           (insert
;;            (with-temp-buffer
;;              (magit-git-insert "imerge" "diagram" "--no-color" "--commits")
;;              (re-search-backward "^Key:")
;;              (delete-region (point) (point-max))
;;              (buffer-string))))))))

;; (defun gerrit-magit-insert-status3 (&optional type value)
;;   "Insert section showing recent commits.
;; Show the last `magit-log-section-commit-count' commits."
;;   (let* ((start (format "HEAD~%s" magit-log-section-commit-count))
;;          (range (and (magit-rev-verify start)
;;                      (concat start "..HEAD"))))

;;     (magit-insert-section (open-reviews)
;;       (magit-insert-heading "Open Gerrit Reviews")
;;       (magit-insert-section (longer)
;;         (insert-text-button
;;          (substitute-command-keys
;;           (format "Type \\<%s>\\[%s] to show more history"
;;                   'magit-log-mode-map
;;                   'magit-log-double-commit-limit))
;;          'action (lambda (_button)
;;                    (message "clicked"))
;;          ;; (magit-log-double-commit-limit))
;;          'follow-link t
;;          'mouse-face 'magit-section-highlight))

;;       (insert ?\n)
;;       (insert (format "l1\nl2 %s\n" range))

;;       (magit-insert-log range
;;                         (cons (format "-n%d" magit-log-section-commit-count)
;;                               (--remove (string-prefix-p "-n" it)
;;                                         magit-log-section-arguments))))))

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

;; (defun gerrit-get-assignee (project version changeidnr)
;;   "Retrieve summary of TICKETID in jira."
;;   (let* ((url
;;           (concat "https://" gerrit-host
;;                   ;; "/changes/?q=7730&o=ALL_REVISIONS"))
;;                   ;; "/changes/?q=7730&o=ALL_REVISIONS"))
;;          ;; (format "/changes/%s~%s~%d/assignee" project version changeidnr)))
;;          (url-request-method "GET")
;;          (url-request-extra-headers
;;           `(("Content-Type" . "application/json")
;;             ("Authorization" . ,(concat "Basic " (gerrit-authentication)))))
;;          )

;;     ;; (message url)))
;;     (switch-to-buffer (url-retrieve-synchronously url))
;;     (goto-char url-http-end-of-headers)))))
;;     ;; (with-current-buffer (url-retrieve-synchronously url)
;;     ;;   (goto-char url-http-end-of-headers)
;;       ;; (if-let ((json-object-type 'hash-table)
;;       ;;          (json-key-type 'symbol)
;;       ;;          (result (json-read))
;;       ;;          (name (map-elt result 'name)))
;;       ;;     (message "Assignee is set to %s" name)))))

;; TODO write some testcases



(provide 'gerrit)
;;; gerrit.el ends here
