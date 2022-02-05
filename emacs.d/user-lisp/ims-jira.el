;;; ims-jira.el --- Jira integration @ IMS -*- lexical-binding: t; -*-
(require 'url)
(require 'url-http)
(require 'url-vars)
(require 's)
(require 'dashboard-table)

(defcustom ims-jira-host "jira.rnd.ims.co.at"
  "hostname of the jira instance"
  :group 'ims-jira
  :type 'string)

(defun jira-rest-authentication ()
  "Return an encoded string with jira username and password."
  (let ((pass-entry (auth-source-user-and-password ims-jira-host)))

    (if-let ((username (nth 0 pass-entry))
             (password (nth 1 pass-entry)))
        (base64-encode-string
         (concat username ":" password)))))

(defmacro jira-rest--read-json (str)
  "Read json string STR."
  (if (progn
        (require 'json)
        (fboundp 'json-parse-string))
      `(json-parse-string ,str
                          :object-type 'alist
                          :array-type 'list
                          :null-object nil
                          :false-object nil)
    `(let ((json-array-type 'list)
           (json-object-type 'alist)
           (json-false nil))
       (json-read-from-string ,str))))

(defun jira-rest-sync (method data &optional path)
  "Interact with the API using method METHOD and data DATA.
Optional arg PATH may be provided to specify another location further
down the URL structure to send the request."
  (let ((url-request-method method)
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(concat "Basic " (jira-rest-authentication)))))
        (url-request-data data)
        (target (concat "https://" ims-jira-host path)))

    (with-current-buffer (url-retrieve-synchronously target t)
      (goto-char url-http-end-of-headers)
      (jira-rest--read-json
       (buffer-substring (point) (point-max))))))

(defun ims-jira-summary-and-status (ticketid)
  (interactive "sEnter ticket id: ")
  (if-let ((result (jira-rest-sync "GET" nil
                                   (format "/rest/api/2/issue/%s?fields=summary,status" ticketid)))
           (fields (alist-get 'fields result)))
      fields))

(defun ims-jira-summary (ticketid)
  (interactive "sEnter ticket id: ")
  (if-let ((result (jira-rest-sync "GET" nil
                                   (format "/rest/api/2/issue/%s?fields=summary" ticketid)))
           (fields (alist-get 'fields result)))
      (alist-get 'summary fields)))

(defun ims-jira-status (ticketid)
  (interactive "sEnter ticket id: ")
  (if-let ((result (jira-rest-sync "GET" nil
                                   (format "/rest/api/2/issue/%s?fields=status" ticketid)))
           (fields (alist-get 'fields result)))
      ;; This returns a structure like the following:
      ;;  ((self . "https://jira.rnd.ims.co.at/rest/api/2/status/10001")
      ;;   (description . "Jira status description: https://confluence.rnd.ims.co.at/display/RDM/Jira+issue+statuses")
      ;;   (iconUrl . "https://jira.rnd.ims.co.at/images/icons/status_generic.gif")
      ;;   (name . "Done")
      ;;   (id . "10001")
      ;;   (statusCategory
      ;;    (self . "https://jira.rnd.ims.co.at/rest/api/2/statuscategory/3")
      ;;    (id . 3)
      ;;    (key . "done")
      ;;    (colorName . "green")
      ;;    (name . "Done")))
      (alist-get 'status fields)))

(defun ims-jira-search-issues (jql &optional start-at max-results fields)
  ;;fields: Do I want to allow both lists of str as well as a commma separted str?
  (interactive)
  ;; TODO output nicely readable error msg
  ;; take a look at interface in
  ;; https://github.com/pycontribs/jira/blob/09ece94f3cae7e6d0becfa87d77d6a05ce01cdf6/jira/client.py#L2807

  (let* (
         (fields (or fields "summary"))
         (result (jira-rest-sync "GET" nil
                                   (format "/rest/api/2/search/?jql=%s&fields=%s"
                                           ;; the jql-str is hexified in the url library
                                           jql
                                           fields
                                           ))))
      result))

(defun thi-jira--search-all (jql fields)
  (let ((continue t)
        (start-idx 0)
        (cur-results nil)
        (results '()))
    (while continue
      (setq cur-results
            (ims-jira-search-issues
             jql
             start-idx nil fields))
      (setq results (append
                     results
                     (alist-get 'issues cur-results)))
      (setq start-idx (+ start-idx (length (alist-get 'issues cur-results))))
      ;; (message "start: %s" start-idx)
      (setq continue (< start-idx (alist-get 'total cur-results))))
    results))

(defun thi-jira--format-issue (key desired-fields fields)
  (concat
   ;; TODO only consider desired-fields
   ;; TODO desired fields should be a list of symbols
   (propertize (concat key ": ") 'face 'magit-hash)
   (propertize (alist-get 'summary fields) 'face 'magit-section-highlight)
   " "
   (propertize (concat "[" (alist-get 'name (alist-get 'status fields)) "]")
               'face 'magit-tag)))

(defun thi-jira-list-issues (&optional jql)
  (interactive)
  (let* ((fields "key,summary,status")
         (issues (thi-jira--search-all
                  jql
                  fields)))
    (concat
     (s-join "\n" (mapcar (lambda (issue)
                            (thi-jira--format-issue (alist-get 'key issue) fields
                                                    (alist-get 'fields issue))) issues))
     (format "\ntotal: %d"  (length issues)))))


(defun thi-jira-list-issues-reporter-me ()
  (interactive)
  (thi-jira-list-issues "reporter = currentUser() AND project = RD"))

(defun thi-jira-colored-status (status)
  ;; note that status is upper case
  (if (equal status "DONE")
      (propertize (concat "[" status "]")
                  'face '(:foreground "green"))
    (propertize (concat "[" status "]") 'face 'magit-tag)))

(defun thi-jira-metadata (issue)
  (let ((fields (alist-get 'fields issue)))
    `[
      ,(propertize (alist-get 'key issue) 'face 'magit-hash)
      ,(propertize (alist-get 'summary fields) 'face 'magit-section-highlight)
      ,(thi-jira-colored-status (upcase (alist-get 'name (alist-get 'status fields))))

      ;; TODO fixversions is a list: concat all fix verions
      ,(or (alist-get 'name (car (alist-get 'fixVersions fields))) "")
      ;; ,(propertize (alist-get 'name (alist-get 'fixVersions fields)) 'face 'magit-section-highlight)
      ;; "A"
      ]))

(defun jira-table-get-section-data (jql)
  "Return a list with \"tabulated-list-entries\"."
  (message "called with %s" jql)
  (let* ((fields "key,summary,status,fixVersions")
         (issues (thi-jira--search-all
                  jql
                  fields)))
    (seq-map (lambda (issue)
               ;; TODO use section + issue id instead of nil
               `(nil ,(thi-jira-metadata issue)))
             issues)))

(define-derived-mode jira-table-mode dashboard-table-mode "jira-table"
  "jira-table mode"
  (variable-pitch-mode)
  )

;;;###autoload
(defun thi-jira-table ()
  "Show a dashboard in a new buffer."
  (interactive)
  (switch-to-buffer "jira-table")
  (jira-table-mode)

  (setq dashboard-table-section-alist
        `(("small sd tasks" . "labels=small_sd_task")
          (,(propertize "Implementd tasks" 'face '(:inherit dashboard-table-section :background "red")) .
           "\"Group/s\" in (SD-highlevel, SD-lowlevel, SD-web) AND status = \"Implemented?\" ORDER BY created DESC")))
  (setq dashboard-table-columns
        [("Key" 10)
         ("Summary" 75)
         ("Status" 10)
         ("FixVersion" 10)
         ])
  (setq dashboard-table-get-section-data-function
        #'jira-table-get-section-data)


  (dashboard-table--refresh)
  )

;; TODO use svg-lib for displaying tags
;; TODO display summary of tickets inline like in confluence, but the summary should be not part of the ASCII file!!
;; maybe think about asking in emacs-devel about this request (collection of meta-data from tickets in e.g. bug-reference-mode)

;; -> use invisible properties and use a hook that replaces all occurrences of tickets with the desired output
;; -> is it possible to replace those properties if they already exist?


;; which emacs feature can I use for that?
;;https://raw.githubusercontent.com/rougier/svg-lib/master/svg-lib-demo.org




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OLD ;;;;;;;;;;;;;;;;;;;

(defun ims-jira-get-ticket-summary (ticketid)
  "Retrieve summary of TICKETID in jira."
  (interactive "sEnter ticket id: ")
  (require 'json)
  (require 'subr-x)
  (let* ((url
          (concat "https://" ims-jira-host
                  (format "/rest/api/2/issue/%s?fields=summary" ticketid)))
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Basic " (ims-jira-authentication)))))
         ;; (gnutls-verify-error nil)
         )
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (if-let ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (result (json-read))
               (fields (map-elt result 'fields)))
          (map-elt fields 'summary)))))

(defun thi-jira-summary-status (ticketid)
  (interactive "sEnter ticket id: ")
  (let* ((fields (ims-jira-summary-and-status ticketid))
        (summary (alist-get 'summary fields))
        (status (alist-get 'status fields)))
    ;; TODO return a cached SVG tag for the status
    ;; TODO clickable?
    (concat
     (propertize (concat ticketid ": ") 'face 'magit-hash)
     (propertize summary 'face 'magit-section-highlight)
     " "
     (propertize (concat "[" (alist-get 'name status) "]")
                 'face 'magit-tag))))



;; (ims-jira-get-ticket-summary "RD-872")
;; (ims-jira-get-ticket-status "RD-872")
;; (message (ims-jira-authentication)


(provide 'ims-jira)
