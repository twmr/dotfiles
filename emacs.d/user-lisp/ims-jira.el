;;; ims-jira.el --- Jira integration @ IMS -*- lexical-binding: t; -*-
(require 'url)
(require 'url-http)
(require 'url-vars)
(require 's)

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

(defun ims-jira-search-issues (jql-str &optional start-at max-results fields)
  ;;fields: Do I want to allow both lists of str as well as a commma separted str?
  (interactive "sEnter jql str: ")
  (interactive)
  ;; TODO output nicely readable error msg
  ;; take a look at interface in
  ;; https://github.com/pycontribs/jira/blob/09ece94f3cae7e6d0becfa87d77d6a05ce01cdf6/jira/client.py#L2807

  (if-let ((result (jira-rest-sync "GET" nil
                                   (format "/rest/api/2/search/%s"
                                           ;; the jql-str is hexified in the url library
                                           jql-str
                                           ))))
      result))

(defun thi-jira--format-issue (key fields)
  (concat
     (propertize (concat key ": ") 'face 'magit-hash)
     (propertize (alist-get 'summary fields) 'face 'magit-section-highlight)
     " "
     (propertize (concat "[" (alist-get 'name (alist-get 'status fields)) "]")
                 'face 'magit-tag)))

(defun thi-jira-list-all-issues () ;;(jql-str)
  (interactive)
  ;; TODO support for more than 50 issues: see start-at parameter
  (s-join "\n" (mapcar (lambda (issue)
                         (thi-jira--format-issue (alist-get 'key issue)
                                                 (alist-get 'fields issue)))
                       (alist-get 'issues (ims-jira-search-issues
                                           "?jql=labels = small_sd_task&fields=summary,status")))))
                                           ;; "?jql=labels%20%3D%20small_sd_task&fields=summary,status")))))

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


;; (ims-jira-get-ticket-summary "RD-872")
;; (ims-jira-get-ticket-status "RD-872")
;; (message (ims-jira-authentication)


;; TODO use svg-lib for displaying tags
;; TODO display summary of tickets inline like in confluence, but the summary should be not part of the ASCII file!!
;; maybe think about asking in emacs-devel about this request (collection of meta-data from tickets in e.g. bug-reference-mode)

;; -> use invisible properties and use a hook that replaces all occurrences of tickets with the desired output
;; -> is it possible to replace those properties if they already exist?


;; which emacs feature can I use for that?
;;https://raw.githubusercontent.com/rougier/svg-lib/master/svg-lib-demo.org


(provide 'ims-jira)
