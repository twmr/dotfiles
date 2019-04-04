;;; ims-jira.el --- Jira integration @ IMS -*- lexical-binding: t; -*-

(defcustom ims-jira-host "jira.rnd.ims.co.at"
  "hostname of the jira instance"
  :group 'ims-jira
  :type 'string)

(defun ims-jira-authentication ()
  "Return an encoded string with jira username and password."
  (let ((pass-entry (auth-source-user-and-password ims-jira-host)))

    (if-let ((username (nth 0 pass-entry))
             (password (nth 1 pass-entry)))
        (base64-encode-string
         (concat username ":" password)))))

(defun ims-jira-get-ticket-summary (ticketid)
  "Retrieve summary of TICKETID in jira."
  (require 'json)
  (require 'url)
  (require 'url-http)
  (require 'url-vars)
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
;; (message (ims-jira-authentication))

(provide 'ims-jira)
