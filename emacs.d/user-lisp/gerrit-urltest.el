(require 'auth-source)
(require 'json)
(require 'url)
(require 'url-http)
(require 'url-vars)
(require 'subr-x)


(defcustom ims-gerrit-host "gerrit.ims.co.at"
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

(defun ims-gerrit--get-auth-headers ()
  "Helper function to setup auth header for gerrit url call."
  `(("Content-Type" . "application/x-www-form-urlencoded")
    ("Authorization" .
     ,(concat
       "Basic "
       (ims-gerrit-authentication)))))

(defun ims-gerrit--retrieve-page-as-json (path)
  "Shortcut for gerrit api PATH to return valid json."
  (let ((url-request-extra-headers (ims-gerrit--get-auth-headers)))
    (with-current-buffer (url-retrieve-synchronously
                          (concat "https://" ims-gerrit-host path))
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))

      ;; FIXME
      (buffer-string))
      ;; (json-read-from-string (buffer-string)))
))


(defun gerrit-get-server-version ()
  (interactive)
  (switch-to-buffer "*gerritests*")
  (erase-buffer)
  (dolist (loopvar `(0 1 2 3 4 5 6 7 8))
    (insert (format "\n##### %d\n" loopvar))
    (insert (prin1-to-string (ims-gerrit--retrieve-page-as-json
                              "/config/server/version")))))
