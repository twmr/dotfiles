;; my personal git-review integration in magit
;; *) download a change
;;   - download change metadata (number, branch, msg; see output of git-review -l) using "git review -l" or REST API and make the number make it possible to select changes
;; *) update current change
;;   - if you simply want to download the change again
;; TODO handle interactive uploads (typing yes when calling git-review)
;; *) upload for review
;;   - option for reviewer-names (with completion, sorted by most recently used)
;;   - option for topic-name (with completion (my created topicnames))
;;   -

(global-set-key (kbd "C-x i") 'magit-review-popup)

(magit-define-popup magit-review-popup
  "Popup console for review commands."
  :man-page "git-review"
  :actions  '("Download"
              (?d "change"  magit-review-download-change)
              "Upload"
              (?u "upload"  magit-review-upload-popup)))


;; add "Gerrit" action to magit-dispatch popup
(magit-define-popup-action 'magit-dispatch-popup ?g "Gerrit" 'magit-review-popup)

;; is a magit-popup the correct way to implement for this?
(magit-define-popup magit-review-upload-popup
  "Popup console for uploading a change."
  :man-page "git-review"
  :options  '(
              ;; add the possibility to cycle through a list of list of reviewers
              (?r "reviewers"                        "--reviewer=")

              (?t "topic"                            "--topic="))
  :actions  '((?u "upload"  magit-review-upload-run)))

(defvar magit-review-download-change-history nil)

;; https://magit.vc/manual/magit-popup/Defining-Prefix-and-Suffix-Commands.html#Defining-Prefix-and-Suffix-Commands
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

(defun magit-review-upload-change ()
  (interactive)
  ;; TODO
  (message "WIP"))

(defun magit-review-upload-run ()
  (let (
        (review-args (string-join (magit-review-upload-arguments) " ")))
    (message (format "uploading change (%s)" review-args))
    (magit-git-command (format "git review %s" review-args))))


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
