;; my personal git-review integration in magit.
;; *) DONE download a change
;;   - DONE download change metadata (number, branch, msg; see output of git-review -l) using "git review -l" or REST API and make the number make it possible to select changes
;; *) update current change
;;   - if you simply want to download the change again
;; TODO handle interactive uploads (typing yes when calling git-review)
;; *) DONE upload for review
;;   - option for reviewer-names (with completion, sorted by most recently used)
;;   - option for topic-name (with completion (my created topicnames))
;;   -
;;
;; (add-hook 'after-init-hook #'git-review-load-lists)
;; (global-set-key (kbd "C-x i") 'hydra-gerrit-upload/body)
