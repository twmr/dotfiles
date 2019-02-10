(setq mu4e-get-mail-command "offlineimap")

(setq mu4e-use-fancy-chars t)

(setq mu4e-drafts-folder "/Drafts"
      mu4e-sent-folder   "/Sent"
      mu4e-trash-folder   "/Trash")

(setq mu4e-maildir-shortcuts
      '(("/INBOX" . ?i)
        ("/Sent" . ?s)
        ("/Trash" . ?t)))

;; Try to display images in mu4e
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

(setq mu4e-update-interval 600)

