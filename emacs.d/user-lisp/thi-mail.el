(setq user-mail-address "thomas.hisch@tuwien.ac.at"
      user-full-name "Thomas Hisch")

(defvar th:email-addresses
  '("t.hisch@gmail\\.com"
    "thomas.hisch@tuwien\\.ac\\.at")
  "Regexp of my email addreses.")

;; do I need this ??
(setq notmuch-identities
  '("thomas.hisch@tuwien.ac.at"))

(defvar th:email-addresses-regexp
  (concat "^\\("
          (mapconcat 'identity th:email-addresses "\\|")
          "\\)$"))

(setq tu-smtp-port 587)
(setq tu-smtp-host "mail.tuwien.ac.at")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "xdg-open")

(setq message-signature-file "~/.signature")

;; Configure outbound mail (SMTP)
(setq smtpmail-starttls-credentials `((,tu-smtp-host ,tu-smtp-port nil nil))
      smtpmail-smtp-server tu-smtp-host
      smtpmail-local-dmain "thisch.org"
      smtpmail-default-smtp-server tu-smtp-host
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service tu-smtp-port
      smtpmail-auth-credentials `((,tu-smtp-host ,tu-smtp-port "thisch" nil))
      smtpmail-debug-info t
      smtpmail-debug-verb t)

;; kill buffer after sending mail
(setq message-kill-buffer-on-exit t)

(defun expand-only-unread-hook () (interactive)
       (let ((unread nil)
             (open (notmuch-show-get-message-ids-for-open-messages)))
         (notmuch-show-mapc (lambda ()
                              (when (member "unread" (notmuch-show-get-tags))
                                (setq unread t))))
         (when unread
           (let ((notmuch-show-hook (remove 'expand-only-unread-hook notmuch-show-hook)))
             (notmuch-show-filter-thread "tag:unread")))))

(add-hook 'notmuch-show-hook 'expand-only-unread-hook)