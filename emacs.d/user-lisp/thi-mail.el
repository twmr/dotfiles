(setq user-mail-address "thomas.hisch@ims.co.at"
      user-full-name "Thomas Hisch")

(defvar th:email-addresses
  '("t.hisch@gmail\\.com"
    "thomas.hisch@ims\\.co\\.at")
  "Regexp of my email addreses.")

;; do I need this ??
(setq notmuch-identities
  '("thomas.hisch@ims.co.at"))

(defvar th:email-addresses-regexp
  (concat "^\\("
          (mapconcat 'identity th:email-addresses "\\|")
          "\\)$"))

(setq work-smtp-port 587)
(setq work-smtp-host "smtp.ims.co.at")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "xdg-open")

;; (setq message-signature-file "~/.signature")

;; Configure outbound mail (SMTP)
(setq smtpmail-starttls-credentials `((,work-smtp-host ,work-smtp-port nil nil))
      smtpmail-smtp-server work-smtp-host
      smtpmail-local-dmain "thisch.org"
      smtpmail-default-smtp-server work-smtp-host
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service work-smtp-port
      smtpmail-auth-credentials `((,work-smtp-host ,work-smtp-port "I010229@ims.co.at" nil))
      smtpmail-debug-info t
      smtpmail-debug-verb t)

;; kill buffer after sending mail
(setq message-kill-buffer-on-exit t)

(defun expand-only-unread-hook ()
  (interactive)
  (let ((unread nil)
        (open (notmuch-show-get-message-ids-for-open-messages)))
    (notmuch-show-mapc (lambda ()
                         (when (member "unread" (notmuch-show-get-tags))
                           (setq unread t))))
    (when unread
      (let ((notmuch-show-hook (remove 'expand-only-unread-hook notmuch-show-hook)))
        (notmuch-show-filter-thread "tag:unread")))))

(add-hook 'notmuch-show-hook 'expand-only-unread-hook)
