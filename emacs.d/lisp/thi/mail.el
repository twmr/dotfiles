(setq user-mail-address "thomas.hisch@ims.co.at"
      user-full-name "Thomas Hisch")

(defvar th:email-addresses
  '("t.hisch@gmail\\.com"
    "thomas.hisch@ims\\.co\\.at")
  "Regexp of my email addreses.")

(defvar th:email-addresses-regexp
  (concat "^\\("
          (mapconcat 'identity th:email-addresses "\\|")
          "\\)$"))

(setq ims-smtp-port 25)
(setq ims-smtp-host "mail.ims.co.at")

(when (string= system-name "pc-52-rh.ims.co.at")
  (setq ims-smtp-port 587)
  (setq ims-smtp-host "smtp.ims.co.at"))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "xdg-open")

;; Configure outbound mail (SMTP)
(setq smtpmail-starttls-credentials `((,ims-smtp-host ,ims-smtp-port nil nil))
      smtpmail-smtp-server ims-smtp-host
      smtpmail-local-dmain "thisch.org"
      smtpmail-default-smtp-server ims-smtp-host
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service ims-smtp-port
      smtpmail-auth-credentials `((,ims-smtp-host ,ims-smtp-port "thomas.hisch" nil))
      smtpmail-debug-info t
      smtpmail-debug-verb t)

;; kill buffer after sending mail
(setq message-kill-buffer-on-exit t)
