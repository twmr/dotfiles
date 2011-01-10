(require 'cc-mode)
(setq c-default-style (quote ( (c-mode . "stroustrup")
                               (c++-mode . "stroustrup")
                               (java-mode . "java")
                               (awk-mode . "awk")
                               (other . "gnu") )))

;; set linux c-style if filename or directory contains the string
;; linux
(defun maybe-linux-style ()
  (when (and buffer-file-name
             (string-match "linux" buffer-file-name))
    (c-set-style "Linux")))
(add-hook 'c-mode-hook 'maybe-linux-style)

;; do not create newlines for electric keys if the following line is
;; nonblank
(defun c-semi&comma-no-newlines-before-nonblanks ()
  (save-excursion
    (if (and (eq last-command-char ?\;)
           (zerop (forward-line 1))
           (not (looking-at "^[ \t]*$")))
        'stop
      nil)))
(setq c-hanging-semi&comma-criteria
  (cons 'c-semi&comma-no-newlines-before-nonblanks
        c-hanging-semi&comma-criteria))

;; due to no-newlines-before-nonblanks I need this from the
;; google-styleguide
(defun google-make-newline-indent ()
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map [ret] 'newline-and-indent))
(add-hook 'c-mode-common-hook 'google-make-newline-indent)


;; automatic identation (auto mode -> /a in modeline) and empowers the
;; Del key to delete all whitespace to the left of the point (hungry
;; mode -> h in modeline) for C-based languages (including java) for
;; current keybindings see - cc-mode manual
(add-hook 'c-mode-common-hook
          '(lambda () (c-toggle-auto-hungry-state 1)))


;; makes delete map to hungry mode
(defun thi-map-delete-hungry ()
  (define-key c-mode-base-map [delete] 'c-hungry-backspace))
(add-hook 'c-mode-common-hook 'thi-map-delete-hungry)
