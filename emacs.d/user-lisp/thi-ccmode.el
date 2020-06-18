(require 'cc-mode)
(setq c-default-style '((c-mode . "stroustrup")
                        (c++-mode . "stroustrup")
                        (java-mode . "java")
                        (awk-mode . "awk")
                        (other . "gnu") ))

;; turn auto newlines off
;; (setq c-toggle-auto-newline -1)

;; set linux c-style if filename or directory contains the string
;; linux
;; (defun maybe-linux-style ()
;;   (when (and buffer-file-name
;;              (string-match "linux" buffer-file-name))
;;     (c-set-style "Linux")))
;; (add-hook 'c-mode-hook 'maybe-linux-style)

;; (defun cf-fem-lib-style ()
;;   (interactive)
;;   (message (buffer-file-name))
;;   (when (and buffer-file-name
;;              (or (string-match "cf-fem-lib" buffer-file-name)
;;                  (string-match "pyspu" buffer-file-name)
;;                  (string-match "Sessa" buffer-file-name)))
;;     (progn
;;       (c-set-style "stroustrup")
;;       (setq c-basic-offset 2)
;;       (c-set-offset 'inclass 2)
;;       ;; (c-set-offset 'access-label 0)
;;       ;; (c-set-offset 'topmost-intro 0)
;;       )
;;     ;; (setq-default c-basic-offset 2
;;     ;;               tab-width 2
;;     ;;               indent-tabs-mode nil)
;;     ))

;; (defun compilation-cffemlib-stuff ()
;;   (when (string-match "cf-fem-lib" buffer-file-name)
;;     ;; (make-local-variable 'compile-command)
;;     ;; (setq 'compile-commnad "cd /home/thomas/gitrepos/tudadoc && make")
;;     (set (make-local-variable 'compilation-read-command) nil)
;;     (set (make-local-variable 'compile-command)
;;          "cd ~/cf-fem-lib/build && make")))

;; (add-hook 'c++-mode-hook 'cf-fem-lib-style)
;; (add-hook 'c++-mode-hook 'compilation-cffemlib-stuff)


(defun ims-style ()
  (interactive)
  (message (buffer-file-name))
  (when (and buffer-file-name
             (string-match "sandbox" buffer-file-name))
    (progn
      (c-set-style "stroustrup")
      (setq c-basic-offset 2)
      (c-set-offset 'inclass 2)
      ;; (c-set-offset 'access-label 0)
      ;; (c-set-offset 'topmost-intro 0)
      )
    ;; (setq-default c-basic-offset 2
    ;;               tab-width 2
    ;;               indent-tabs-mode nil)
    ))
(add-hook 'c++-mode-hook 'ims-style)


;; do not create newlines for electric keys if the following line is
;; nonblank
;; (defun c-semi&comma-no-newlines-before-nonblanks ()
;;   (save-excursion
;;     (if (and (eq last-command-char ?\;)
;;            (zerop (forward-line 1))
;;            (not (looking-at "^[ \t]*$")))
;;         'stop
;;       nil)))
;; (setq c-hanging-semi&comma-criteria
;;  (cons 'c-semi&comma-no-newlines-before-nonblanks
;;         c-hanging-semi&comma-criteria))

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
;; (add-hook 'c-mode-common-hook
;;           (lambda () (c-toggle-auto-hungry-state 1)))

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (c-toggle-hungry-state 1)
;;             (superword-mode 1)
;;             ))

(define-key c-mode-map (kbd "C-`") 'helm-semantic-or-imenu)
(define-key c++-mode-map (kbd "C-`") 'helm-semantic-or-imenu)

;; makes delete map to hungry mode
;;(defun thi-map-delete-hungry ()
;;  (define-key c-mode-base-map [backspace] 'c-hungry-backspace))
;;(add-hook 'c-mode-common-hook 'thi-map-delete-hungry)
