(evil-mode 1)

;;curser color fix (emacs 24)
;;http://stackoverflow.com/questions/9900232/changing-color-themes-emacs-24-order-matters
(setq evil-default-cursor t)

(define-key evil-normal-state-map (kbd "*")
  (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'symbol)))))
(define-key evil-normal-state-map (kbd "#")
  (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'word)))))
;;TODO modeline color changes

;; (defvar evil-normal-mode-line-background (face-background 'mode-line))
;; (defvar evil-normal-mode-line-foreground (face-foreground 'mode-line))
;; (defun evil-set-mode-line-color (&rest after-which-mode)
;;   (set-face-background
;;    'mode-line
;;    (cond
;;     ((evil-visual-state-p) "cornflower blue")
;;     ((evil-emacs-state-p) evil-normal-mode-line-background)
;;     ((evil-visual-state-p) "cornflower blue")
;;     ((evil-normal-state-p) "DarkGoldenrod1"))
;;    ))


;;   ;; (set-face-foreground
;;   ;;  'mode-line
;;   ;;  (cond
;;   ;;   ((eq viper-current-state 'replace-state)
;;   ;;    "black")
;;   ;;   ((eq viper-current-state 'emacs-state)
;;   ;;    evil-normal-mode-line-foreground)
;;   ;;   ((eq viper-current-state 'insert-state)
;;   ;;    "black")
;;   ;;   ((eq viper-current-state 'vi-state)
;;   ;;    "black"))))

;; (add-hook 'evil-normal-state-entry-hook 'evil-set-mode-line-color)
;; (add-hook 'evil-visual-state-entry-hook 'evil-set-mode-line-color)
;; (add-hook 'evil-emacs-state-entry-hook 'evil-set-mode-line-color)
;; (add-hook 'window-configuration-change-hook 'evil-set-mode-line-color)


;; DISABLE hl-line in visual mode:
;; (add-hook 'evil-visual-state-entry-hook 'thi-turn-hl-line-mode-off)
;; (add-hook 'evil-visual-state-exit-hook 'thi-turn-hl-line-mode-on)

;; associate certain modes with default evil states

;; extra keybindings

(define-key evil-normal-state-map "gs" 'magit-status)
(define-key evil-insert-state-map "\C-k" 'kill-line)
(define-key evil-normal-state-map "\C-k" 'kill-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-normal-state-map "\C-e" 'end-of-line)

(define-key evil-insert-state-map [remap newline] 'evil-ret-and-indent)

;;thank you @magnars
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)


(define-key evil-normal-state-map "\C-u" 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-.") nil)

(define-key evil-normal-state-map "+" 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map "-" 'evil-numbers/dec-at-pt)

;; @see http://stackoverflow.com/questions/10569165/how-to-map-jj-to-esc-in-emacs-evil-mode
;; @see http://zuttobenkyou.wordpress.com/2011/02/15/some-thoughts-on-emacs-and-vim/
;; NOTE that this function caused the 'mkdir' problem in ansi-term (if it comes up in insert-mode)
;; (define-key evil-insert-state-map "k" #'cofi/maybe-exit)
;; (evil-define-command cofi/maybe-exit ()
;;   :repeat change
;;   (interactive)
;;   (let ((modified (buffer-modified-p)))
;;     (insert "k")
;;     (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
;;                nil 0.5)))
;;       (cond
;;        ((null evt) (message ""))
;;        ((and (integerp evt) (char-equal evt ?j))
;;     (delete-char -1)
;;     (set-buffer-modified-p modified)
;;     (push 'escape unread-command-events))
;;        (t (setq unread-command-events (append unread-command-events
;;                           (list evt))))))))

(delete 'term-mode evil-insert-state-modes)

;; see https://github.com/redguardtoo/emacs.d/blob/master/init-evil.el
(require 'cl-lib)
(cl-loop for (mode . state) in
      '(
        (eshell-mode . emacs)
        (shell-mode . emacs)
        (term-mode . emacs)
        (compilation-mode . emacs)
        (image-mode . emacs)
        (image-dired-mode . emacs)
        (image-dired-thumbnail-mode . emacs)
        (speedbar-mode . emacs)
        (quickrun/mode . emacs)
        (makey-key-mode . emacs)
        (flycheck-error-list-mode . emacs)
        (paradox-menu-mode . emacs)
        (dashboard-mode . emacs)
        (python-mode . emacs)
        (emacs-lisp-mode . emacs)
        (text-mode . emacs)
        )
      do (evil-set-initial-state mode state))
