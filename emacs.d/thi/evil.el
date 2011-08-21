(evil-mode 1)

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
(add-hook 'evil-visual-state-entry-hook 'thi-turn-hl-line-mode-off)
(add-hook 'evil-visual-state-exit-hook 'thi-turn-hl-line-mode-on)

;; associate certain modes with default evil states

;; extra keybindings

(define-key evil-normal-state-map "gs" 'magit-status)
(define-key evil-insert-state-map "\C-k" 'kill-line)
