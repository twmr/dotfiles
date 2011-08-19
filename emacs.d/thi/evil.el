(evil-mode 1)

;;TODO modeline color changes

;; DISABLE hl-line in visual mode:
(add-hook 'evil-visual-state-entry-hook 'thi-turn-hl-line-mode-off)
(add-hook 'evil-visual-state-exit-hook 'thi-turn-hl-line-mode-on)

;; associate certain modes with default evil states

;; extra keybindings

(define-key evil-normal-state-map "gs" 'magit-status)
(define-key evil-insert-state-map "\C-k" 'kill-line)
