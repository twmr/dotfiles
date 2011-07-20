(evil-mode 1)

;;TODO modeline color changes

;; TODO start hl-line in each buffer where the major modes are note in hl-line-disabled-modes-list

(defun thi-turn-hl-line-mode-on ()
  (interactive)
  (if (not (member major-mode hl-line-disabled-modes-list))
      (hl-line-mode 1)))

(defun thi-turn-hl-line-mode-off ()
  (interactive)
  (if (not (member major-mode hl-line-disabled-modes-list))
      (hl-line-mode -1)))


;; DISABLE hl-line in visual mode:
(add-hook 'evil-visual-state-entry-hook 'thi-turn-hl-line-mode-off)
(add-hook 'evil-visual-state-exit-hook 'thi-turn-hl-line-mode-on)
