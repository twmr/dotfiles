;; type C-c C-c in quickrun buffer to kill a stalled process
(setq quickrun-timeout-seconds nil)

;; https://github.com/syohex/emacs-quickrun/issues/15
(defun my/quickrun-hook ()
  (goto-char (point-min)))
(add-hook 'quickrun-after-run-hook 'my/quickrun-hook)
