;; Make dired less verbose
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)
(add-hook 'dired-mode-hook #'dired-details-show)
