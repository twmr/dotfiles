;;; Recent Files

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 60)
(setq recentf-max-saved-items 500)
;;(setq recentf-save-file (concat user-cache-file-dir ".recentf"))

(defun xsteve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))

;; todo integrate it into ido better and bind it to some Vimpulse/Viper keys
(global-set-key [f11] 'xsteve-ido-choose-from-recentf)
