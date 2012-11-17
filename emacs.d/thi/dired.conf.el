(eval-after-load 'evil
  `(evil-define-key 'normal dired-mode-map "`" 'dired-up-directory))


(defun thi::directorychooser ()
  "Use ido to select a recently used directory from the `thi::directory-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (dired
     (ido-completing-read "Directory open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  thi::directory-list)
                          nil t))))
(global-set-key [f12] 'thi::directorychooser)
