(eval-after-load 'evil
  (progn
  `(evil-define-key 'normal dired-mode-map (kbd "<return>")
     'dired-find-alternate-file) ; was dired-advertised-find-file
  `(evil-define-key 'normal dired-mode-map (kbd "`")
     '(lambda () (interactive) (find-alternate-file ".."))))
)
;;http://www.ergoemacs.org/emacs/emacs_dired_tips.html

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
