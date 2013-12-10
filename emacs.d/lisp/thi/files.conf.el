(setq safe-local-variable-values '((encoding . utf-8)))
;;FIXME The following line leads to recentf file modified do u want to save it ...
;; (setq backup-directory-alist '(("." . '(concat thi::cache-file-dir "/backups/"))))
(mkdir (concat thi::cache-file-dir "/backups") t)
(setq backup-directory-alist `(("." . ,(concat thi::cache-file-dir
                                               "backups"))))
(setq kept-old-versions 5)
(setq delete-old-versions t)
(setq backup-by-copying t)
(setq version-control t)

;;TODO study these variables
;; (setq auto-save-list-file-prefix
      ;; (concat thi::cache-file-dir ".auto-saves-"))
;; (setq auto-save-file-name-transforms
      ;; `((".*" ,thi::cache-file-dir t)))
