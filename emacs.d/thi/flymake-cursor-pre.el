;; stuff in this file is executed before flymake-cursor is loaded

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (defun flymake-get-tex-args (file-name)
    (list "chktex" (list "-q" "-v0" file-name)))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-display-warning (warning)
  "Display a warning to the user, using message"
  (message warning))
