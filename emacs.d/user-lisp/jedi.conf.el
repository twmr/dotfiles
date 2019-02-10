;; (setq jedi:setup-keys t) ;; this var is obsolete as of jedi 0.1.3
(setq jedi:complete-on-dot t)

;; annoying function signature tooltip no longer gets displayed :)
(defun jedi:handle-post-command ())

(defvar jedi:goto-stack '())
(defun jedi:jump-to-definition ()
  (interactive)
  (add-to-list 'jedi:goto-stack
               (list (buffer-name) (point)))
  (jedi:goto-definition))

(defun jedi:jump-back ()
  (interactive)
  (let ((p (pop jedi:goto-stack)))
    (if p (progn
            (switch-to-buffer (nth 0 p))
            (goto-char (nth 1 p))))))
