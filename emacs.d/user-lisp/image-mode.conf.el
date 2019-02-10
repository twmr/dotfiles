(defun thi::go-back-to-dired ()
  (interactive)
  (dired-at-point default-directory))

(define-key image-mode-map (kbd "<right>") 'image-next-file)
(define-key image-mode-map (kbd "<left>") 'image-previous-file)
(define-key image-mode-map (kbd "<DEL>") 'thi::go-back-to-dired)
