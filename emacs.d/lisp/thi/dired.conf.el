;; C-a is nicer in dired if it moves back to start of files
(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(eval-after-load 'evil
  '(progn
     (evil-define-key 'normal dired-mode-map (kbd "<return>")
       'dired-find-alternate-file) ; was dired-advertised-find-file
     (evil-define-key 'normal dired-mode-map (kbd "`")
       '(lambda () (interactive) (find-alternate-file "..")))
     (evil-define-key 'normal dired-mode-map (kbd "<DEL>")
       'dired-up-directory)
     (evil-define-key 'normal dired-mode-map (kbd "C-a")
       'dired-back-to-start-of-files)
  )
)
;;http://www.ergoemacs.org/emacs/emacs_dired_tips.html

(define-key dired-mode-map (kbd "<return>")
  'dired-find-alternate-file) ; was dired-advertised-find-file

(define-key dired-mode-map (kbd "`")
  '(lambda () (interactive) (find-alternate-file "..")))

(define-key dired-mode-map (kbd "<DEL>")
  'dired-up-directory)

(define-key dired-mode-map (kbd "C-a")
  'dired-back-to-start-of-files)
