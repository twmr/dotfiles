;;; Recent Files

;; recentf-save-file has to be before (require ..) otherwise you have
;; to load the new file manually with recentf-load-list
(setq recentf-save-file (concat thi::cache-file-dir "/recentf"))

(require 'recentf)
(recentf-mode 1)
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-menu-items 60)
(setq recentf-max-saved-items 500)
(setq recentf-exclude '("COMMIT_EDITMSG"
                        ".newsrc-dribble"
                        "/session\\."
                        ".*-autoloads\\.el\\'"
                        "/elpa/"
                        ))

;; TODO merge recentf-ido-find-file and ...-find-dir function into 1 general
;; function

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (file-assoc-list
          (mapcar (lambda (x)
                    (cons (replace-regexp-in-string
                           home "~" x) x))
                  recentf-list))
         (filename-list
          (remove-duplicates (mapcar #'car file-assoc-list)
                             :test #'string=))
         (filename (ivy-completing-read "Choose recent file: "
                                        filename-list nil t)))
    (when filename
      (find-file (cdr (assoc filename file-assoc-list))))))


(defun recentf-ido-find-dir ()
  "Find a recent directory using Ido."
  ;; TODO temporarily unset the ido-directory face
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (dir-assoc-list
          (mapcar (lambda (x)
                    (cons (replace-regexp-in-string
                           home "~" (file-name-directory x)) x))
                  recentf-list))
         (dirname-list
          (remove-duplicates (mapcar #'car dir-assoc-list)
                             :test #'string=)))
    (dired (ivy-completing-read "Choose recent dir: "
                                dirname-list
                                nil
                                t))))

;; TODO somehow if this is activated C-e in eshell and ansi-term triggers
;; ide-my-edit-input - whyyy??
;;(define-key recentf-mode-map (kbd "C-e") 'ido-my-edit-input)
