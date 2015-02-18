;;; It's Magit! An Emacs mode for Git.

(setq magit-repo-dirs `(,"~/gitrepos" "~/.emacs.d" "~/.emacs.d/el-get/el-get"))
(setq magit-commit-signoff nil) ;; TODO set this to nil only for IMS repos
(setq magit-remote-ref-format 'remote-slash-branch)
(setq magit-completing-read-function 'magit-ido-completing-read)
(setq magit-save-some-buffers nil)

(add-hook 'magit-mode-hook
          (lambda ()
            (setq yas/dont-activate t)))
            ;; (magit-filenotify-mode 1)))
;; (add-hook 'magit-mode-hook 'turn-on-magit-svn)

;; (define-key magit-mode-map (kbd "M-3") 'split-window-horizontally) ; was magit-show-level-3
;; (define-key magit-mode-map (kbd "M-2") 'split-window-vertically)   ; was magit-show-level-2
;; (define-key magit-mode-map (kbd "M-1") 'delete-other-windows)      ; was magit-show-level-1

;; TODO reordering (untracked-files) doesn't seem to work!??!
(remove-hook 'magit-status-sections-hook 'magit-insert-stashes)
;; (remove-hook 'magit-status-sections-hook 'magit-insert-untracked-files)
(remove-hook 'magit-status-sections-hook 'magit-insert-status-local-line)
(remove-hook 'magit-status-sections-hook 'magit-insert-status-remote-line)
(remove-hook 'magit-status-sections-hook 'magit-insert-status-head-line)
(remove-hook 'magit-status-sections-hook 'magit-insert-status-tags-line)
(remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-commits)
(remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-commits)
;; (add-hook 'magit-status-sections-hook 'magit-insert-untracked-files t)

(defun magit-pull ()
  (interactive)
  (magit-run-git-async "pull" "--rebase" "-v"))

(defun magit-toggle-section () ;; overwrite magit-toggle-section function
  "Toggle hidden status of current section."
  (interactive)
  (if (eq 'hunk (first (magit-section-context-type (magit-current-section))))
      (magit-toggle-file-section)
    (magit-section-hideshow
     (lambda (s)
       (magit-section-set-hidden s (not (magit-section-hidden s)))))))

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
      ad-do-it
        (delete-other-windows))

(defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
      (interactive)
        (kill-buffer)
          (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; Add a "latest commits" section
;; (magit-define-section-jumper latest   "Latest commits")
;; (defun vbe:magit-insert-latest-commits ()
;;   (magit-git-section 'latest "Latest commits:"
;;                      (apply-partially 'magit-wash-log 'unique)
;;                      "log" "--format=format:* %h %s"
;;                      (magit-diff-abbrev-arg)
;;                      "HEAD~5..HEAD"))
;; (add-to-list 'magit-status-sections-hook 'vbe:magit-insert-latest-commits t)
