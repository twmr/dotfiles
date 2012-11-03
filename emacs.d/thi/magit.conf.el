;;; It's Magit! An Emacs mode for Git.

;; disable vc atm as I don't think that I need it
;; TODO VC vs. magit
(setq vc-handled-backends nil)

;; follow sylinks to source files in version controlled systems
;; (setq vc-follow-symlinks t)

(setq magit-repo-dirs `(,"~/gitrepos" "~/.emacs.d" "~/.emacs.d/el-get/el-get"))
(setq magit-commit-signoff nil) ;; TODO set this to nil only for IMS repos
(setq magit-remote-ref-format 'remote-slash-branch)
(setq magit-completing-read-function 'magit-ido-completing-read)
(setq magit-save-some-buffers nil)

(add-hook 'magit-mode-hook
          (lambda () (setq yas/dont-activate t)))
(add-hook 'magit-mode-hook 'turn-on-magit-svn)

;; (define-key magit-mode-map (kbd "M-3") 'split-window-horizontally) ; was magit-show-level-3
;; (define-key magit-mode-map (kbd "M-2") 'split-window-vertically)   ; was magit-show-level-2
;; (define-key magit-mode-map (kbd "M-1") 'delete-other-windows)      ; was magit-show-level-1

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
