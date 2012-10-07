(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-input-ignoredups t)
 '(comint-prompt-read-only t)
 '(custom-safe-themes (quote ("5d039e03dea6910a87bb3ba507c92f2d30672ef6016e4612f05514bc3cdacc85" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "a16379c0d5f9144e4e734a50d851b2347e762fe2a9ecd518377a497c853fb66f" "e28aa80863908e5d4f155546cda4dfe7bcf91e1c" "47205295626f777c037e6bbbbe0ce28cb941762e" default)))
 '(evil-mode-line-format (quote before))
 '(flymake-cursor-error-display-delay 0.4)
 '(flymake-gui-warnings-enabled nil)
 '(fringe-mode (quote (5 . 5)) nil (fringe))
 '(ido-auto-merge-delay-time 2.7)
 '(ido-enable-tramp-completion nil)
 '(ido-use-faces t)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-status-buffer-switch-function (quote switch-to-buffer))
 '(matlab-indent-level 2)
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . default) ("pdf" . "evince %s"))))
 '(org-log-done nil)
 '(preview-default-option-list (quote ("displaymath" "floats" "graphics" "textmath" "showlabels")))
 '(preview-scale-function 1.5)
 '(preview-transparent-color (quote (highlight :background)))
 '(safe-local-variable-values (quote ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t) (TeX-master . t))))
 '(viper-emacs-state-mode-list (quote (Custom-mode dired-mode efs-mode tar-mode browse-kill-ring-mode recentf-mode recentf-dialog-mode occur-mode mh-folder-mode gnus-group-mode gnus-summary-mode completion-list-mode help-mode Info-mode Buffer-menu-mode compilation-mode rcirc-mode jde-javadoc-checker-report-mode view-mode vm-mode vm-summary-mode magit-key-mode)))
 '(viper-vi-state-mode-list (quote (fundamental-mode makefile-mode awk-mode m4-mode xrdb-mode winmgr-mode autoconf-mode cvs-edit-mode html-mode html-helper-mode emacs-lisp-mode lisp-mode lisp-interaction-mode jde-mode java-mode cc-mode c-mode c++-mode objc-mode fortran-mode f90-mode basic-mode bat-mode asm-mode prolog-mode flora-mode sql-mode text-mode indented-text-mode tex-mode latex-mode bibtex-mode ps-mode diff-mode idl-mode perl-mode cperl-mode javascript-mode tcl-mode python-mode sh-mode ksh-mode csh-mode gnus-article-mode mh-show-mode ada-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-first-match ((t (:foreground "#ad7f8a" :weight bold))))
 '(ido-subdir ((((min-colors 88) (class color)) (:foreground "DarkGoldenrod1"))))
 '(magit-item-highlight ((t (:inverse-video nil))) t))
 ;; '(tabbar-button ((t (:foreground "#ad7f8a"))))
 ;;'(tabbar-selected ((t (:background "DarkGoldenrod1" :foreground "Black"))))
 ;;'(tabbar-separator ((t (:height 0.5))))
 ;;'(tabbar-unselected ((t nil))))
