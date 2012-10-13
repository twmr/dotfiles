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
 '(safe-local-variable-values (quote ((c-offsets-alist (inexpr-class . +) (inexpr-statement . +) (lambda-intro-cont . +) (inlambda . c-lineup-inexpr-block) (template-args-cont c-lineup-template-args +) (incomposition . +) (inmodule . +) (innamespace . +) (inextern-lang . +) (composition-close . 0) (module-close . 0) (namespace-close . 0) (extern-lang-close . 0) (composition-open . 0) (module-open . 0) (namespace-open . 0) (extern-lang-open . 0) (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +) (objc-method-args-cont . c-lineup-ObjC-method-args) (objc-method-intro . [0]) (friend . 0) (cpp-define-intro c-lineup-cpp-define +) (cpp-macro-cont . +) (cpp-macro . [0]) (inclass . +) (stream-op . c-lineup-streamop) (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist) (arglist-cont c-lineup-gcc-asm-reg 0) (arglist-intro . +) (catch-clause . 0) (else-clause . 0) (do-while-closure . 0) (label . 2) (access-label . -) (substatement-label . 2) (substatement . +) (statement-case-open . 0) (statement-case-intro . +) (statement-block-intro . +) (statement-cont . +) (statement . 0) (brace-entry-open . 0) (brace-list-entry . 0) (brace-list-intro . +) (brace-list-close . 0) (brace-list-open . 0) (block-close . 0) (inher-cont . c-lineup-multi-inher) (inher-intro . +) (member-init-cont . c-lineup-multi-inher) (member-init-intro . +) (annotation-var-cont . +) (annotation-top-cont . 0) (topmost-intro-cont . c-lineup-topmost-intro-cont) (topmost-intro . 0) (knr-argdecl . 0) (func-decl-cont . +) (inline-close . 0) (inline-open . +) (class-close . 0) (class-open . 0) (defun-block-intro . +) (defun-close . 0) (defun-open . 0) (string . c-lineup-dont-change) (arglist-close . c-lineup-arglist) (substatement-open . 0) (case-label . 0) (block-open . 0) (c . 1) (comment-intro . 0) (knr-argdecl-intro . -)) (c-cleanup-list scope-operator brace-else-brace brace-elseif-brace brace-catch-brace empty-defun-braces list-close-comma defun-close-semi) (c-hanging-semi&comma-criteria c-semi&comma-no-newlines-before-nonblanks) (c-hanging-colons-alist (member-init-intro before) (inher-intro) (case-label after) (label after) (access-label after)) (c-hanging-braces-alist (substatement-open after) (brace-list-open after) (brace-entry-open) (defun-open after) (class-open after) (inline-open after) (block-open after) (block-close . c-snug-do-while) (statement-case-open after) (substatement after)) (c-comment-only-line-offset . 0) (c-tab-always-indent . t) (TeX-master . t))))
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
