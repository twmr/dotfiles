(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-input-ignoredups t)
 '(comint-prompt-read-only t)
 '(evil-emacs-state-modes (quote (archive-mode bbdb-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode ert-results-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode ibuffer-mode jde-javadoc-checker-report-mode magit-commit-mode magit-diff-mode magit-key-mode magit-log-mode magit-mode magit-reflog-mode magit-show-branches-mode magit-stash-mode magit-status-mode magit-wazzup-mode mh-folder-mode monky-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode help-mode grep-mode)))
 '(evil-mode-line-format (quote before))
 '(evil-motion-state-modes (quote (apropos-mode Buffer-menu-mode calendar-mode color-theme-mode command-history-mode compilation-mode dictionary-mode Info-mode speedbar-mode undo-tree-visualizer-mode view-mode)))
 '(flymake-cursor-error-display-delay 0.4)
 '(flymake-gui-warnings-enabled nil)
 '(fringe-mode (quote (5 . 5)) nil (fringe))
 '(ido-auto-merge-delay-time 2.7)
 '(ido-enable-tramp-completion nil)
 '(ido-max-window-height 30)
 '(ido-use-faces t)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-status-buffer-switch-function (quote switch-to-buffer))
 '(matlab-indent-level 2)
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . default) ("pdf" . "evince %s"))))
 '(org-log-done nil)
 '(preview-default-option-list (quote ("displaymath" "floats" "graphics" "textmath" "showlabels")))
 '(preview-scale-function 1.5)
 '(preview-transparent-color (quote (highlight :background)))
 '(recentf-exclude (quote ("COMMIT_EDITMSG" "session.*")))
 '(safe-local-variable-values (quote ((nxml-child-indent . 4) (TeX-master . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-first-match ((t (:foreground "#ad7f8a" :weight bold))))
 '(ido-subdir ((((min-colors 88) (class color)) (:foreground "DarkGoldenrod1"))))
 '(tabbar-default ((t (:background "gray17" :foreground "gray" :height 0.9 :family "Mono"))) t)
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "gold" :box (:line-width 1 :color "white" :style pressed-button)))) t))
 ;; '(tabbar-button ((t (:foreground "#ad7f8a"))))
 ;;'(tabbar-selected ((t (:background "DarkGoldenrod1" :foreground "Black"))))
 ;;'(tabbar-separator ((t (:height 0.5))))
 ;;'(tabbar-unselected ((t nil))))
