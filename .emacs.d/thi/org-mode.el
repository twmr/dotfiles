(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-block nil :inherit 'fixed-pitch)

(setq org-agenda-files (quote ("~/Dropbox/notes-org/studium.org"
                               "~/Dropbox/notes-org/emacs.org")))

;; fontify src blocks, doesn't work with fixed-pitch blocks :(
;;(setq org-src-fontify-natively t)

;; (setq org-hide-leading-stars t)
(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . delete-window)
                                      ("1" . delete-other-windows)
                                      ("2" . split-window-vertically)
                                      ("3" . split-window-horizontally)
                                      ("h" . hide-other)
                                      ("k" . org-kill-note-or-show-branches)
                                      ("r" . org-reveal)
                                      ("s" . org-save-all-org-buffers)
                                      ("z" . org-add-note)
                                      ("c" . self-insert-command)
                                      ("C" . self-insert-command)
                                      ("J" . org-clock-goto))))
;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
