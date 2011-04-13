(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-block nil :inherit 'fixed-pitch)

(setq org-agenda-files (quote ("~/Dropbox/notes-org/studium.org"
                               "~/Dropbox/notes-org/laserPA.org"
                               "~/Dropbox/notes-org/diplomarbeit.org"
                               "~/Dropbox/notes-org/physik.org"
                               "~/Dropbox/notes-org/emacs.org")))

;; fontify src blocks, doesn't work with fixed-pitch blocks :(
;;(setq org-src-fontify-natively t)

;; (setq org-hide-leading-stars t)
;; (setq org-use-speed-commands t)
;; (setq org-speed-commands-user (quote (("0" . delete-window)
;;                                       ("1" . delete-other-windows)
;;                                       ("2" . split-window-vertically)
;;                                       ("3" . split-window-horizontally)
;;                                       ("h" . hide-other)
;;                                       ("k" . org-kill-note-or-show-branches)
;;                                       ("r" . org-reveal)
;;                                       ("s" . org-save-all-org-buffers)
;;                                       ("z" . org-add-note)
;;                                       ("c" . self-insert-command)
;;                                       ("C" . self-insert-command)
;;                                       ("J" . org-clock-goto))))
;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; taken from http://tincman.wordpress.com/2011/01/04/research-paper-management-with-emacs-org-mode-and-reftex/
;;

;; needed because every time when i load a org file a get asked for a master file name ?!?!!
(setq reftex-default-bibliography
      '("~/Dropbox/research/refs.bib"))

(setq org-link-abbrev-alist
      '(("bib" . "~/Dropbox/research/refs.bib::%s")
        ("diplomarbeit" . "~/notes-org/diplomarbeit.org::#%s")
        ("papers" . "~/research/papers/%s.pdf")))

(defun org-mode-reftex-search ()
  ;;jump to the notes for the paper pointed to at from reftex search
  (interactive)
  (org-open-link-from-string (format "[[diplomarbeit:%s]]" (reftex-citation t))))

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
         ;enable auto-revert-mode to update reftex when bibtex file changes on disk
         (global-auto-revert-mode t)

         (reftex-parse-all)

         ;add a custom reftex cite format to insert links
         (reftex-set-cite-format
          '((?b . "[[bib:%l][%l-bib]]")
            (?n . "[[diplomarbeit:%l][%l-diplomarbeit]]")
            (?p . "[[papers:%l][%l-paper]]")
            (?c . "\\cite{%l}")
            (?t . "%t")
            (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)
