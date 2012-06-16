(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-block nil :inherit 'fixed-pitch)

(setq org-agenda-files (quote ("~/gitrepos/orgnotes/studium.org"
                               "~/gitrepos/orgnotes/laserPA.org"
                               "~/gitrepos/orgnotes/diplomarbeit.org"
                               "~/gitrepos/orgnotes/physik.org"
                               "~/gitrepos/orgnotes/emacs.org")))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (gnuplot . t)
   (python . t)))


(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;; this will turn off asking for a confirmation
(setq org-confirm-babel-evaluate nil)

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
      '("~/gitrepos/orgnotes/refs.bib"))

(setq org-link-abbrev-alist
      '(("bib" . "~/gitrepos/orgnotes/refs.bib::%s")
        ("diplomarbeit" . "~/gitrepos/orgnotes/diplomarbeit.org::#%s")
        ("papers" . "~/Dropbox/research/%s.pdf")))

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

         ;; this prevent reftex asking for the master file
         (make-local-variable 'TeX-master)
         (setq TeX-master t)

         (reftex-parse-all)

         ;add a custom reftex cite format to insert links
         (reftex-set-cite-format
          '((?b . "[[bib:%l][%l-bib]]")
            (?n . "[[diplomarbeit:%l][%l-diplomarbeit]]")
            (?p . "[[papers:%l][%l-paper]]")
            (?c . "\\cite{%l}")
            (?t . "%t")
            (?h . "\n** [[papers:%l][%t]]\n  :PROPERTIES:\n  :Custom_ID: %l\n  :END:\n")))))

  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)


;;beamer


;; allow for export=>beamer by placing

;; #+LaTeX_CLASS: beamer in org files
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
  ;; beamer class, for presentations
  '("beamer"
     "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}\n
      \\usepackage{verbatim}\n
      \\newcommand{\\xvec}{\\mathbf{x}}\n
      \\institute{{{{beamerinstitute}}}}\n
      \\subject{{{{beamersubject}}}}\n"

     ("\\section{%s}" . "\\section*{%s}")

     ("\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}"
       "\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}")))

  ;; letter class, for formal letters

  (add-to-list 'org-export-latex-classes

  '("letter"
     "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"

     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
