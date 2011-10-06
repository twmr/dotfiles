(setq safe-local-variable-values (quote ((TeX-master . t))))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; make auctex aware of the multi-file document structure
(setq-default TeX-master nil)

;;The following makes C-c-c not ask, just do the default action. Adds C-c-a for asking
(setq TeX-command-force "")
(add-hook 'LaTeX-mode-hook
'(lambda()
(define-key LaTeX-mode-map "\C-c\C-a" ; 'a' for ask, change to anything you want
(lambda (arg) (interactive "P")
(let ((TeX-command-force nil))
(TeX-command-master arg))))))

;;Inserts {} automaticly on _ and ^
(setq TeX-electric-sub-and-superscript t)


(defun turn-on-outline-minor-mode ()
  (outline-minor-mode 1))

(defun turn-on-visual-line-mode ()
  (visual-line-mode 1))

(defun turn-on-longlines-mode ()
  (longlines-mode 1))

(defun compilation-latex-stuff ()
  (interactive)
  (when (string-match "tudadoc" buffer-file-name)
    ;; (make-local-variable 'compile-command)
    ;; (setq 'compile-commnad "cd /home/thomas/gitrepos/tudadoc && make")
    (set (make-local-variable 'compilation-read-command) nil)
    (set (make-local-variable 'compile-command)
         "cd /home/thomas/gitrepos/tudadoc && make")))

(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-visual-line-mode)
(add-hook 'latex-mode-hook 'turn-on-visual-line-mode)
(add-hook 'LaTeX-mode-hook 'turn-off-auto-fill)
(add-hook 'latex-mode-hook 'turn-off-auto-fill)
(add-hook 'LaTeX-mode-hook 'compilation-latex-stuff)
(add-hook 'latex-mode-hook 'compilation-latex-stuff)
;; (add-hook 'LaTeX-mode-hook 'turn-on-longlines-mode)
;; (add-hook 'latex-mode-hook 'turn-on-longlines-mode)
;;(setq outline-minor-mode-prefix "\C-c\C-o") ; Or something else


(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-use-external-file-finders t)
(setq reftex-external-file-finders
      '(("tex" . "kpsewhich -format=.tex %f")
        ("bib" . "kpsewhich -format=.bib %f")))

(add-hook 'outline-minor-mode-hook
          (lambda ()
            (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
            (define-key outline-minor-mode-map [(shift tab)] 'org-global-cycle)))
(add-hook 'outline-mode-hook
          (lambda ()
            (define-key outline-mode-map [(tab)] 'org-cycle)
            (define-key outline-mode-map [(shift tab)] 'org-global-cycle)))
