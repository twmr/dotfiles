(setq safe-local-variable-values (quote ((TeX-master . t))))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; make auctex aware of the multi-file document structure
(setq-default TeX-master nil)

;;The following makes C-c-c not ask, just do the default action. Adds C-c-a for asking
(setq TeX-command-force "")
(add-hook 'LaTeX-mode-hook
          (lambda ()
            ; 'a' for ask, change to anything you want
            (define-key LaTeX-mode-map "\C-c\C-a"
              (lambda (arg) (interactive "P")
                (let ((TeX-command-force nil))
                  (TeX-command-master arg))))))

;;Inserts {} automaticly on _ and ^
(setq TeX-electric-sub-and-superscript t)


(defun compilation-latex-stuff ()
  (interactive)
  (when (string-match "tudadoc" buffer-file-name)
    ;; (make-local-variable 'compile-command)
    ;; (setq 'compile-commnad "cd /home/thomas/gitrepos/tudadoc && make")
    (set (make-local-variable 'compilation-read-command) nil)
    (set (make-local-variable 'compile-command)
         "cd /home/thomas/gitrepos/tudadoc && make")))

(dolist (hook '(LaTeX-mode-hook latex-mode-hook))
  (add-hook hook
            (lambda ()
              (turn-on-outline-minor-mode)
              (turn-on-visual-line-mode)
              (turn-off-auto-fill)
              ;; (turn-on-longlines-mode)
              (compilation-latex-stuff))))
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

;; from Matthias
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(eval-after-load "tex"
 '(add-to-list 'TeX-expand-list
              '("%u" okular-make-url)))

(defun okular-make-url () (concat
               "file://"
               (expand-file-name (funcall file (TeX-output-extension) t)
                       (file-name-directory (TeX-master-file)))
               "#src:"
               (TeX-current-line)
               (TeX-current-file-name-master-relative)))

(setq TeX-view-program-list '(("Okular" "okular --unique %u")))
(setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular")))
