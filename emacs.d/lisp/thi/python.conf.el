;; ;; the following code does not work when we run emacs from a remove pc (ssh)

;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i")

(setq python-skeleton-autoinsert t)

;; (defvar ac-source-python
;;   '((candidates .
;;                 (lambda ()
;;                   (mapcar (lambda (completion)
;;                              (first (last (split-string completion "\\." t))))
;;                           (python-symbol-completions (python-partial-symbol)))))))


;; (add-hook 'python-mode-hook
;;              (lambda() (setq ac-sources '(ac-source-python))))

;;force loading of python-cell-mode
;; (require 'python-cell)

;; Emacs will automatically insert a new line after “fill-column” number of
;; columns. PEP8 specifies a maximum of 79, but this can be set to a smaller
;; value also, for example 72.  this is used e.g. in M-q (fill-paragraph)
;; FIXME this disables fci-mode and highlight-indentation-mode !??!
;; (add-hook 'python-mode-hook (lambda ()
;;                               (setq-default-fill-column 72)))

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'python-mode-hook 'company-anaconda)
(add-hook 'python-mode-hook #'fci-mode 1)
(add-hook 'python-mode-hook #'highlight-indentation-mode 1)
;; (add-hook 'python-mode-hook #'python-cell-mode 1)
(add-hook 'python-mode-hook #'yas-minor-mode 1)
;; (add-hook 'python-mode-hook #'superword-mode 1)
;; (add-hook 'python-mode-hook #'rainbow-delimiters-mode 1) ;; simply too slow
;; (add-hook 'python-mode-hook #'semantic-mode 1)
;; TODO this does not work ?!??!
;; (add-hook 'python-mode-hook #'turn-on-auto-fill 1)
(add-hook 'python-mode-hook (lambda ()
                              (push '("lambda" . 955) prettify-symbols-alist)))

;; redefine jedi's C-. (jedi:goto-definition)
;; to remember position, and set C-, to jump back
;; (define-key python-mode-map (kbd "C-.") 'jedi:jump-to-definition)
;; (define-key python-mode-map (kbd "C-,") 'jedi:jump-back)
;; (define-key python-mode-map (kbd "C-c d") 'jedi:show-doc)


;; TESTING CODE
;; (font-lock-add-keywords 'python-mode `(("\\<\\(TEST\\)"
;;                                         1 '(:weight bold :overline t) prepend)))
;; (font-lock-add-keywords 'python-mode `((
;;                                         ,(rx line-start (* space)
;;                                              (group (and "#" (or (and "#" (* (not (any "\n"))))
;;                                                                  (and " <" (or "codecell" "markdowncell") ">"))
;;                                                          line-end)))
;;                                         1 '(:weight bold :overline t) prepend)))
