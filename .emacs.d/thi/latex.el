(setq safe-local-variable-values (quote ((TeX-master . t))))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; make auctex aware of the multi-file document structure
(setq-default TeX-master nil)


(defun turn-on-outline-minor-mode ()
  (outline-minor-mode 1))

;;FIXME use vendor function for this
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/org-mode/lisp"))
(require 'org-install)
(require 'org-habit)

(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
;;(setq outline-minor-mode-prefix "\C-c\C-o") ; Or something else

(add-hook 'outline-minor-mode-hook
          (lambda ()
            (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
            (define-key outline-minor-mode-map [(shift tab)] 'org-global-cycle)))
(add-hook 'outline-mode-hook
          (lambda ()
            (define-key outline-mode-map [(tab)] 'org-cycle)
            (define-key outline-mode-map [(shift tab)] 'org-global-cycle)))
