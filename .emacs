(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style (quote ((c-mode . "stroustrup") (c++-mode . "stroustrup") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((TeX-master . t))))
 '(tool-bar-mode nil))

(set-default-font "Inconsolata Bold 10")

(setq auto-mode-alist (cons '("\\.F90" . f90-mode) auto-mode-alist))

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'before-save-hook 'delete-trailing-whitespace)