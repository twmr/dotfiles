(setq inhibit-startup-screen t)

;;; My location for external packages.
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(require 'color-theme)
(require 'color-theme-tango-2)
(color-theme-tango-2)

(require 'linum)
(global-linum-mode 1)
(setq column-number-mode t)
;;(require 'color-theme-tango)
;;(color-theme-tango)
;;(require 'color-theme-subdued)
;;(color-theme-subdued)

;; whitespace fixes
;;  -- ws-trim.el --
;;(show-ws-toggle-show-trailing-whitespace)
;;(show-ws-toggle-show-tabs)
;;  -- ethan-wspace --
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)

(setq c-default-style (quote ( (c-mode . "stroustrup")
                               (c++-mode . "stroustrup")
                               (java-mode . "java")
                               (awk-mode . "awk")
                               (other . "gnu") )))
(setq safe-local-variable-values (quote ((TeX-master . t))))

(tool-bar-mode -1)
(set-scroll-bar-mode 'right)   ; replace 'right with 'left to place it to the left
(set-default-font "DejaVu Sans Mono 8")

(setq compilation-scroll-output t)
(setq auto-mode-alist (cons '("\\.F90" . f90-mode) auto-mode-alist))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil) ;; make auctex aware of the multi-file document structure
