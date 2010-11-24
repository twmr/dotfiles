(setq inhibit-startup-screen t)

;;; My location for external packages.
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;:::::::::::::::::::::::::::::::::::::::::::::::
;: Appearance
;:::::::::::::::::::::::::::::::::::::::::::::::

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

(tool-bar-mode -1)

(set-scroll-bar-mode nil) ; replace 'right with 'left to place it to the left
;;default font is now set in .Xdefaults
;;(set-default-font "ProFont-9")

;:::::::::::::::::::::::::::::::::::::::::::::::
;: Logical Behavour
;:::::::::::::::::::::::::::::::::::::::::::::::

;; whitespace fixes
(add-to-list 'load-path "~/.emacs.d/ethan-wspace/lisp")
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)

(setq c-default-style (quote ( (c-mode . "stroustrup")
                               (c++-mode . "stroustrup")
                               (java-mode . "java")
                               (awk-mode . "awk")
                               (other . "gnu") )))

(add-to-list 'load-path
             "~/.emacs.d/site-lisp/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/site-lisp/yasnippet-0.6.1c/snippets")


;; set linux c-style if filename or directory contains the string linux
(defun maybe-linux-style ()
  (when (and buffer-file-name
             (string-match "linux" buffer-file-name))
    (c-set-style "Linux")))
(add-hook 'c-mode-hook 'maybe-linux-style)

(setq safe-local-variable-values (quote ((TeX-master . t))))

;; follow sylinks to source files in version controlled systems
(setq vc-follow-symlinks t)

;; the compilation buffer will scroll automatically to follow the
;; output as it comes in.
(setq compilation-scroll-output t)
;; empowers the Del key to delete all whitespace to the left of the
;; point.
(setq c-toggle-hungry-state t)

;; automatic identation
(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

;; Go into proper mode according to file extension
(setq auto-mode-alist
      (append '(("\\.C$"    . c++-mode)
        ("\\.cc$"   . c++-mode)
        ("\\.cpp$"  . c++-mode)
        ("\\.cxx$"  . c++-mode)
        ("\\.hxx$"  . c++-mode)
        ("\\.h$"    . c++-mode)
        ("\\.hh$"   . c++-mode)
        ("\\.idl$"  . c++-mode)
        ("\\.ipp$"  . c++-mode)
        ("\\.c$"    . c-mode)
        ("\\.ma?k\\'" . makefile-mode)
        ("\\(M\\|m\\|GNUm\\)akefile\\(\\.in\\)?" . makefile-mode)
        ("\\.pl$"   . perl-mode)
        ("\\.pm$"   . perl-mode)
        ("\\.java$" . java-mode)
        ("\\.txt$"  . text-mode)
        ("\\.tex$" . latex-mode)
        ("\\.sty$" . latex-mode)
        ("\\.bbl$" . latex-mode)
        ("\\.html$" . html-helper-mode)
        ("\\.el\\'" . emacs-lisp-mode)
        ("\\.texinfo\\'" . texinfo-mode)
        ("\\.texi\\'" . texinfo-mode)
        ("\\.s\\'" . asm-mode)
        ("\\.S\\'" . asm-mode)
        ("\\.asm\\'" . asm-mode)
        ("\\.F90\\'" . f90-mode)
        ("ChangeLog\\'" . change-log-mode)
        ("change\\.log\\'" . change-log-mode)
        ("changelo\\'" . change-log-mode)
        ("ChangeLog\\.[0-9]+\\'" . change-log-mode)
        ("changelog\\'" . change-log-mode)
        ("changelog\\.[0-9]+\\'" . change-log-mode)
        ("\\$CHANGE_LOG\\$\\.TXT" . change-log-mode)
        ("\\.tar\\'" . tar-mode)
        ("\\.\\(arc\\|zip\\|lzh\\|zoo\\|jar\\)\\'" . archive-mode)
        ("\\.\\(ARC\\|ZIP\\|LZH\\|ZOO\\|JAR\\)\\'" . archive-mode)
        ("[]>:/\\]\\..*emacs\\'" . emacs-lisp-mode)
        ("\\`\\..*emacs\\'" . emacs-lisp-mode)
        ("[:/]_emacs\\'" . emacs-lisp-mode)
        ("\\.gp$" . gnuplot-mode)
        )
              auto-mode-alist))

(add-hook 'text-mode-hook 'turn-on-auto-fill)


;:::::::::::::::::::::::::::::::::::::::::::::::
;: LaTeX Stuff
;:::::::::::::::::::::::::::::::::::::::::::::::

(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; make auctex aware of the multi-file document structure
(setq-default TeX-master nil)
