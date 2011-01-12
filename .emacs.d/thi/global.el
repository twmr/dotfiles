
(defvar user-cache-file-dir (expand-file-name (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs/")))

;; don't clutter dirs with backup files - use this dir instead
;; maybe not useful for files inside dropbox (add exception ?)
(setq backup-by-copying t)
(setq backup-dir-alist '(("." . ,user-cache-file-dir)))
(setq auto-save-list-file-prefix
      (concat user-cache-file-dir ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-cache-file-dir t)))

;;TODO move into auto-complete local file
(setq ac-comphist-file (concat user-cache-file-dir "ac-comphist.dat"))

;:::::::::::::::::::::::::::::::::::::::::::::::
;: Appearance
;:::::::::::::::::::::::::::::::::::::::::::::::

(setq inhibit-startup-screen t)

(require 'linum)
(global-linum-mode 1)
;;(set-face-foreground 'linum "white")
;;(set-face-background 'linum "black")

(setq column-number-mode t)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

;;default font is now set in .Xdefaults
;;(set-default-font "ProFont-9")

;; highlight the current line
;; (faces are set in color-theme)
;;optional: set a custom face, so we can
;; recognize from the normal marking (selection)
;;(defface hl-line '((t (:background "Gray")))
;;  "Face to use for `hl-line-face'." :group 'hl-line)
;;(setq hl-line-face 'hl-line)
(global-hl-line-mode t) ; turn it on for all modes by default

;:::::::::::::::::::::::::::::::::::::::::::::::
;: Keymappings
;:::::::::::::::::::::::::::::::::::::::::::::::

(global-set-key [f3] 'dabbrev-expand)
(global-set-key [f4] 'query-replace)
(global-set-key "\M-#" 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

;; replace buffermenu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;:::::::::::::::::::::::::::::::::::::::::::::::
;: Logical Behavour
;:::::::::::::::::::::::::::::::::::::::::::::::

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; If there is a tab, make it the size of 2 spaces
(setq-default tab-width 2)

;; the compilation buffer will scroll automatically to follow the
;; output as it comes in.
;; - is not a good idea if you want to see and jump to the firs error
;;   so comment this out
;;(setq compilation-scroll-output t)

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
