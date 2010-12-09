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

(tool-bar-mode -1)

(set-scroll-bar-mode nil) ; replace 'right with 'left to place it to the left

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

;:::::::::::::::::::::::::::::::::::::::::::::::
;: Logical Behavour
;:::::::::::::::::::::::::::::::::::::::::::::::

;:::::::::::::::::::::::::::::::::::::::::::::::
;; CC-MODE
(require 'cc-mode)
(setq c-default-style (quote ( (c-mode . "stroustrup")
                               (c++-mode . "stroustrup")
                               (java-mode . "java")
                               (awk-mode . "awk")
                               (other . "gnu") )))

;; set linux c-style if filename or directory contains the string
;; linux
(defun maybe-linux-style ()
  (when (and buffer-file-name
             (string-match "linux" buffer-file-name))
    (c-set-style "Linux")))
(add-hook 'c-mode-hook 'maybe-linux-style)

;; do not create newlines for electric keys if the following line is
;; nonblank
(defun c-semi&comma-no-newlines-before-nonblanks ()
  (save-excursion
    (if (and (eq last-command-char ?\;)
           (zerop (forward-line 1))
           (not (looking-at "^[ \t]*$")))
        'stop
      nil)))
(setq c-hanging-semi&comma-criteria
  (cons 'c-semi&comma-no-newlines-before-nonblanks
        c-hanging-semi&comma-criteria))

;; due to no-newlines-before-nonblanks I need this from the
;; google-styleguide
(defun google-make-newline-indent ()
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map [ret] 'newline-and-indent))
(add-hook 'c-mode-common-hook 'google-make-newline-indent)


;; automatic identation (auto mode -> /a in modeline) and empowers the
;; Del key to delete all whitespace to the left of the point (hungry
;; mode -> h in modeline) for C-based languages (including java) for
;; current keybindings see - cc-mode manual
(add-hook 'c-mode-common-hook
          '(lambda () (c-toggle-auto-hungry-state 1)))


;; makes delete map to hungry mode
(defun thi-map-delete-hungry ()
  (define-key c-mode-base-map [delete] 'c-hungry-backspace))
(add-hook 'c-mode-common-hook 'thi-map-delete-hungry)


;:::::::::::::::::::::::::::::::::::::::::::::::
;; Vi Mode : Viper and Vimpulse

(setq vimpulse-want-vi-keys-in-apropos nil)
(setq vimpulse-want-vi-keys-in-buffmenu nil)
(setq vimpulse-want-vi-keys-in-dired nil)
(setq vimpulse-want-vi-keys-in-help nil)
(setq vimpulse-want-vi-keys-in-Info nil)
(setq vimpulse-want-change-undo nil)

;;(add-to-list 'load-path "~/.emacs.d/undo-tree")
(add-to-list 'load-path "~/.emacs.d/vimpulse")
(require 'vimpulse)

;; redefine (equiv. of the famous `map Y y$')
(defun viper-yank-line (arg)
  "Delete to the end of line."
  (interactive "P")
  (viper-goto-eol (cons arg ?y)))


;; ugly ugly ugly fix
(add-hook 'viper-vi-state-hook
          '(lambda () (set-face-background 'mode-line "red")))
(add-hook 'viper-emacs-state-hook
          '(lambda ()
             (set-face-background 'mode-line "#2e3436")
             (set-face-foreground 'mode-line "#eeeeec")))


(setq-default viper-auto-indent t)

(setq viper-change-notification-threshold 0
      viper-expert-level 5
      viper-inhibit-startup-message t
      viper-vi-style-in-minibuffer nil
      viper-want-ctl-h-help t)

(setq-default viper-ex-style-editing nil)
(setq-default viper-ex-style-motion nil)
(setq-default viper-delete-backwards-in-replace t)


;;(setq viper-emacs-state-id
;;      (concat (propertize "<EMACS>" 'face 'isearch) " "))

;; Show when minibuffer is in Emacs mode.
;;(when (facep 'viper-minibuffer-emacs)
;;  (set-face-foreground 'viper-minibuffer-emacs "red")
;;  (set-face-background 'viper-minibuffer-emacs "black"))


;; whitespace fixes
(add-to-list 'load-path "~/.emacs.d/ethan-wspace/lisp")
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)


(add-to-list 'load-path
             "~/.emacs.d/site-lisp/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/site-lisp/yasnippet-0.6.1c/snippets")

;; keyboard scroll one line at a time
;; http://www.emacswiki.org/emacs/SmoothScrolling
;; -------------------------------------
;; following commands do not work - still no smooth scrolling
;; -- (setq scroll-step 1)
;; -- (setq scroll-conservatively 10000)
;; however this works: (still not as smooth as in vim :( )
(require 'smooth-scrolling)

;; follow sylinks to source files in version controlled systems
(setq vc-follow-symlinks t)

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

;; Icicles - completion
(add-to-list 'load-path "~/.emacs.d/icicles/")
(require 'icicles)

;:::::::::::::::::::::::::::::::::::::::::::::::
;: Git Stuff
;:::::::::::::::::::::::::::::::::::::::::::::::

;; FIXME study emacs Git packages
;;(add-to-list 'load-path ".../git/contrib/emacs")
;;(require 'git)
;;(require 'git-blame)


;:::::::::::::::::::::::::::::::::::::::::::::::
;: LaTeX Stuff
;:::::::::::::::::::::::::::::::::::::::::::::::

(setq safe-local-variable-values (quote ((TeX-master . t))))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; make auctex aware of the multi-file document structure
(setq-default TeX-master nil)
