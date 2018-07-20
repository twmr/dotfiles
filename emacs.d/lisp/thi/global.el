;; "y or n" instead of "yes or no"
(defalias 'yes-or-no-p 'y-or-n-p)

;:::::::::::::::::::::::::::::::::::::::::::::::
;: Appearance
;:::::::::::::::::::::::::::::::::::::::::::::::

(setq inhibit-startup-screen t)
(setq default-indicate-empty-lines nil)

;: LINUM
(when (eq 'thi::theme 'solarized-light)
  (face-spec-set 'linum
                 '((t (:inherit (quote shadow)
                                :background "#d7d7af"
                                :foreground "#87875f"
                                :slant normal
                                :height 66)))))

;; (when window-system
;;   (set-fringe-mode '(8 . 0))
;; Linum format to avoid graphics glitches in fringe
;; (setq linum-format " %4d "))

;; linum should be disabled for certain modes where linenumbers do not
;; make sense
;; http://www.emacswiki.org/emacs/LineNumbers
(setq linum-disabled-modes-list '(compilation-mode
                                  direx-mode
                                  DocView-mode
                                  eshell-mode
                                  git-commit-mode
                                  latex-mode
                                  magit-status-mode
                                  mu4e-compose-mode
                                  org-mode
                                  wl-summary-mode))

;; fill-column-indicator
(setq-default fci-rule-column 80)

;; this function is required otherwise linum-disabled-modes-list is not
;; taken into account
(defun linum-on ()
    (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
              (string-match "*" (buffer-name))
              )
    (linum-mode 1)))
;;(set-face-foreground 'linum "white")
;;(set-face-background 'linum "black")

(set-face-attribute 'highlight nil :underline nil)

(line-number-mode 1)                    ; Show line number
(column-number-mode 1)                  ; Show colum number
(set-scroll-bar-mode 'right)            ; Scrollbar on the right
(scroll-bar-mode -1)                    ; But no scrollbar
(horizontal-scroll-bar-mode -1)         ; But no scrollbar

(size-indication-mode)
;;default font is now set in .Xresources
;; (set-default-font "ProFont-9")

;:::::::::::::::::::::::::::::::::::::::::::::::
;: Logical Behavour
;:::::::::::::::::::::::::::::::::::::::::::::::

;; FIXME temporary fix this because committing in magit takes so long
;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))
;; (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode -1))))

;; automatically adds marks
(with-eval-after-load 'auto-mark
  (global-auto-mark-mode 1))

;; If there is a tab, make it the size of 2 spaces
;; TODO Check if this causes problems with fgallina/python.el
(setq-default tab-width 2)

(dolist (hook '(erc-mode-hook
        LaTeX-mode-hook
        org-mode-hook
        edit-server-start-hook
        markdown-mode-hook))
  (add-hook hook (lambda () (progn
                              (variable-pitch-mode t)))))
                              ;; (wrap-column-mode)))))

;; ibuffer-perspective is not available via package.el
;; (add-hook 'ibuffer-hook
;;          (lambda ()
;;            (ibuffer-perspective-list)))

;; Go into proper mode according to file extension
(setq auto-mode-alist
      (append '(( "\\.C$"    . c++-mode)
                ("\\.cc$"   . c++-mode)
                ("\\.cpp$"  . c++-mode)
                ("\\.cxx$"  . c++-mode)
                ("\\.hxx$"  . c++-mode)
                ("\\.h$"    . c++-mode)
                ("\\.hh$"   . c++-mode)
                ("\\.idl$"  . c++-mode)
                ("\\.ipp$"  . c++-mode)
                ("\\.c$"    . c-mode)
                ("CMakeLists.txt$"    . cmake-mode)
                ;; ("SCons\\(cript\\|truct\\)$"    . python-mode)
                ("SConscript$"    . python-mode)
                ("SConstruct$"    . python-mode)
                ("xonshrc$"    . python-mode)
                ("\\.xsh$"    . python-mode)
                ("\\.ma?k\\'" . makefile-mode)
                ("\\(M\\|m\\|GNUm\\)akefile\\(\\.in\\)?" . makefile-mode)
                ("\\.pl$"   . perl-mode)
                ("\\.pm$"   . perl-mode)
                ("\\.pdf$"   . pdf-view-mode)
                ("\\.java$" . java-mode)
                ("\\.txt$"  . text-mode)
                ("\\.tex$" . latex-mode)
                ("\\.gtikz$" . latex-mode)
                ("\\.tikz$" . latex-mode)
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
                ("\\.m\\'" . matlab-mode)
                ("\\.json\\'" . json-mode)
                ("\\.job\\'" . shell-script-mode)
                ("gitconfig$" . gitconfig-mode)
                ("\\.\\(md\\|markdown\\)\\'" . markdown-mode)
                ("bash\\(rc\\|_profile\\)$" . shell-script-mode)
                ("\\.pdf\\'" . pdf-view-mode)
                ("\\.zsh\\'" . shell-script-mode)
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
                ("pylintrc" . conf-mode)
                ("\\.service$" . conf-mode)
                )
              auto-mode-alist))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; (semantic-mode 1)
;; see http://www.gnu.org/software/emacs/manual/html_node/semantic/Sticky-Func-Mode.html#Sticky-Func-Mode
;; (global-semantic-stickyfunc-mode 1)
;; (global-semantic-idle-summary-mode 1)


;; see http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
;; (defadvice ido-find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
;; 

;:::::::::::::::::::::::::::::::::::::::::::::::
;: Yank and Paste
;:::::::::::::::::::::::::::::::::::::::::::::::

; http://www.emacswiki.org/emacs/CopyAndPaste
; You need an emacs with bug #902 fixed for this to work properly. It
; has now been fixed in CVS HEAD it makes "highlight/middlebutton"
; style (X11 primary selection based) copy-paste work as expected if
; you're used to other modern apps (that is to say, the mere act of
; highlighting doesn't overwrite the clipboard or alter the kill ring,
; but you can paste in merely highlighted text with the mouse if you
; want to)
;;FIXME this does not work if evil command mode is active
;;TODO use a lambda which checks if command mode is active (if yes signals a visible-bell) if not calls mouse-yank-primary)

;; NOTE the following keybinding is already the default in the latest emacs version
;; (global-set-key [mouse-2] 'mouse-yank-primary)  ; make mouse middle-click only paste from primary X11 selection, not clipboard and kill ring.

;; automatically indent yanked code
;; http://www.emacswiki.org/emacs/AutoIndentation
;; (dolist (command '(yank yank-pop mouse-yank-primary))
;;   (eval `(defadvice ,command (after indent-region activate)
;;            (and (not current-prefix-arg)
;;                 (member major-mode '(emacs-lisp-mode
;;                                      lisp-mode
;;                                      clojure-mode
;;                                      scheme-mode
;;                                      haskell-mode
;;                                      ruby-mode
;;                                      rspec-mode
;;                                      python-mode
;;                                      ada-mode
;;                                      c-mode
;;                                      c++-mode
;;                                      java-mode
;;                                      matlab-mode
;;                                      objc-mode
;;                                      latex-mode
;;                                      plain-tex-mode))
;;                 (let ((mark-even-if-inactive transient-mark-mode))
;;                   (indent-region (region-beginning) (region-end) nil))))))


