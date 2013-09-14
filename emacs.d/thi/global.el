;; "y or n" instead of "yes or no"
(defalias 'yes-or-no-p 'y-or-n-p)

;:::::::::::::::::::::::::::::::::::::::::::::::
;: Appearance
;:::::::::::::::::::::::::::::::::::::::::::::::

(setq inhibit-startup-screen t)
(setq default-indicate-empty-lines nil)


;: LINUM
(global-linum-mode 1)
;; linum should be disabled for certain modes where linenumbers do not
;; make sense
;; http://www.emacswiki.org/emacs/LineNumbers
(setq linum-disabled-modes-list '(eshell-mode
                                  org-mode
                                  latex-mode
                                  wl-summary-mode
                                  compilation-mode))

;; this function is required otherwise linum-disabled-modes-list is not
;; taken into account
(defun linum-on ()
    (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
              (string-match "*" (buffer-name))
              )
    (linum-mode 1)))
;;(set-face-foreground 'linum "white")
;;(set-face-background 'linum "black")


;: LINE Highlighting (highlight the current line)
;; exceptions where hl-line is not desired:
;; (setq hl-line-disabled-modes-list '(org-mode))
(setq hl-line-enabled-modes-list '(magit-status-mode
                                   magit-log-mode))
(set-face-attribute 'highlight nil :underline nil)
(add-hook 'find-file-hook 'thi-turn-hl-line-mode-on2)
(add-hook 'magit-mode-hook 'thi-turn-hl-line-mode-on2)


(line-number-mode 1)                    ; Show line number
(column-number-mode 1)                  ; Show colum number
(tool-bar-mode -1)                      ; Kill the toolbar
(menu-bar-mode -1)                      ; Kill the menu bar
(set-scroll-bar-mode 'right)            ; Scrollbar on the right
(scroll-bar-mode -1)                    ; But no scrollbar

(size-indication-mode)
;;default font is now set in .Xresources
;; (set-default-font "ProFont-9")

;:::::::::::::::::::::::::::::::::::::::::::::::
;: Logical Behavour
;:::::::::::::::::::::::::::::::::::::::::::::::

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; automatically adds marks
(eval-after-load 'auto-mark
  '(global-auto-mark-mode 1))

;; If there is a tab, make it the size of 2 spaces
;; TODO Check if this causes problems with fgallina/python.el
(setq-default tab-width 2)

(dolist (hook '(erc-mode-hook
        LaTeX-mode-hook
        org-mode-hook
        edit-server-start-hook
        markdown-mode-hook))
  (add-hook hook (lambda () (progn
                              (variable-pitch-mode t)
                              (wrap-column-mode)))))

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
        ("CMakeLists.txt$"    . cmake-mode)
        ;; ("SCons\\(cript\\|truct\\)$"    . python-mode)
        ("SConscript$"    . python-mode)
        ("SConstruct$"    . python-mode)
        ("\\.ma?k\\'" . makefile-mode)
        ("\\(M\\|m\\|GNUm\\)akefile\\(\\.in\\)?" . makefile-mode)
        ("\\.pl$"   . perl-mode)
        ("\\.pm$"   . perl-mode)
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
        ("\\.json\\'" . js2-mode)
        ("gitconfig$" . gitconfig-mode)
        ("\\.\\(md\\|markdown\\)\\'" . markdown-mode)
        ("bash\\(rc\\|_profile\\)$" . shell-script-mode)
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
        )
              auto-mode-alist))

(add-hook 'text-mode-hook 'turn-on-auto-fill)


(defcustom thi::programming-language-major-modes
  '(prog-mode     ; This is the mode perl, makefile, lisp-mode, scheme-mode,
                  ; emacs-lisp-mode, sh-mode, java-mode, c-mode, c++-mode,
                  ; python-mode inherits from.
    lua-mode
    cmake-mode
    tex-mode                            ; LaTeX inherits
    sgml-mode                           ; HTML inherits
    css-mode
    nxml-mode
    diff-mode
    haskell-mode
    rst-mode)
  "What considering as programming languages.")

(defun thi::customize-programming-language-mode ()
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\|NOTE\\|REFACTOR\\|FIX\\)"
      1
      '(:box (:color "grey10" :line-width 2) :background "red" :bold t :foreground "yellow")
      prepend)))
  ;; (idle-highlight-mode 1)
  ;; temporarily disabled the rainbow modes as i think they cause speed problems
  ;; (rainbow-mode 1)
  ;; (rainbow-delimiters-mode 1)
  (setq show-trailing-whitespace t)
  ;; (flyspell-prog-mode)
  )

(dolist (mode thi::programming-language-major-modes)
  (add-hook
   (intern (concat (symbol-name mode) "-hook"))
   'thi::customize-programming-language-mode))

;; (semantic-mode 1)
;; see http://www.gnu.org/software/emacs/manual/html_node/semantic/Sticky-Func-Mode.html#Sticky-Func-Mode
;; (global-semantic-stickyfunc-mode 1)
;; (global-semantic-idle-summary-mode 1)


;; add warning face for certain keywords
;; (defvar warning-words-regexpVV
;;   (regexp-opt '("FIXME" "TODO" "BUG" "XXX" "DEBUG") 'words)
;;   "Regexp matching words that commonly denote something that
;;  warrants attention in programs.")

;; fontify watch keywords(TODO,FIXME) in prog-modes (taken from emacs-starter-kit)
;; (defun esk-add-watchwords ()
;;   (font-lock-add-keywords
;;    nil '(("\\<\\(FIXME\\|TODO\\|FIX\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
;;           1 font-lock-warning-face t))))

;; (add-hook 'prog-mode-hook 'esk-add-watchwords)
;; (add-hook 'LaTeX-mode-hook 'esk-add-watchwords)

;; from emacs-wiki
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(define-key global-map "\M-Q" 'unfill-paragraph)
(define-key global-map "\C-\M-q" 'unfill-region)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))
;; Handy key definitions
(define-key global-map "\M-Q" 'unfill-paragraph)
(define-key global-map "\M-\C-q" 'unfill-region)


;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)



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


;; taken from Julien Danjou
(setq frame-title-format '("" invocation-name ": %b"))
;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq visible-bell t)
(setq-default fill-column 76)
(setq user-full-name "Thomas Hisch")

(set-default 'indicate-buffer-boundaries '((up . nil) (down . nil) (t . left)))
(setq next-screen-context-lines 5)      ; I want to keep more lines when
                                        ; switching pages
(setq use-dialog-box nil)               ; Seriously…
(put 'narrow-to-region 'disabled nil)
(set-default 'indent-tabs-mode nil)    ; always use spaces to indent, no tab

;; (display-time-mode 1)
(global-hi-lock-mode 1)                 ; highlight stuff
(savehist-mode 1)

(delete-selection-mode 1)               ; Transient mark can delete/replace
(global-hl-line-mode 1)                 ; Highlight the current line
;; (windmove-default-keybindings)        ; Move between frames with Shift+arrow
(show-paren-mode t)
(url-handler-mode 1)                    ; Allow to open URL
(mouse-avoidance-mode 'animate)         ; Move the mouse away
(ffap-bindings)                         ; Use ffap
;; (iswitchb-mode 1)
;; (browse-kill-ring-default-keybindings)
;; (which-func-mode 1)
