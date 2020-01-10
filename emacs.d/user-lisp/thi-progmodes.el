(defcustom thi::programming-language-major-modes
  '(prog-mode     ; This is the mode perl, makefile, lisp-mode, scheme-mode,
                  ; emacs-lisp-mode, sh-mode, java-mode, c-mode, c++-mode,
                  ; python-mode inherits from.
    lua-mode
    cmake-mode
    tex-mode      ; LaTeX inherits
    sgml-mode     ; HTML inherits
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
      '(:box (:color "grey10" :line-width 2) :background "red" :foreground "yellow")
      prepend)))
  (setq show-trailing-whitespace t)
  (eldoc-mode -1)
  ;; (rainbow-delimiters-mode 1)
  ;; (idle-highlight-mode 1)
  ;; temporarily disabled the rainbow modes as i think they cause speed problems
  ;; (rainbow-mode 1)
  ;; (flyspell-prog-mode)
  )

(dolist (mode thi::programming-language-major-modes)
  (add-hook
   (intern (concat (symbol-name mode) "-hook"))
   'thi::customize-programming-language-mode))
