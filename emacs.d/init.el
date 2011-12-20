;; -*- coding: utf-8 -*-

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))

(setq custom-file "~/.emacs.d/thi/custom.el")
(load custom-file 'noerror)

(if (< emacs-major-version 24)
    (load "thi/color-theme")
  (progn ;; else part (for emacs-24)
    (setq custom-theme-directory "~/.emacs.d/themes")
    (load-theme 'wombat)))

(load "thi/defuns")
(load "thi/global")
(load "vendor/key-chord")
(load "vendor/iy-go-to-char")
(key-chord-mode 1)
(load "vendor/eassist")

(load "thi/bindings")
(if (< emacs-major-version 24)
    (load "thi/folding"))
(load "thi/ido")
(load "thi/ccmode")
(load "thi/latex")
(load "thi/gnuplot")
(load "thi/recentf")
(load "thi/nxml")
(load "thi/compilation")
(load "thi/matlab")

(vendor 'undo-tree) ;; test undo tree once again and see if there are
                    ;; any conflicts with evil
(vendor 'auto-complete)
(vendor 'smex)

(vendor 'evil)
;; (vendor 'vimpulse)

(vendor 'rainbow-delimiters)
(vendor 'ethan-wspace)
;; (vendor 'yasnippet)
(vendor 'auto-mark)
(vendor 'orgmode)
(vendor 'magit)
(vendor 'python)
(vendor 'flymake)
(vendor 'flymake-cursor)

;; keyboard scroll one line at a time
;; http://www.emacswiki.org/emacs/SmoothScrolling
;; -------------------------------------
;; following commands do not work - still no smooth scrolling
;; -- (setq scroll-step 1)
;; -- (setq scroll-conservatively 10000)
;; however this works: (still not as smooth as in vim :( )
;;(vendor 'smooth-scrolling)
