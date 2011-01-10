;; -*- coding: utf-8 -*-

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))

(setq custom-file "~/.emacs.d/thi/custom.el")
(load custom-file 'noerror)

(load "thi/defuns")
(load "thi/global")
(load "thi/ido")
(load "thi/magit")
(load "thi/color-theme")
(load "thi/ccmode")
(load "thi/latex")
(load "thi/gnuplot")

(vendor 'undo-tree)
(vendor 'vimpulse)
(vendor 'ethan-wspace)
(vendor 'yasnippet)

;; keyboard scroll one line at a time
;; http://www.emacswiki.org/emacs/SmoothScrolling
;; -------------------------------------
;; following commands do not work - still no smooth scrolling
;; -- (setq scroll-step 1)
;; -- (setq scroll-conservatively 10000)
;; however this works: (still not as smooth as in vim :( )
(vendor 'smooth-scrolling)
