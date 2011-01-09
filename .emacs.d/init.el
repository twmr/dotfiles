;; -*- coding: utf-8 -*-

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(load "thi/defuns")
(load "thi/global")
(load "thi/ido")
(load "thi/magit")
;; keyboard scroll one line at a time
;; http://www.emacswiki.org/emacs/SmoothScrolling
;; -------------------------------------
;; following commands do not work - still no smooth scrolling
;; -- (setq scroll-step 1)
;; -- (setq scroll-conservatively 10000)
;; however this works: (still not as smooth as in vim :( )
(load "thi/smooth-scrolling")

(require 'init-theme)
(require 'init-ccmode)
(require 'init-latex)
(require 'init-gnuplot)

(vendor 'vimpulse)
(vendor 'ethan-wspace)
(vendor 'yasnippet)
