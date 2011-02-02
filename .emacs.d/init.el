;; -*- coding: utf-8 -*-

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))

(setq custom-file "~/.emacs.d/thi/custom.el")
(load custom-file 'noerror)

(load "thi/defuns")
(load "thi/global")
(load "thi/bindings")
(load "thi/ido")
(load "thi/magit")
(load "thi/color-theme")
(load "thi/ccmode")
(load "thi/latex")
(load "thi/gnuplot")
(load "thi/recentf")
(load "thi/compilation")
(load "thi/org-mode")

(vendor 'undo-tree)

;; avoid that :qw quits the whole window
;; FIXME move this to somewhere else
(setq vimpulse-want-quit-like-Vim nil) ;; needs to be before vimpulse

(vendor 'vimpulse)
(vendor 'ethan-wspace)
(vendor 'yasnippet)
(vendor 'auto-mark)

;;FIXME vendor function can't load org-mode atm
;;(vendor 'org-mode)

;; keyboard scroll one line at a time
;; http://www.emacswiki.org/emacs/SmoothScrolling
;; -------------------------------------
;; following commands do not work - still no smooth scrolling
;; -- (setq scroll-step 1)
;; -- (setq scroll-conservatively 10000)
;; however this works: (still not as smooth as in vim :( )
(vendor 'smooth-scrolling)
