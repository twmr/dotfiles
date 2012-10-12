;; -*- coding: utf-8 -*-

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/el-get"))

(setq custom-file "~/.emacs.d/thi/custom.el")
(load custom-file 'noerror)
;;(setq debug-on-error t)

;; Require el-get to install packages
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))
(el-get 'sync)

(load "thi/el-get")

;; (load-theme 'wombat)
(load-theme 'naquadah)
(rainbow-delimiters-mode 1)

(load "thi/defuns")
(load "thi/global")
(load "thi/iedit")
(load "thi/auto-complete")
(load "thi/org-mode")
(load "thi/yasnippet")
(load "thi/git-commit-mode")


(load "vendor/key-chord")
(load "vendor/iy-go-to-char")
(key-chord-mode 1)
;; (load "vendor/eassist")
(load "vendor/lambda-mode")

(load "thi/bindings")
(load "thi/ido")
(load "thi/ccmode")
(load "thi/latex")
(load "thi/gnuplot")
(load "thi/recentf")
(load "thi/nxml")
(load "thi/compilation")
(load "thi/matlab")

(elpa-vendor 'evil "0.0.0")
(vendor 'auto-mark)
(vendor 'flymake)
(vendor 'flymake-cursor)

;; (vendor 'smex)
;; (vendor 'markdown-mode)
;; (vendor 'textlint)
;; (vendor 'ace-jump-mode)
;; (vendor 'ethan-wspace)
;; (vendor 'yasnippet)
;; (vendor 'orgmode)
;; (vendor 'magit)
;; (vendor 'git-commit)
;; (vendor 'python)
;; (vendor 'gnuplot)
;; (vendor 'expand-region)

;; keyboard scroll one line at a time
;; http://www.emacswiki.org/emacs/SmoothScrolling
;; -------------------------------------
;; following commands do not work - still no smooth scrolling
;; -- (setq scroll-step 1)
;; -- (setq scroll-conservatively 10000)
;; however this works: (still not as smooth as in vim :( )
;;(vendor 'smooth-scrolling)
