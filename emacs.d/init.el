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

(add-to-list 'el-get-recipe-path "~/.emacs.d/thi/recipes")
(el-get 'sync)
(load "thi/el-get")
(load-theme 'naquadah)

(load "thi/defuns")
(load "thi/global")
(load "thi/bindings")
(load "thi/ido")
(load "thi/ccmode")
(load "thi/latex")
(load "thi/recentf")
(load "thi/nxml")
(load "thi/compilation")
(load "thi/matlab")
(load "thi/flymake-pre")
(vendor 'flymake-cursor)
(vendor 'ace-jump-mode)


;;customizations for el-get packages
;;TODO automatically load these files when the el-get packs are loaded
(load "thi/smex")
(load "thi/ethan-wspace")
(load "thi/org-mode")
(load "thi/auto-complete")
(load "thi/yasnippet")
(load "thi/git-commit-mode")
(load "thi/gnuplot")
(load "thi/iedit")
(load "thi/rainbow-delimiters-mode")
(load "thi/magit")

;;elpa packages
(elpa-vendor 'evil "0.0.0")
;; (load "thi/undo-tree") ;; FIXME why does this not work ?

(load "vendor/eassist") ;; (for C/C++ development - see bindings.el)
(load "vendor/key-chord") ;; from emacs-rocks
(load "vendor/iy-go-to-char")
(key-chord-mode 1)
(load "vendor/lambda-mode") ;; useful for python development

;; (vendor 'auto-mark)
;; (vendor 'markdown-mode)
;; (vendor 'python)

;; keyboard scroll one line at a time
;; http://www.emacswiki.org/emacs/SmoothScrolling
;; -------------------------------------
;; following commands do not work - still no smooth scrolling
;; -- (setq scroll-step 1)
;; -- (setq scroll-conservatively 10000)
;; however this works: (still not as smooth as in vim :( )
;;(vendor 'smooth-scrolling)
