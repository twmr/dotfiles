;; -*- coding: utf-8 -*-

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/el-get"))

(setq custom-file "~/.emacs.d/thi/custom.el")
(load custom-file 'noerror)
;;(setq debug-on-error t)

(setq thi::packages
        '(auto-complete
          magit
          magithub
          ;; git-modes
          org-mode
          cmake-mode
          markdown-mode
          yaml-mode
          expand-region
          ace-jump-mode
          ;; textlint ;; not needed atm
          yasnippet
          naquadah-theme
          smex
          ethan-wspace
          gnuplot-mode
          iedit
          protobuf-mode
          rainbow-mode
          rainbow-delimiters
          highlight-indentation
          browse-kill-ring
          ;; nognus
          ;; go-mode
          el-get
          multi-term
          browse-kill-ring
          goto-last-change
          idle-highlight-mode
          mmm-mode))

;; Require el-get to install packages
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/thi/recipes")

;; (el-get 'sync)
;; (setq el-get-is-lazy t)
;; (setq el-get-byte-compile nil)
(el-get 'sync thi::packages)



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
;; (load "thi/org-mode")
(load "thi/auto-complete")
(load "thi/yasnippet")

;; TODO fix conflict with magit
;; (load "thi/git-commit-mode")
(load "thi/gnuplot")
(load "thi/iedit")
(load "thi/rainbow-delimiters-mode")
(load "thi/magit")

(vendor 'python)
(load "vendor/lambda-mode") ;; useful for python development


;;elpa packages
(elpa-vendor 'evil "0.0.0")
;; (load "thi/undo-tree") ;; FIXME why does this not work ?

(load "vendor/eassist") ;; (for C/C++ development - see bindings.el)
;; (load "vendor/key-chord") ;; from emacs-rocks
;; (load "vendor/iy-go-to-char")
;; (key-chord-mode 1)

;; TODO activate this when you think you need it
;; (vendor 'auto-mark)

;; keyboard scroll one line at a time
;; http://www.emacswiki.org/emacs/SmoothScrolling
;; -------------------------------------
;; following commands do not work - still no smooth scrolling
;; -- (setq scroll-step 1)
;; -- (setq scroll-conservatively 10000)
;; however this works: (still not as smooth as in vim :( )
;;(vendor 'smooth-scrolling)
