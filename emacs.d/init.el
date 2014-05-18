;;; personal-emacs-contig --- by thomas hisch

;; (setq debug-on-error t)

(dolist (p '("/lisp" "/lisp/vendor" "/el-get/el-get" "/el-get/mu4e/mu4e"))
  (add-to-list 'load-path (expand-file-name
                           (concat user-emacs-directory p))))

(defvar thi::cache-file-dir (expand-file-name
                             (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs")))
(defvar thi::config-dir (expand-file-name
                         (concat user-emacs-directory "/lisp/thi")))
(setq custom-file (concat thi::config-dir "/custom.el"))
(load custom-file 'noerror)
(mkdir thi::cache-file-dir t)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar prelude-packages
  '(json-mode
    solarized-theme
    smart-mode-line
    ace-window
    ;; moe-theme
    )
  "A list of packages to ensure are installed at launch.")

;; TODO rewrite prelude-packages-installed-p s.t. it does not required "cl"
(require 'cl-lib)
(require 'cl)
(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(package-initialize)

;; http://comments.gmane.org/gmane.emacs.vim-emulation/1700
(setq evil-want-C-i-jump nil)

;; Each file named <somelibrary>.conf.el is loaded just after the library is
;; loaded.
(dolist (file (directory-files thi::config-dir))
  (when (string-match (format "^\\(.+\\)\\.conf\\.el$") file)
    (eval-after-load (match-string-no-properties 1 file)
      `(load ,(concat thi::config-dir "/" file)))))

(setq thi::packages
        '(auto-complete
          git-modes
          magit
          magit-filenotify
          perspective
          projectile
          direx
          ;; magithub
          git-modes
          ;; calfw
          org-mode
          cmake-mode
          eassist
          haskell-mode
          markdown-mode
          yaml-mode
          ;; expand-region ;; temp. disabled because atm it triggers the
          ;; error void var. er/add-pyhon-.. while opening a python file
          ace-jump-mode
          ;; ace-window			
          yasnippet
          flx
          smex
          flycheck
          dired+
          ;;replace+
          find-file-in-repository
          fill-column-indicator
          ethan-wspace
          lua-mode
          ;;minimap
          evil-leader
          evil
          evil-numbers
          python-cell
          epc
          jedi
          ;; gnuplot-mode ;; not needed atm
          ;; iedit
          protobuf-mode
          ;;rainbow-mode
          rainbow-delimiters
          highlight-indentation
          ;; sr-speedbar (commented out as long as there is no upstream fix for the ad-advised-.. problem)
          ;; browse-kill-ring
          bbdb
          ;; nognus
          ;; go-mode
          el-get
          ;; multi-term
          browse-kill-ring
          goto-last-change
          idle-highlight-mode
          window-numbering
          mmm-mode))

(when (string= system-name "pc-52-rh.ims.co.at")
  (delete 'org-mode thi::packages)
  (delete 'markdown-mode thi::packages))

;; Require el-get to install packages
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(add-to-list 'el-get-recipe-path (concat thi::config-dir "/recipes"))

;; (el-get 'sync)
;; (setq el-get-is-lazy t)
;; (setq el-get-byte-compile nil)
(el-get 'sync thi::packages)

;; see http://stackoverflow.com/questions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr
(setq solarized-high-contrast-mode-line t) ;; this fixes the spurious underline in the modeline
;; (defvar thi::theme 'solarized-light)

(defvar thi::theme
  (if (string= system-name "pc-52-rh.ims.co.at")
    'solarized-light
    'solarized-dark))
(if (daemonp)
    (add-hook 'after-make-frame-functions
              '(lambda (f)
                 (with-selected-frame f
                   (when (window-system f) (load-theme thi::theme t)))))
  (load-theme thi::theme t))
(sml/setup)

(load "thi/defuns")
(load "thi/global")
(load "thi/vc")
(load "thi/bindings")
(load "thi/projects")
(load "thi/ido")
(load "thi/ccmode")
;; I don't need a latex setup atm
;;(load "thi/latex")
(load "thi/recentf")
(load "thi/nxml")
(load "thi/compilation")
(load "thi/term")
(load "vendor/sr-speedbar") ;; contains the fix for emacs-24.4
(load "thi/graphene")
(load "thi/python") ;; we need to load it manually because python.conf.el
                    ;; does not get loaded using our el-get specific loader

(custom-set-faces '(window-numbering-face
                    ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)))))
(window-numbering-mode 1)

;;customizations for el-get packages
;;TODO automatically load these files when the el-get packs are loaded
(load "thi/auto-complete")

;; (load "vendor/key-chord") ;; from emacs-rocks
;; (load "vendor/iy-go-to-char")
;; (key-chord-mode 1)

;; TODO activate this when you think you need it
;; (vendor 'auto-mark)

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook #'global-undo-tree-mode)
(add-hook 'after-init-hook #'yas-global-mode 1)
(add-hook 'after-init-hook #'persp-mode 1)
(add-hook 'after-init-hook #'global-prettify-symbols-mode 1)

;;; init.el ends here
