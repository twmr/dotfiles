;;; personal-emacs-config --- by thomas hisch

;; (setq debug-on-error t)

(dolist (p '("/lisp" "/lisp/vendor" "/el-get/mu4e/mu4e"))
  (add-to-list 'load-path (expand-file-name
                           (concat user-emacs-directory p))))

(defvar thi::cache-file-dir
  (expand-file-name
   (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs")))
(defvar thi::config-dir
  (expand-file-name
   (concat user-emacs-directory "/lisp/thi")))
(setq custom-file (concat thi::config-dir "/custom.el"))
(load custom-file 'noerror)
(mkdir thi::cache-file-dir t)

(setq paradox-execute-asynchronously t)

;; Each file named <somelibrary>.conf.el is loaded just after the library is
;; loaded.
(dolist (file (directory-files thi::config-dir))
  (when (string-match (format "^\\(.+\\)\\.conf\\.el$") file)
    (eval-after-load (match-string-no-properties 1 file)
      `(load ,(concat thi::config-dir "/" file)))))

(require 'package)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(setq thi::packages
      '(;; hungry-delete ;; http://endlessparentheses.com/hungry-delete-mode.html
        use-package
        git-commit-mode
        git-rebase-mode
        gitconfig-mode
        gitignore-mode
        magit-filenotify
        perspective
        ;; ibuffer-perspective ;; not avail.
        direx
        helm-projectile
        julia-mode
        ;; ;; calfw
        cmake-mode
        ;; eassist
        ;; haskell-mode
        markdown-mode
        yaml-mode
        expand-region
        ace-window
        smart-mode-line
        auctex
        project-persist
        moz-controller
        ;; aggressive-indent
        ;; yasnippet
        ;; ;; dired+
        ;; dired-efap
        ;; notmuch
        ;; ;; replace+
        ;; find-file-in-repository
        ;; ;; python-cell
        ;; epc
        ;; ;; company-anaconda
        ;; ;; jedi (hard dependency on auto-complete mode)

        ;; fill-column-indicator
        ;; ethan-wspace
        ;; lua-mode
        ;; paredit
        ;; ;; minimap
        ;; ;; gnuplot-mode ;; not needed atm
        ;; ;; iedit
        ;; ;; rainbow-mode
        ;; ;; rainbow-delimiters
        ;; highlight-indentation
        ;; discover
        ;; ;; sr-speedbar (commented out as long as there is no upstream fix for the ad-advised-.. problem)
        ;; ;; browse-kill-ring
        ;; ;; bbdb
        ;; ;; bbdb-vcard
        ;; ;; nognus
        ;; ;; go-mode
        ;; ;; multi-term
        ;; browse-kill-ring
        ;; goto-last-change
        ;; idle-highlight-mode
        ;; quickrun
        ;; dockerfile-mode
        ;; json-mode
        ;; solarized-theme
        ))

(require 'use-package)
;; https://github.com/alezost/emacs-config/prog.el

(use-package paradox :ensure t)

(use-package ace-jump-mode :ensure t :defer t
  :init
  (progn
    (autoload 'ace-jump-mode "ace-jump-mode" nil t)
    (bind-key "C-." 'ace-jump-mode)))

(use-package company :ensure t :defer t
  :idle (global-company-mode))

(use-package ethan-wspace :ensure t :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook #'ethan-wspace-mode 1)
    )
  )

(use-package highlight-indentation :ensure t)

(use-package magit :ensure t)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  ;; :bind ;; see http://tuhdo.github.io/helm-intro.html#sec-6
  ;; (("C-`" . 'helm-semantic-or-imenu))
  ;; :init (progn
  ;;         (load "thi/python.conf.el"))
  :init
  (progn
    (with-eval-after-load 'helm
      (bind-key "C-`" #'helm-semantic-or-imenu python-mode-map)
      )
    )
  )

(use-package jedi :ensure t
  :disabled t
  ;; redefine jedi's C-. (jedi:goto-definition)
  ;; to remember position, and set C-, to jump back
  :bind (("C-." . jedi:jump-to-definition)
   ("C-," . jedi:jump-back)
   ("C-c d" . jedi:show-doc))
  :init
  (progn
    (add-hook 'python-mode-hook 'jedi:setup)
    ))

(use-package fill-column-indicator :ensure t)
(use-package flx :ensure t)
(use-package flx-ido :ensure t)

(use-package flycheck
  :ensure t
  :defer t
  :idle (global-flycheck-mode))

(use-package fill-column-indicator :ensure t :defer t)

(use-package helm :ensure t :defer t)

(use-package hydra :ensure t :defer t
  :init
  (progn
    (defhydra hydra-zoom (global-map "<f5>")
      ;; Now, <f5> g 4g 2l will zoom in 5 times, and zoom out 2 times for a
      ;; total of +3 zoom.
      "zoom"
      ("g" text-scale-increase "in")
      ("l" text-scale-decrease "out"))

    (defhydra hydra-zoom (global-map "<f4>")
      "shift"
      ("h" python-indent-shift-left "left")
      ("l" python-indent-shift-right "right"))

    (global-set-key
     (kbd "C-M-o")
     (defhydra hydra-window ()
       ;; http://oremacs.com/2015/02/03/one-hydra-two-hydra/
       "window"
       ("h" windmove-left)
       ("j" windmove-down)
       ("k" windmove-up)
       ("l" windmove-right)
       ("a" (lambda ()
              (interactive)
              (ace-window 1)
              (add-hook 'ace-window-end-once-hook
                        'hydra-window/body))
        "ace")
       ("v" (lambda ()
              (interactive)
              (split-window-right)
              (windmove-right))
        "vert")
       ("x" (lambda ()
              (interactive)
              (split-window-below)
              (windmove-down))
        "horz")
       ("s" (lambda ()
              (interactive)
              (ace-window 4)
              (add-hook 'ace-window-end-once-hook
                        'hydra-window/body))
        "swap")
       ("d" (lambda ()
              (interactive)
              (ace-window 16)
              (add-hook 'ace-window-end-once-hook
                        'hydra-window/body))
        "del")
       ("o" delete-other-windows "1" :color blue)
       ("i" ace-maximize-window "a1" :color blue)
       ("q" nil "cancel")))))


(use-package smex :ensure t)

;; requires semantic-mode to be enabled
(use-package stickyfunc-enhance :ensure t)

(use-package undo-tree
  :ensure t
  :defer t
  :idle (add-hook 'after-init-hook #'global-undo-tree-mode))

(use-package projectile :ensure t :defer t
  :idle (projectile-global-mode t))

(use-package pdf-tools
  :ensure t
  :defer t
  :init
  (progn
    (autoload 'pdf-view-mode "pdf-tools" nil t)
    (pdf-tools-install)
    )
  )

(use-package window-numbering
  :ensure t
  :defer t
  :init
  (progn
    (custom-set-faces '(window-numbering-face
                        ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)))))
    (window-numbering-mode 1))
  )

(use-package yasnippet :ensure t :defer t)

(use-package zop-to-char
  :ensure t
  :defer t
  :init
  (progn
    (bind-key "M-z" 'zop-to-char)))


;; see http://stackoverflow.com/questions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr
(setq solarized-high-contrast-mode-line t) ;; this fixes the spurious underline in the modeline
(defvar thi::theme 'sanityinc-tomorrow-night)

;; (defvar thi::theme
;;   (if (string= system-name "dirac")
;;       'solarized-dark
;;     'solarized-light))
(if (daemonp)
    (add-hook 'after-make-frame-functions
              '(lambda (f)
                 (with-selected-frame f
                   (when (window-system f)
                     (load-theme thi::theme t)
                     (sml/setup)))))
  (progn
    (load-theme thi::theme t)
    (sml/setup)))

(load "thi/defuns")
(load "thi/global")
(load "thi/progmodes")
(load "thi/danjou")
(load "thi/vc")
(load "thi/bindings")
(load "thi/projects")
(load "thi/ido")
(load "thi/mail")
(load "thi/ccmode")
(load "thi/latex")
(load "thi/recentf")
(load "thi/nxml")
(load "thi/compilation")
(load "thi/term")
(load "vendor/sr-speedbar") ;; contains the fix for emacs-24.4
(load "thi/graphene")



(require 'url-tramp)

(put 'dired-find-alternate-file 'disabled nil)

;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'after-init-hook #'yas-global-mode 1)
;; (add-hook 'after-init-hook #'global-prettify-symbols-mode 1)
;; ;; (add-hook 'after-init-hook #'global-hungry-delete-mode 1)
;; (add-hook 'after-init-hook #'global-discover-mode)
;; (add-hook 'after-init-hook #'helm-projectile-on)
(add-hook 'after-init-hook #'persp-mode)

;;; init.el ends here
