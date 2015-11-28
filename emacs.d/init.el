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

(eval-when-compile
  (require 'use-package))
;; https://github.com/alezost/emacs-config/prog.el

(use-package paradox :ensure t)

(use-package matlab-mode :ensure t :defer t)

(use-package ace-jump-mode :ensure t :defer t
  :init
  (progn
    (autoload 'ace-jump-mode "ace-jump-mode" nil t)
    (bind-key "C-." 'ace-jump-mode)))

(use-package color-identifiers-mode :ensure t :defer t)
(use-package cmake-mode :ensure t :defer t)

(use-package company :ensure t :defer t
  :config (progn
            (defun company-complete-common-or-cycle ()
              "Insert the common part of all candidates, or select the next one."
              (interactive)
              (when (company-manual-begin)
                (let ((tick (buffer-chars-modified-tick)))
                  (call-interactively 'company-complete-common)
                  (when (eq tick (buffer-chars-modified-tick))
                    (let ((company-selection-wrap-around t))
                      (call-interactively 'company-select-next))))))
            (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
            (global-company-mode)))

(use-package cython-mode :ensure t :defer t)

(use-package ethan-wspace :ensure t :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook #'ethan-wspace-mode 1)
    )
  )

(use-package highlight-indentation :ensure t)

(use-package magit :ensure t
  :init
  (progn
    (setq magit-completing-read-function 'ivy-completing-read)
    )
  )

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  ;; :bind ;; see http://tuhdo.github.io/helm-intro.html#sec-6
  ;; (("C-`" . 'helm-semantic-or-imenu))
  ;; :init (progn
  ;;         (load "thi/python.conf.el"))
  ;; :init
  ;; (progn
  ;;   (with-eval-after-load 'helm
  ;;     (bind-key "C-`" #'helm-semantic-or-imenu 'python-mode-map)
  ;;     )
  ;;   )
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

;; (use-package evil :ensure t)

(use-package flycheck
  :ensure t
  :init (progn
          (setq flycheck-highlighting-mode 'lines)
          (setq flycheck-display-errors-delay 0.4)
          (setq flycheck-completion-system 'ido)
          (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
          )
  :config (global-flycheck-mode))

(use-package fill-column-indicator :ensure t :defer t)

(use-package helm :ensure t :defer t)

(use-package hydra :ensure t
  :init
  (progn
    (defhydra hydra-zoom (global-map "<f5>")
      ;; Now, <f5> g 4g 2l will zoom in 5 times, and zoom out 2 times for a
      ;; total of +3 zoom.
      "zoom"
      ("g" text-scale-increase "in")
      ("l" text-scale-decrease "out"))

    (defhydra hydra-shift (global-map "<f4>")
      "shift"
      ("h" python-indent-shift-left "left")
      ("l" python-indent-shift-right "right"))

;;     (global-set-key
;;      (kbd "C-x h")
;;      (defhydra hydra-apropos (:color blue
;;                                      :hint nil)
;;        "
;; _a_propos        _c_ommand
;; _d_ocumentation  _l_ibrary
;; _v_ariable       _u_ser-option
;; ^ ^          valu_e_"
;;        ("a" apropos)
;;        ("d" apropos-documentation)
;;        ("v" apropos-variable)
;;        ("c" apropos-command)
;;        ("l" apropos-library)
;;        ("u" apropos-user-option)
;;        ("e" apropos-value)))

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
       ("q" nil "cancel")))


    (defhydra hydra-pdftools (:color blue :hint nil)
      "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
      ("\\" hydra-master/body "back")
      ("<ESC>" nil "quit")
      ("al" pdf-annot-list-annotations)
      ("ad" pdf-annot-delete)
      ("aa" pdf-annot-attachment-dired)
      ("am" pdf-annot-add-markup-annotation)
      ("at" pdf-annot-add-text-annotation)
      ("y"  pdf-view-kill-ring-save)
      ("+" pdf-view-enlarge :color red)
      ("-" pdf-view-shrink :color red)
      ("0" pdf-view-scale-reset)
      ("H" pdf-view-fit-height-to-window)
      ("W" pdf-view-fit-width-to-window)
      ("P" pdf-view-fit-page-to-window)
      ("n" pdf-view-next-page-command :color red)
      ("p" pdf-view-previous-page-command :color red)
      ("d" pdf-view-dark-minor-mode)
      ("b" pdf-view-set-slice-from-bounding-box)
      ("r" pdf-view-reset-slice)
      ("g" pdf-view-first-page)
      ("G" pdf-view-last-page)
      ("e" pdf-view-goto-page)
      ("o" pdf-outline)
      ("s" pdf-occur)
      ("i" pdf-misc-display-metadata)
      ("u" pdf-view-revert-buffer)
      ("F" pdf-links-action-perfom)
      ("f" pdf-links-isearch-link)
      ("B" pdf-history-backward :color red)
      ("N" pdf-history-forward :color red)
      ("l" image-forward-hscroll :color red)
      ("h" image-backward-hscroll :color red))


    ))

(use-package imenu-anywhere
  :ensure t
  :defer t
  :init (global-set-key (kbd "C-.") 'imenu-anywhere)
  :config (defun jcs-use-package ()
            (add-to-list 'imenu-generic-expression
             '("Used Packages"
               "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
  (add-hook 'emacs-lisp-mode-hook #'jcs-use-package))


(use-package multiple-cursors :ensure t :defer t
  :bind (("C-c m e"   . mc/mark-more-like-this-extended)
         ("C-c m h"   . mc/mark-all-like-this-dwim)
         ("C-c m l"   . mc/edit-lines)
         ("C-c m n"   . mc/mark-next-like-this)
         ("C-c m p"   . mc/mark-previous-like-this)
         ("C-c m r"   . vr/mc-mark)
         ("C-c m C-a" . mc/edit-beginnings-of-lines)
         ("C-c m C-e" . mc/edit-ends-of-lines)
         ("C-c m C-s" . mc/mark-all-in-region)))

(use-package smex :ensure t)

(use-package swiper :ensure t
  :config (ivy-mode 1)
  :bind (("C-s" . swiper))
  )

(use-package counsel :ensure t
  :bind (("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-h s" . counsel-info-lookup-symbol)))

;; requires semantic-mode to be enabled
(use-package stickyfunc-enhance :ensure t)

(use-package undo-tree
  :ensure t
  :defer t
  :config (add-hook 'after-init-hook #'global-undo-tree-mode))

(use-package projectile :ensure t :defer t
  :config (progn
            (projectile-global-mode t)
            ;; (setq projectile-completion-system 'ivy)
            (setq projectile-completion-system 'ido)
            ))

(use-package helm-projectile :ensure t :defer t
  :init
  ;; https://www.reddit.com/r/emacs/comments/3m8i5r/helmprojectile_quickly_findcreate_new_file_in/
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile)
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
        "Create file"
      :action 'find-file))
  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t)
  )


(use-package pdf-tools
  :ensure t
  :config
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-page)
    (bind-keys :map pdf-view-mode-map
        ("\\" . hydra-pdftools/body)
        ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
        ("g"  . pdf-view-first-page)
        ("G"  . pdf-view-last-page)
        ("l"  . image-forward-hscroll)
        ("h"  . image-backward-hscroll)
        ("j"  . pdf-view-next-page)
        ("k"  . pdf-view-previous-page)
        ("e"  . pdf-view-goto-page)
        ("u"  . pdf-view-revert-buffer)
        ("al" . pdf-annot-list-annotations)
        ("ad" . pdf-annot-delete)
        ("aa" . pdf-annot-attachment-dired)
        ("am" . pdf-annot-add-markup-annotation)
        ("at" . pdf-annot-add-text-annotation)
        ("y"  . pdf-view-kill-ring-save)
        ("i"  . pdf-misc-display-metadata)
        ("s"  . pdf-occur)
        ("b"  . pdf-view-set-slice-from-bounding-box)
        ("r"  . pdf-view-reset-slice))
     (use-package org-pdfview
       :ensure t))

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

(use-package zotelo :ensure t :defer t)


;; see http://stackoverflow.com/questions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr
;; (setq solarized-high-contrast-mode-line t) ;; this fixes the spurious underline in the modeline
;; (defvar thi::theme 'sanityinc-tomorrow-night)
(defvar thi::theme 'aurora)

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
