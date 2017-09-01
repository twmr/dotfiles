;;; personal-emacs-config --- by thomas hisch
;;; Code:
;; (setq debug-on-error t)

(require 'xdg)

(dolist (p '("/lisp" "/lisp/vendor"))
  (add-to-list 'load-path (expand-file-name
                           (concat user-emacs-directory p))))

(defvar thi::cache-file-dir
  (expand-file-name
   (concat (xdg-cache-home) "/emacs")))
(defvar thi::config-dir
  (expand-file-name
   (concat user-emacs-directory "/lisp/thi")))
(require 'cl-lib) ;; cl-delete-if-not
(defvar thi::directory-list
  (cl-delete-if-not
   'file-exists-p
   (mapcar (lambda (path)
             (replace-regexp-in-string "~" (getenv "HOME") path))
           '(
             "~/gitrepos/dotfiles/emacs.d"
             "~/gitrepos/dotfiles/emacs.d/lisp/thi"
             ))))

(setq custom-file (concat thi::config-dir "/custom.el"))
(load custom-file 'noerror)
(mkdir thi::cache-file-dir t)

;; Each file named <somelibrary>.conf.el is loaded just after the library is
;; loaded.
(dolist (file (directory-files thi::config-dir))
  (when (string-match (format "^\\(.+\\)\\.conf\\.el$") file)
    (eval-after-load (match-string-no-properties 1 file)
      `(load ,(concat thi::config-dir "/" file)))))

(require 'package)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;; https://github.com/alezost/emacs-config/prog.el

;; (use-package anaconda-mode :ensure t)

(use-package paradox :ensure t
  :config
  (setq paradox-execute-asynchronously t))

(use-package ace-jump-mode :ensure t :defer t
  :init
  (progn
    (autoload 'ace-jump-mode "ace-jump-mode" nil t)
    (bind-key "C-." 'ace-jump-mode)))

;; (use-package auto-complete :ensure t
;;   :init
;;   (progn
;;     (global-auto-complete-mode)))

(use-package better-shell
    :ensure t
    :bind (("C-'" . better-shell-shell)
           ("C-;" . better-shell-remote-open)))

(use-package bpr :ensure t
  :config
  (setq bpr-colorize-output t) ;; use -color-apply-on-region function on output buffer
  (setq bpr-process-mode #'comint-mode))


(use-package color-identifiers-mode :ensure t :defer t)
(use-package cmake-mode :ensure t :defer t)

(use-package company :ensure t :defer t
  :config
  (global-company-mode))
;;   :config (progn
;;             (defun company-complete-common-or-cycle ()
;;               "Insert the common part of all candidates, or select the next one."
;;               (interactive)
;;               (when (company-manual-begin)
;;                 (let ((tick (buffer-chars-modified-tick)))
;;                   (call-interactively 'company-complete-common)
;;                   (when (eq tick (buffer-chars-modified-tick))
;;                     (let ((company-selection-wrap-around t))
;;                       (call-interactively 'company-select-next))))))
;;             (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
;;             (global-company-mode)))

;;(use-package cython-anaconda :ensure t
;;  :config
;;  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package cython-mode :ensure t :defer t)

(use-package elpy :ensure t
  :init
  (elpy-enable))

(use-package ethan-wspace :ensure t :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook #'ethan-wspace-mode 1)
    (defun thi::tabs-are-less-evil ()
      (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))
    (add-hook 'makefile-mode-hook 'thi::tabs-are-less-evil)
    (add-hook 'sh-mode-hook 'thi::tabs-are-less-evil)
    ))

(use-package help-mode+ :ensure t)

(use-package highlight-function-calls :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-function-calls-mode))

(use-package highlight-indentation :ensure t)

(use-package magit :ensure t
  :init
  (progn
    (setq magit-completing-read-function 'ivy-completing-read)
    )
  )

(use-package magit-gerrit :ensure t
  :if (string= system-name "PC-16609")
  )

(use-package page-break-lines :ensure t :defer t
  :config
  (global-page-break-lines-mode))

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
          (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
          )
  :config (global-flycheck-mode))

(use-package fill-column-indicator :ensure t :defer t)

(use-package helm
  :ensure t
  :defer t
  :config (progn
            (defun thi::directorychooser ()
              "Use ido to select a recently used directory from the `thi::directory-list'."
              (interactive)
              (dired
               (helm-comp-read "Directory open:" thi::directory-list :fuzzy t)))
            (global-set-key [f12] 'thi::directorychooser)))

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

(use-package ini-mode :ensure t :defer t)

(use-package jabber
  :ensure t
  :if (string= system-name "PC-16609")
  :config (progn
            (setq jabber-invalid-certificate-servers '("srv-voip-04"))
            (setq jabber-connection-ssl-program "starttls")
            (setq jabber-account-list
                  '(
                    ("thomas.hisch@srv-voip-04"
                     ;; (:network-server . "conference.srv-voip-04")
                     )))
            (setq jabber-history-enabled t)
            (setq jabber-backlog-number 100)
            (setq jabber-backlog-days 30)
            (setq jabber-auto-reconnect t)
            (setq jabber-roster-show-title nil)
            (setq jabber-roster-show-bindings nil)
            (setq jabber-show-offline-contacts nil)

            (setq jabber-avatar-verbose nil)
            (setq jabber-vcard-avatars-retrieve nil)
            (setq jabber-chat-buffer-format "jabber-%n")
            (setq jabber-groupchat-buffer-format "jabber-gc-%n")

            (setq jabber-muc-autojoin '("hpc_sd@conference.srv-voip-04"))
            (setq jabber-muc-autojoin '("sd@conference.srv-voip-04"))

            (defun notify-jabber-notify (from buf text proposed-alert)
              "(jabber.el hook) Notify of new Jabber chat messages via notify.el"
              (when (or jabber-message-alert-same-buffer
                        (not (memq (selected-window) (get-buffer-window-list buf))))
                (if (jabber-muc-sender-p from)
                    (notify (format "(PM) %s"
                                    (jabber-jid-displayname (jabber-jid-user from)))
                            (format "%s: %s" (jabber-jid-resource from) text)))
                (notify (format "%s" (jabber-jid-displayname from))
                        text)))
            (add-hook 'jabber-alert-message-hooks 'notify-jabber-notify)

            ;; Preventing messages in the echo area from clobbering the mini buffer
            ;; (define-jabber-alert echo "Show a message in the echo area"
            ;;   (lambda (msg)
            ;;     (unless (minibuffer-prompt)
            ;;       (message "%s" msg))))

            ;; (setq jabber-roster-line-format  " %c %-25n %u %-8s  %S  %a")
            (setq jabber-roster-line-format  " %c %-25n %u %-8s  %S")
            ))

(use-package json-mode :ensure t :defer t)

(use-package lua-mode :ensure t :defer t)

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

(use-package pip-requirements :ensure t :defer t)

(use-package python-switch-quotes :ensure t :defer t)

(use-package smex :ensure t)

(use-package sr-speedbar :ensure t)

;; (use-package swiper
;;   :ensure t
;;   :config
;;   (ivy-mode 1))

  ;; (bind-keys :map swiper-map
  ;;            ("C-." (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol))))))
  ;;            ((kbd "M-.") (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'word))))))

  ;; :bind (("C-s" . swiper))

(use-package counsel :ensure t)
  ;; :bind (("C-h v" . counsel-describe-variable)
  ;;        ("C-h f" . counsel-describe-function)
  ;;        ("C-h s" . counsel-info-lookup-symbol)))

;; requires semantic-mode to be enabled
(use-package stickyfunc-enhance :ensure t)

(use-package undo-tree
  :ensure t
  :defer t
  :config (add-hook 'after-init-hook #'global-undo-tree-mode))

(use-package perspective :ensure t :disabled t
  ;; disabled because it still uses frame local variables
  (bind-keys :map projectile-mode-map
        ("s-s" . projecile-persp-switch-project))
  :config
  (project-persist-mode 1) ;; C-c P n; C-c P f
  )

(use-package projectile :ensure t :defer t
  :config (progn
            (projectile-global-mode t)

             ;; needed for the ignore files feature in .projectile (see https://emacs.stackexchange.com/a/16964/2761)
            (setq projectile-indexing-method 'native)

            ;; (setq projectile-completion-system 'ivy)
            (setq projectile-completion-system 'ido)
            ;; (setq projectile-switch-project-action 'projectile-find-dir)

            ;; With this setting, once you have selected your project, you
            ;; will remain in Projectile's completion system to select a
            ;; sub-directory of your project, and then that sub-directory is
            ;; opened for you in a dired buffer. If you use this setting,
            ;; then you will probably also want to set
            (setq projectile-find-dir-includes-top-level t)))

(use-package project-persist :ensure t :defer t
  :config
  (project-persist-mode t) ;; C-c P n; C-c P f
)

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
    (unless (daemonp)
      (pdf-tools-install))
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

(use-package visual-fill-column :ensure t)

(use-package which-key :ensure t
  :config
    (setq which-key-paging-key "<f5>")
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

(use-package zotelo :ensure t :defer t
  :init
  (progn
    (add-hook 'TeX-mode-hook 'zotelo-minor-mode)))

;; see http://stackoverflow.com/questions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr
;; (setq solarized-high-contrast-mode-line t) ;; this fixes the spurious underline in the modeline
(defvar thi::theme 'sanityinc-tomorrow-night)
;; (defvar thi::theme 'solarized-light)
;; (defvar thi::theme 'tango-dark)

;; (defvar thi::theme
;;   (if (string= system-name "dirac")
;;       'solarized-dark
;;     'solarized-light))
(if (daemonp)
    (add-hook 'after-make-frame-functions
              '(lambda (f)
                 (with-selected-frame f
                   (when (window-system f)
                     (tool-bar-mode -1)
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
(load "thi/recentf")
(load "thi/bindings")
(load "thi/ido")
(load "thi/mail")
(load "thi/ccmode")
;; TODO eval-after-loadify
;; (load "thi/latex")
(load "thi/nxml")
(load "thi/compilation")
(load "thi/term")
(load "thi/graphene")

(global-eldoc-mode -1)

(require 'url-tramp)

(put 'dired-find-alternate-file 'disabled nil)

;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'after-init-hook #'yas-global-mode 1)
(add-hook 'after-init-hook #'global-prettify-symbols-mode 1)
(add-hook 'after-init-hook #'global-eldoc-mode -1)
;; ;; (add-hook 'after-init-hook #'global-hungry-delete-mode 1)
;; (add-hook 'after-init-hook #'global-discover-mode)
;; (add-hook 'after-init-hook #'helm-projectile-on)
(add-hook 'after-init-hook #'persp-mode)

;;; init.el ends here
