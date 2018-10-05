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
(require 'cl-lib) ;; cl-delete-if-not, cl-loop
(defvar thi::directory-list
  (cl-delete-if-not
   'file-exists-p
   (mapcar (lambda (path)
             (replace-regexp-in-string "~" (getenv "HOME") path))
           '("~/.zsh.d"
             "~/.zsh"
             "~/gitrepos/dotfiles/emacs.d"
             "~/gitrepos/dotfiles/emacs.d/lisp/thi"
             "~/sandbox/unstable/expses"
             "~/sandbox/unstable/pocscripts"
             "~/sandbox/unstable/tools"
             "~/sandbox/unstable/configdb"
             "~/sandbox/unstable/pyhwctrl"
             "~/sandbox/unstable/jobqueue"
             "~/Nextcloud/study_math/"
             ))))

;; TODO try to get rid of the custom.el file, which is hard to keep in version control
;; see https://github.com/jwiegley/use-package/pull/508
;; see https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
;; (setq custom-file (concat thi::config-dir "/custom.el"))
(setq custom-file "/dev/null")

(setq package-archives
   '(("melpa" . "http://melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("elpy" . "http://jorgenschaefer.github.io/packages/")))

;; (load custom-file 'noerror)
(mkdir thi::cache-file-dir t)
(mkdir (concat thi::cache-file-dir "/backups") t)

;; see http://stackoverflow.com/questions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr
;; (setq solarized-high-contrast-mode-line t) ;; this fixes the spurious underline in the modeline
(defvar thi::theme 'sanityinc-tomorrow-night)
;; (defvar thi::theme 'solarized-light)
;; (defvar thi::theme 'tango-dark)

;; (defvar thi::theme
;;   (if (string= system-name "dirac")
;;       'solarized-dark
;;     'solarized-light))

;; Each file named <somelibrary>.conf.el is loaded just after the library is
;; loaded.
(dolist (file (directory-files thi::config-dir))
  (when (string-match (format "^\\(.+\\)\\.conf\\.el$") file)
    (eval-after-load (match-string-no-properties 1 file)
      `(load ,(concat thi::config-dir "/" file)))))

;; Bootstrap `use-package'
(if (fboundp 'package-installed-p)
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
  (require 'use-package))

(eval-when-compile
  (require 'use-package))
;; https://github.com/alezost/emacs-config/prog.el

;; (use-package anaconda-mode :ensure t)

(use-package paradox :ensure t
  :custom
  (paradox-github-token t)
  :config
  (setq paradox-execute-asynchronously t))

(use-package paredit :ensure t)

(use-package ace-jump-mode :ensure t :defer t
  :init
  (progn
    (autoload 'ace-jump-mode "ace-jump-mode" nil t)
    (bind-key "C-." 'ace-jump-mode)))

;; (use-package all-the-icons-ivy
;;   :ensure t
;;   :config
;;   (all-the-icons-ivy-setup))

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

;; (use-package counsel
;;   :ensure t
;;   :after ivy
;;   ;; :bind (("C-h v" . counsel-describe-variable)
;;   ;;        ("C-h f" . counsel-describe-function)
;;   ;;        ("C-h s" . counsel-info-lookup-symbol)))
;;   :config (progn
;;             (counsel-mode)
;;             (ivy-set-actions  ;; hit M-o to see available actions
;;              'counsel-find-file
;;              '(("s" magit-status "gitstat")))))

(use-package cython-mode :ensure t :defer t)

(use-package dockerfile-mode :ensure t :defer t)

(use-package eglot :ensure t
  :config
  (add-to-list 'eglot-server-programs
               `(python-mode . ("pyls" "-v" "--tcp" "--host"
                                "localhost" "--port" :autoport))))

(use-package elpy :ensure t
  :init
  (elpy-enable))

(use-package ethan-wspace :ensure t :defer t
  :custom
  (mode-require-final-newline nil)
  :init
  (progn
    (add-hook 'prog-mode-hook #'ethan-wspace-mode 1)
    (defun thi::tabs-are-less-evil ()
      (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))
    (add-hook 'makefile-mode-hook 'thi::tabs-are-less-evil)
    (add-hook 'sh-mode-hook 'thi::tabs-are-less-evil)
    ))

(use-package git-commit
  :ensure t
  :bind (:map git-commit-mode-map
              ("C-c C-f" . git-commit-fix-jira-insert)
              ("C-c C-r" . git-commit-related-jira-insert))
  :custom
  ;; according to https://chris.beams.io/posts/git-commit/
  (git-commit-summary-max-length 50)
  (git-commit-fill-column 72)
  :config
  (progn
    (defun git-commit-insert-jira-header (header ticket)
      (setq header (format "%s: %s" header ticket))
      (save-excursion
        (goto-char (point-max))
        (cond ((re-search-backward "^[-a-zA-Z]+: [^<]+? <[^>]+>" nil t)
               (end-of-line)
               (insert ?\n header)
               (unless (= (char-after) ?\n)
                 (insert ?\n)))
              (t
               (while (re-search-backward (concat "^" comment-start) nil t))
               (unless (looking-back "\n\n" nil)
                 (insert ?\n))
               (insert header ?\n)))
        (unless (or (eobp) (= (char-after) ?\n))
          (insert ?\n))))

    (defun git-commit-read-jira-ticket ()
      (list (read-string "Jira Ticket: ")))

    (defun git-commit-fix-jira-insert (ticket)
      (interactive (git-commit-read-jira-ticket))
      (git-commit-insert-jira-header "Fixes" ticket))

    (defun git-commit-related-jira-insert (ticket)
      (interactive (git-commit-read-jira-ticket))
      (git-commit-insert-jira-header "Related" ticket))))


;; TODO help-mode+ is unavailable??
;; (use-package help-mode+ :ensure t)

(use-package helpful :ensure t
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

(use-package highlight-function-calls :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-function-calls-mode))

(use-package highlight-indentation :ensure t)

(use-package ssh-config-mode :ensure t)

(use-package magit
  ;; bindings: C-c M-g: magit-file-popup (use it in a buffer)
  :ensure t
  :custom
  (magit-repository-directories `(("~/gitrepos" . 1)
                                  ("~/.emacs.d" . 0)))
  (magit-completing-read-function 'ivy-completing-read))

(use-package page-break-lines :ensure t :defer t
  :config
  (global-page-break-lines-mode))

(use-package python
  :custom
  ;; django style:
  ;;
  ;; \"\"\"
  ;; Process foo, return bar.
  ;; \"\"\"

  ;; \"\"\"
  ;; Process foo, return bar.

  ;; If processing fails throw ProcessingError.
  ;; \"\"\"
  (python-fill-docstring-style 'django)
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

(use-package evil :ensure t
  :config (progn
          ;; (delete 'term-mode evil-insert-state-modes)

          ;;see https://github.com/redguardtoo/emacs.d/blob/master/init-evil.el
          (cl-loop for (mode . state) in
                   '(
                     (conf-mode . emacs)
                     (c++-mode . emacs)
                     (compilation-mode . emacs)
                     (dashboard-mode . emacs)
                     (deadgrep-mode . emacs)
                     (dired-mode . emacs)
                     (dockerfile-mode . emacs)
                     (emacs-lisp-mode . emacs)
                     (eshell-mode . emacs)
                     (flycheck-error-list-mode . emacs)
                     (fundamental-mode . emacs)
                     (help-mode . emacs)
                     (helpful-mode . emacs)
                     (image-dired-mode . emacs)
                     (image-dired-thumbnail-mode . emacs)
                     (image-mode . emacs)
                     (json-mode . emacs)
                     (makey-key-mode . emacs)
                     (magit-repolist-mode . emacs)
                     (org-mode . emacs)
                     (paradox-menu-mode . emacs)
                     (protobuf-mode . emacs)
                     (python-mode . emacs)
                     (quickrun/mode . emacs)
                     (sh-mode . emacs)
                     (shell-mode . emacs)
                     (shell-script-mode . emacs)
                     (shell-script-mode . emacs)
                     (speedbar-mode . emacs)
                     (term-mode . emacs)
                     (text-mode . emacs)
                     (yaml-mode . emacs)
                     )
                   do (evil-set-initial-state mode state))))

(use-package flycheck
  :ensure t
  :init (progn
          (setq flycheck-highlighting-mode 'lines)
          (setq flycheck-display-errors-delay 0.4)
          (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
          (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
          )
  :config (global-flycheck-mode))

(use-package flycheck-pycheckers
  :ensure t
  :init (setq flycheck-pycheckers-checkers '(pylint flake8)))

(use-package fill-column-indicator :ensure t :defer t)

(use-package helm
  :ensure t
  :defer t
  :bind (("C-x C-h" . helm-mini)
         ([f12] . thi::directorychooser))
  :config (progn
            (setq helm-mode-fuzzy-match t)
            (defun thi::directorychooser ()
              "Use ido to select a recently used directory from the `thi::directory-list'."
              (interactive)
              (dired
               (helm-comp-read "Directory open:" thi::directory-list :fuzzy t)))))


(use-package hydra :ensure t
  :init
  (progn
    (defhydra hydra-zoom (global-map "<f5>")
      ;; Now, <f5> g 4g 2l will zoom in 5 times, and zoom out 2 times for a
      ;; total of +3 zoom.
      "zoom"
      ("g" text-scale-increase "in")
      ("l" text-scale-decrease "out"))

    (defhydra hydra-shift (global-map "<f6>")
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

(use-package ido
  :disabled t
  :custom
  (ido-auto-merge-delay-time 2.7)
  (ido-max-window-height 30)
  (ido-use-faces t)
  :config
  (load "thi/ido"))

(use-package ivy
  ;; see https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html
  :ensure t
  :diminish
  ;; nice things about ivy:
  ;; *) to keep the completion buffer open (even after a candidate was selected) type
  ;;    (see http://oremacs.com/swiper/#example---define-a-new-command-with-several-actions)
  ;;    C-c C-o  (opens an ivy occur buffer)
  ;;    now type j and k for naviation and oo for jumping to the selection
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window)
         :map ivy-minibuffer-map
         ("TAB" . ivy-next-line))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-display-style 'fancy)
  (ivy-height 15)
  (ivy-use-virtual-buffers t)
  ;; needed for fuzzy matching (see https://oremacs.com/2016/01/06/ivy-flx/)
  ;; (ivy-re-builders-alist
  ;;  ;; '((t . ivy--regex-fuzzy)))
  ;;  '((t . ivy--subseq-fuzzy)))
  (ivy-initial-inputs-alist nil)
  ;; todo not really needed?
  (ivy-use-selectable-prompt t)
  :config (ivy-mode))

(use-package ivy-hydra
  ;; type C-o to see hydra help in completion list
  :ensure t
  :after ivy)

;; (use-package ivy-rich
;;   :ensure t
;;   :after ivy
;;   :custom
;;   (ivy-virtual-abbreviate 'full
;;                           ivy-rich-switch-buffer-align-virtual-buffer t
;;                           ivy-rich-path-style 'abbrev)
;;   :config
;;   (ivy-set-display-transformer 'ivy-switch-buffer
;;                                'ivy-rich-switch-buffer-transformer))


;; (use-package jabber
;;   :ensure t
;;   :if (string= (system-name) "PC-16609")
;;   :config (progn
;;             (setq jabber-invalid-certificate-servers '("srv-voip-04"))
;;             (setq jabber-connection-ssl-program "starttls")
;;             (setq jabber-account-list
;;                   (when (string= (system-name) "PC-16609")
;;                     '("thomas.hisch@srv-voip-04")
;;                     '("tomtom@ottorocker.nsupdate.info")
;;                     ;; (:network-server . "conference.srv-voip-04")
;;                     ))
;;             (setq jabber-history-enabled t)
;;             (setq jabber-backlog-number 100)
;;             (setq jabber-backlog-days 30)
;;             (setq jabber-auto-reconnect t)
;;             (setq jabber-roster-show-title nil)
;;             (setq jabber-roster-show-bindings nil)
;;             (setq jabber-show-offline-contacts nil)

;;             (setq jabber-avatar-verbose nil)
;;             (setq jabber-vcard-avatars-retrieve nil)
;;             (setq jabber-chat-buffer-format "jabber-%n")
;;             (setq jabber-groupchat-buffer-format "jabber-gc-%n")

;;             (if (string= (system-name) "PC-16609")
;;               (setq jabber-muc-autojoin
;;                     '("hpc_sd@conference.srv-voip-04"
;;                       "sd@conference.srv-voip-04")))

;;             (defun notify-jabber-notify (from buf text proposed-alert)
;;               "(jabber.el hook) Notify of new Jabber chat messages via notify.el"
;;               (when (or jabber-message-alert-same-buffer
;;                         (not (memq (selected-window) (get-buffer-window-list buf))))
;;                 (if (jabber-muc-sender-p from)
;;                     (notify (format "(PM) %s"
;;                                     (jabber-jid-displayname (jabber-jid-user from)))
;;                             (format "%s: %s" (jabber-jid-resource from) text)))
;;                 (notify (format "%s" (jabber-jid-displayname from))
;;                         text)))
;;             (add-hook 'jabber-alert-message-hooks 'notify-jabber-notify)

;;             ;; Preventing messages in the echo area from clobbering the mini buffer
;;             ;; (define-jabber-alert echo "Show a message in the echo area"
;;             ;;   (lambda (msg)
;;             ;;     (unless (minibuffer-prompt)
;;             ;;       (message "%s" msg))))

;;             ;; (setq jabber-roster-line-format  " %c %-25n %u %-8s  %S  %a")
;;             (setq jabber-roster-line-format  " %c %-25n %u %-8s  %S")
;;             ))

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

(use-package sr-speedbar :ensure t)

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))
  ;; (bind-keys :map swiper-map
  ;;            ("C-." (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol))))))
  ;;            ((kbd "M-.") (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'word))))))

;; requires semantic-mode to be enabled
(use-package stickyfunc-enhance :ensure t)

(use-package undo-tree
  ;; C-/ undo (without the undo tree graph) !!!
  ;; M-_ redo (without the undo tree graph) !!!
  :ensure t
  :defer t
  :config (add-hook 'after-init-hook #'global-undo-tree-mode))

(use-package persp-mode :ensure t)

(use-package perspective :ensure t :disabled t
  ;; disabled because it does not yet support persp-2.0
  (bind-keys :map projectile-mode-map
        ("s-s" . projecile-persp-switch-project))
  :config
  (project-persist-mode 1) ;; C-c P n; C-c P f
  )

(use-package projectile :ensure t :defer t
  :config (progn
            (projectile-global-mode t)

            ;; ;; needed for the ignore files feature in .projectile (see https://emacs.stackexchange.com/a/16964/2761)
            ;; (setq projectile-indexing-method 'native)

            (setq projectile-completion-system 'ivy)
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

(add-hook 'org-mode-hook
            (lambda ()
              (mapc
               (lambda (face)
                 (set-face-attribute face nil :inherit 'fixed-pitch))
               (list 'org-code
                     'org-link
                     'org-block
                     'org-table
                     'org-block-begin-line
                     'org-block-end-line
                     'org-meta-line
                     'org-document-info-keyword))))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . nil)
     (gnuplot . t)
     (python . t))))

;; (use-package org-projectile
;;   :bind (("C-c n p" . org-projectile-project-todo-completing-read)
;;          ("C-c c" . org-capture))
;;   :config
;;   (progn
;;     (setq org-projectile-projects-file "~/projects.org")
;;     (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
;;     (push (org-projectile-project-todo-entry) org-capture-templates))
;;   :ensure t)

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

(use-package protobuf-mode
  :ensure t
  :config
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (progn
                (setq c-basic-offset 4)
                (setq tab-width 4)))))


(use-package wgrep-ag :ensure t)

(use-package rg :ensure t
  ;; https://github.com/dajva/rg.el
  ;; select word and type "M-s d"
  ;; In *rg* buffer:
  ;;   C-c C-p start wgrep mode
  ;;   C-c c   increase context (toggle)
  :hook wgrep-ag-setup
  :config
  (progn
    (rg-define-search thi::gg :files "*")

    (rg-enable-default-bindings (kbd "M-s"))
    ;; (setq rg-show-header nil)
    (rg-define-toggle "--context 3" (kbd "C-c c"))
  ))

;; an alternative to "rg"
;; see https://github.com/Wilfred/deadgrep/blob/master/docs/ALTERNATIVES.md
(use-package deadgrep :ensure t
  :bind ("<f7>" . deadgrep))

(use-package smex :ensure t :config (load "thi/ido.el"))

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

(use-package yaml-mode :ensure t :defer t)

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

(use-package color-theme-sanityinc-tomorrow :ensure t)

(use-package smart-mode-line :ensure t
  :custom
  (sml/no-confirm-load-theme t)
  :config
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
      (sml/setup))))

(with-eval-after-load 'files
  (setq backup-directory-alist `(("." . ,(concat thi::cache-file-dir
                                                 "backups"))))
  (setq kept-old-versions 5)
  (setq delete-old-versions t)
  (setq backup-by-copying t)
  (setq version-control t)

  (setq auto-save-list-file-prefix
        (concat thi::cache-file-dir ".auto-saves-"))
  (setq auto-save-file-name-transforms
        `((".*" ,thi::cache-file-dir t))))


(load "thi/defuns")
(load "thi/global")
(load "thi/progmodes")
(load "thi/danjou")
(load "thi/recentf")
(load "thi/bindings")
(load "thi/mail")
(load "thi/ccmode")
;; TODO eval-after-loadify
;; (load "thi/latex")
(load "thi/nxml")
(load "thi/compilation")
(load "thi/term")
(load "thi/magit-review")


;; taken from http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
;; (defun flyspell-detect-ispell-args (&optional run-together)
;;   "if RUN-TOGETHER is true, spell check the CamelCase words."
;;   (let (args)
;;     (cond
;;      ((string-match  "aspell$" ispell-program-name)
;;       ;; Force the English dictionary for aspell
;;       ;; Support Camel Case spelling check (tested with aspell 0.6)
;;       (setq args (list "--sug-mode=ultra" "--lang=en_US"))
;;       (if run-together
;;           (setq args (append args '("--run-together"))))
;;      ((string-match "hunspell$" ispell-program-name)
;;       ;; Force the English dictionary for hunspell
;;       (setq args "-d en_US"))
;;      args))))

(cond
 ((executable-find "aspell")
  ;; you may also need `ispell-extra-args'
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")

  ;; Please note that `ispell-local-dictionary` itself will be passed to hunspell cli with "-d"
  ;; it's also used as the key to lookup ispell-local-dictionary-alist
  ;; if we use different dictionary
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']"
           nil ("-d" "en_US") nil utf-8))))
 (t (setq ispell-program-name nil)))

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
;; Please note when you use hunspell, ispell-extra-args will NOT be used.
;; Hack ispell-local-dictionary-alist instead.
;; (setq-default ispell-extra-args (flyspell-detect-ispell-args t))
;; ;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
;; (defadvice ispell-word (around my-ispell-word activate)
;;   (let ((old-ispell-extra-args ispell-extra-args))
;;     (ispell-kill-ispell t)
;;     (setq ispell-extra-args (flyspell-detect-ispell-args))
;;     ad-do-it
;;     (setq ispell-extra-args old-ispell-extra-args)
;;     (ispell-kill-ispell t)))

;; (defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
;;   (let ((old-ispell-extra-args ispell-extra-args))
;;     (ispell-kill-ispell t)
;;     ;; use emacs original arguments
;;     (setq ispell-extra-args (flyspell-detect-ispell-args))
;;     ad-do-it
;;     ;; restore our own ispell arguments
;;     (setq ispell-extra-args old-ispell-extra-args)
;;     (ispell-kill-ispell t)))

;; (defun text-mode-hook-setup ()
;;   ;; Turn off RUN-TOGETHER option when spell check text-mode
;;   (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
;; (add-hook 'text-mode-hook 'text-mode-hook-setup)


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

;; use the desktop-save infrastructure, which I don't like
;; can this be disabled somehow?
;; (add-hook 'after-init-hook #'persp-mode)

;; testing https://github.com/abo-abo/swiper/pull/1518
;; (try "https://raw.githubusercontent.com/MaskRay/swiper/8ca04e88c0c536e6ac8b169b37eba74892678f82/ivy.el")
;; (require 'ivy)
;; (setq ivy-re-builders-alist
;;    '((t . ivy--subseq-fuzzy)))

;; TODO move this into the use-package macros above
(with-eval-after-load 'flycheck
   (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))


;;; init.el ends here
(put 'downcase-region 'disabled nil)
