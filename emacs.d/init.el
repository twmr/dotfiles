;;; init.el --- personal config by thisch -*- lexical-binding:t -*-
;;; Code:
;; (setq debug-on-error t)

(require 'seq)
(require 'xdg)
(require 'cl-lib) ;; cl-loop
(require 'package)

(defvar thi::cache-file-dir
  (expand-file-name
   (concat (xdg-cache-home) "/emacs")))
(defvar thi::config-dir
  (expand-file-name
   (concat user-emacs-directory "/user-lisp")))

(add-to-list 'load-path thi::config-dir)


(when (seq-contains-p command-line-args "--use-exwm")
  (add-to-list 'load-path (expand-file-name (concat user-emacs-directory
                                                    "external/xelb")))
  (add-to-list 'load-path (expand-file-name (concat user-emacs-directory
                                                    "external/exwm")))

  (require 'exwm)
  ;; TODO get rid of perspectives once exwm is properly configured
  ;; TODO start emacs in deamon mode and enable exwm once first frame is created
  ;; TODO emacs lock screen

  ;; FIXME second monitor does not work with this config
  ;; (require 'exwm-randr)
  ;; (setq exwm-randr-workspace-output-plist '(0 "DVI-I-1" 1 "HDMI-0"))
  ;; (add-hook 'exwm-randr-screen-change-hook
  ;;           (lambda ()
  ;;             (start-process-shell-command
  ;;              "xrandr" nil "xrandr --output HDMI-0 --left-of DVI-I-1 --auto")))
  ;; (exwm-randr-enable)

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; The following example demonstrates how to use simulation keys to mimic
  ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
  ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
  ;; and DEST is what EXWM actually sends to application.  Note that both SRC
  ;; and DEST should be key sequences (vector or string).
  (setq exwm-input-simulation-keys
        '(
          ;; movement
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ;; cut/paste.
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; search
          ([?\C-s] . [?\C-f])))

  (exwm-enable)
  (defun exwm-logout ()
      (interactive)
      (recentf-save-list)
      (save-some-buffers)
      (start-process-shell-command "logout" nil "lxsession-logout")))

;; TODO try to get rid of the custom.el file, which is hard to keep in version control
;; see https://github.com/jwiegley/use-package/pull/508
;; see https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
;; (setq custom-file (concat thi::config-dir "/custom.el"))
(setq custom-file "/dev/null")

(setq package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("elpy" . "https://jorgenschaefer.github.io/packages/")))

;; (load custom-file 'noerror)
(mkdir thi::cache-file-dir t)
(mkdir (concat thi::cache-file-dir "/backups") t)

;; see http://stackoverflow.com/questions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr
;; (setq solarized-high-contrast-mode-line t) ;; this fixes the spurious underline in the modeline
(defvar thi::theme 'doom-wilmersdorf)
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
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (package-initialize)
  (require 'use-package))

(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-always t)
  (aw-always-on t)
  :bind (("M-o" . ace-window)))

;; (use-package anaconda-mode :ensure t)

;; (use-package ace-jump-mode :ensure t :defer t
;;   :init
;;   (progn
;;     (autoload 'ace-jump-mode "ace-jump-mode" nil t)
;;     (bind-key "C-." 'ace-jump-mode)))

;; (use-package all-the-icons-ivy
;;   :ensure t
;;   :config
;;   (all-the-icons-ivy-setup))

;; (use-package auto-complete :ensure t
;;   :init
;;   (progn
;;     (global-auto-complete-mode)))

(use-package avy
    :ensure t)

;; (use-package better-shell
;;     :ensure t
;;     :bind (("C-'" . better-shell-shell)
;;            ("C-;" . better-shell-remote-open)))

;; (use-package bpr :ensure t
;;   :config
;;   (setq bpr-colorize-output t) ;; use -color-apply-on-region function on output buffer
;;   (setq bpr-process-mode #'comint-mode))

(use-package cmake-mode :ensure t :defer t)
(use-package color-identifiers-mode :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t)

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

(use-package composite ;; ligature support
  ;; https://github.com/microsoft/cascadia-code/issues/153#issuecomment-548622886
  :defer t
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :hook
  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
    . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; support ligatures, some toned down to prevent hang
  (when (version<= "27.0" emacs-version)
    (let ((alist
           '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
             (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
             (36 . ".\\(?:\\(>\\)>?\\)")
             (37 . ".\\(?:\\(%\\)%?\\)")
             (38 . ".\\(?:\\(&\\)&?\\)")
             (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43 . ".\\(?:\\([>]\\)>?\\)")
             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
             ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
             (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
             (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59 . ".\\(?:\\(;\\);?\\)")
             (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
             (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91 . ".\\(?:\\(|\\)[]|]?\\)")
             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
             (94 . ".\\(?:\\(=\\)=?\\)")
             (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
             (119 . ".\\(?:\\(ww\\)w?\\)")
             (123 . ".\\(?:\\(|\\)[|}]?\\)")
             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
             (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table))
  )

;;(use-package cython-anaconda :ensure t
;;  :config
;;  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package counsel
  :ensure t
  ;; :bind (("C-h v" . counsel-describe-variable)
  ;;        ("C-h f" . counsel-describe-function)
  ;;        ("C-h s" . counsel-info-lookup-symbol)))

  ;; TODO instructions for using TRAMP
  :bind (("C-x C-f" . counsel-find-file))

  :config (progn
            (counsel-mode)
            (defun magit-status-action (x)
              (magit-status x))
            (ivy-set-actions  ;; hit M-o to see available actions
             t
             '(("s" magit-status-action "gitstat")))))

(use-package counsel-projectile
  ;; installed due to the available `counsel-projectile-rg` command

  ;; C-c p f    counsel-projectile-find-file
  ;; C-c p d    counsel-projectile-find-dir
  ;; C-c p s r  counsel-projectile-rg
  ;; C-c p p    counsel-projectile-switch-project
  :ensure t
  :after (counsel)
  :config (progn
            (counsel-projectile-mode)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
            )
  )

(use-package cython-mode :ensure t :defer t)

;; alternative to "rg"
;; see https://github.com/Wilfred/deadgrep/blob/master/docs/ALTERNATIVES.md
(use-package deadgrep :ensure t
  ;; NOTE on debian systems you have to install the ripgrep binary from https://github.com/BurntSushi/ripgrep/releases, otherwise rg --pcre2 ...  returns PCRE2 is not available in this build of ripgrep
  ;; deadgrep used rg --pcre2!!

  ;; TODO try (grep-apply-setting
  ;; 'grep-find-command
  ;;'("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)
  ;;)
  ;; taken from https://stegosaurusdormant.com/emacs-ripgrep/

  :after (dumb-jump)
  :bind (("C-c d" . deadgrep) ;; in entire project/sandbox
         ("C-c D" . deadgrep-in-current-repo))
  :config
  ;; override deadgrep--project-root to include support for dumb-jump files (.dumbjump, .dumbjumpignore)
  (defun deadgrep--project-root ()
    "Guess the project root of the given FILE-PATH."
    (let ((root default-directory)
          (project (locate-dominating-file default-directory #'dumb-jump-get-config)))
      (when project
        (setq root project))
      (when root
        (deadgrep--lookup-override root))))

  (defun deadgrep-in-current-repo (search-term)
    (interactive (list (deadgrep--read-search-term)))
    (let ((deadgrep--project-root
           (lambda ()
             (let ((root default-directory)
                   (project (project-current)))
               (when project
                 (setq root project))
               (when root
                 (deadgrep--lookup-override root))))))
      (deadgrep search-term)))
  )

;; see https://ligerlearn.com/using-emacs-edit-files-within-docker-containers/
(use-package docker-tramp :ensure t :defer t)

(use-package dockerfile-mode :ensure t :defer t)

(use-package doom-themes :ensure t)

(use-package diminish :ensure t)

(use-package dumb-jump
  :ensure t
  :demand t
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-selector 'ivy)
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)
         ("M-g p" . dumb-jump-back)
         ;; :map python-mode-map
         ;; ("M-." . dumb-jump-go)
         ;; ("M-," . dumb-jump-back)
         )
  )

(use-package edit-indirect
  ;; Edit regions in separate buffers, like `org-edit-src-code' but for arbitrary
  ;; regions

  ;; copied from https://gist.github.com/fleimgruber/cd2386d8326fad3e0e2b99ebc2f05739
  ;; referenced in https://github.com/jorgenschaefer/elpy/issues/498#issuecomment-324145571
  ;; needed for editing python docstring in new buffers that are in rst-mode
  ;; TODO add some rst validitiy checks before commiting change in rst-mode buffer.

  ;; rst.el docu: https://docutils.readthedocs.io/en/sphinx-docs/user/emacs.html
  ;; run C-c e to edit docstring in rst-mode buffer
  ;; Commit changes wich C-c C-c
  :ensure t
  :config
  (progn
    ;; https://github.com/Fanael/edit-indirect/issues/6
    (require 's)
    (require 'dash)

    (defvar edit-indirect--left-margin 0)

    (defun vbe:compute-left-margin (code)
      "Compute left margin of a string of code."
      (-min
       (-map #'(lambda (line) (length (car (s-match "^\\s-*" line))))
             (-remove 's-blank? (s-lines code)))))

    (defun vbe:after-indirect-edit-remove-left-margin ()
      "Remove left-margin and save it into a local variable."
      (let ((lm (vbe:compute-left-margin (buffer-substring (point-min) (point-max)))))
        (indent-rigidly (point-min) (point-max) (* -1 lm))
        (setq-local edit-indirect--left-margin lm)))

    (defun vbe:after-indirect-edit-restore-left-margin ()
      "Restore left-margin before commiting."
      (indent-rigidly (point-min) (point-max) edit-indirect--left-margin))

    (add-hook 'edit-indirect-after-creation-hook #'vbe:after-indirect-edit-remove-left-margin)
    (add-hook 'edit-indirect-before-commit-hook #'vbe:after-indirect-edit-restore-left-margin)

    (defun edit-indirect-rst-set-mode (_parent _beg _end)
      (rst-mode))

    (setq edit-indirect-guess-mode-function #'edit-indirect-rst-set-mode)

    (defun edit-indirect-rst-remove-spaces()
      (save-excursion
        (beginning-of-buffer)
        (while (search-forward-regexp "^    \\(.*?\\)" nil t)
          (replace-match "\\1" nil))))

    (defun edit-indirect-rst-add-spaces()
      (save-excursion
        (beginning-of-buffer)
        (while (search-forward-regexp "^\\([^\n]\\)" nil t)
          (replace-match "    \\1" nil))
        (end-of-buffer)
        (insert "    ")))

    (add-to-list 'edit-indirect-after-creation-hook #'edit-indirect-rst-remove-spaces)
    (add-to-list 'edit-indirect-before-commit-hook #'edit-indirect-rst-add-spaces)

    (defun edit-indirect-region-wrap-rst (s e o)
      (edit-indirect-region s e o))

    (defun edit-indirect-rst ()
      (interactive)
      (let ((pt (point)))
        (progn
          (save-excursion
            (setq s (re-search-backward "\\(\"\"\"\\)" nil t)
                  region-start (match-end 0))
            (goto-char pt)
            (setq e (re-search-forward "\\(\"\"\"\\)" nil t)
                  region-end (match-beginning 0))
            (and s e (< s pt e))))
        (edit-indirect-region-wrap-rst region-start region-end t)))
    ))

;; (use-package edwina
;;   :ensure t
;;   :config
;;   (setq display-buffer-base-action '(display-buffer-below-selected))
;;   (edwina-setup-dwm-keys)
;;   (edwina-mode 1))

(use-package eglot :ensure t :defer t
  :config
  (add-to-list 'eglot-server-programs
               `(python-mode . ("pyls" "-v" "--tcp" "--host"
                                "localhost" "--port" :autoport))))

;; (use-package elpy :ensure t
;;   :init
;;   (elpy-enable))

(use-package equake
    :ensure t
    :config  ; some examples of optional settings follow:
    (global-set-key (kbd "C-x C-c") 'equake-check-if-in-equake-frame-before-closing) ; prevent accidental frame-closure
    (setq equake-size-width 0.99) ; set width a bit less than full-screen (prevent 'overflow' on multi-monitor)
    ;; set distinct face for Equake: white foreground with dark blue background, and different font
    (set-face-attribute 'equake-buffer-face 'nil :inherit 'default :family "DejaVu Sans Mono" :background "#000022" :foreground "white"))

(use-package evil :ensure t
  :config (progn
            (setq evil-default-state 'emacs)))

(use-package ethan-wspace
  :ensure t
  :defer t
  :diminish
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

(use-package expand-region :ensure t :defer t
  :bind
  ;;If you expand too far, you can contract the region by pressing - (minus key), or by prefixing the shortcut you defined with a negative argument: C-- C-=.
  (("C-=" . er/expand-region)))

(use-package flx :ensure t)
(use-package flx-ido :ensure t
  :config
  (flx-ido-mode))

(use-package flycheck
  :ensure t
  :custom
  (flycheck-emacs-lisp-initialize-packages t) ;; fixes an unkown pkg error with (require 'hydra) in gerrit.el
  :init (progn
          (setq flycheck-highlighting-mode 'lines)
          (setq flycheck-display-errors-delay 0.4)
          (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
          (add-hook 'python-mode-hook
              '(lambda () (flycheck-select-checker 'python-pylint)))
          ;; (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
          )
  :config (global-flycheck-mode))


(with-eval-after-load 'flycheck
  (flycheck-define-checker python-pylint
    "A Python syntax and style checker using Pylint.

This syntax checker requires Pylint 1.0 or newer.

See URL `https://www.pylint.org/'."
    ;; --reports=n disables the scoring report.
    ;; Not calling pylint directly makes it easier to switch between different
    ;; Python versions; see https://github.com/flycheck/flycheck/issues/1055.
    :command ("python3"
              (eval (flycheck-python-module-args 'python-pylint "pylint"))
              "--reports=n"
              "--output-format=text"
              (eval (if flycheck-pylint-use-symbolic-id
                        "--msg-template={path}:{line}:{column}:{C}:{symbol}:{msg}"
                      "--msg-template={path}:{line}:{column}:{C}:{msg_id}:{msg}"))
              (config-file "--rcfile=" flycheck-pylintrc concat)
              ;; this code is not yet upstream (see PR  #1546
              "--from-stdin" source-original)
    :standard-input t
    :error-filter
    (lambda (errors)
      (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":"
            (or "E" "F") ":"
            (id (one-or-more (not (any ":")))) ":"
            (message) line-end)
     (warning line-start (file-name) ":" line ":" column ":"
              (or "W" "R") ":"
              (id (one-or-more (not (any ":")))) ":"
              (message) line-end)
     (info line-start (file-name) ":" line ":" column ":"
           (or "C" "I") ":"
           (id (one-or-more (not (any ":")))) ":"
           (message) line-end))
    :enabled (lambda ()
               (or (not (flycheck-python-needs-module-p 'python-pylint))
                   (flycheck-python-find-module 'python-pylint "pylint")))
    :verify (lambda (_) (flycheck-python-verify-module 'python-pylint "pylint"))
    :modes python-mode))

(use-package flycheck-package
  :ensure t)

(use-package flycheck-pycheckers
  :ensure t
  :disabled t
  ;; TODO replace source-inplace in definition of pycheckers checker by source-original
  :init (setq flycheck-pycheckers-checkers '(pylint flake8)))

(use-package forge
  :ensure t
  :after magit)

(use-package gerrit
  :if (string= (system-name) "PC-16609.ims.co.at")
  :load-path "~/gitrepos/gerrit.el"
  :custom
  (gerrit-host "gerrit.rnd.ims.co.at")
  (gerrit-save-file (concat thi::cache-file-dir "/git-review"))
  :config
  (progn
    (add-hook 'magit-status-sections-hook #'gerrit-magit-insert-status t)
    (global-set-key (kbd "C-x i") 'gerrit-upload)
    (global-set-key (kbd "C-x o") 'gerrit-download)))

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

;; Pop up last commit information of current line
(use-package git-messenger
  :ensure t
  :bind (:map vc-prefix-map
         ("p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  ;; :pretty-hydra
  ;; ((:title (pretty-hydra-title "Git Messenger" 'alltheicon "git")
  ;;   :color blue :quit-key "q")
  ;;  ("Actions"
  ;;   (("s" git-messenger:popup-show "Show")
  ;;    ;; ("S" git-messenger:popup-show-verbose "Show verbose")
  ;;    ("c" git-messenger:copy-commit-id "Copy hash")
  ;;    ;; ("d" git-messenger:popup-diff "Diff")
  ;;    ("m" git-messenger:copy-message "Copy message")
  ;;    ("," (catch 'git-messenger-loop (git-messenger:show-parent)) "Go Parent"))))
  :init
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

(use-package groovy-mode
  :ensure t
  :defer t
  :mode "Jenkinsfile")

;; TODO help-mode+ is unavailable??
;; (use-package help-mode+ :ensure t)

(use-package helpful :ensure t
  :bind (
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  ("C-h f" . helpful-callable)

  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)))

(use-package highlight-function-calls :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-function-calls-mode))

(use-package highlight-indentation :ensure t)

(use-package helm
  :ensure t
  :bind (("C-x C-h" . helm-mini))
  :config
  (setq helm-mode-fuzzy-match t))

(use-package helm-projectile :ensure t
  :config
  ;; https://www.reddit.com/r/emacs/comments/3m8i5r/helmprojectile_quickly_findcreate_new_file_in/
  ;; (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile)
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
        "Create file"
      :action 'find-file))
  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t)
  )


(use-package hydra :ensure t
  :init
  (progn
    (defhydra hydra-zoom-winner (global-map "<f5>")
      ;; Now, <f5> g 4g 2l will zoom in 5 times, and zoom out 2 times for a
      ;; total of +3 zoom.
      "zoom/winner"
      ("g" text-scale-increase "zoom in")
      ("l" text-scale-decrease "zoom out")
      ("r" winner-undo "win-undo")
      ("t" winner-redo "win-redo"))

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
  (load "thi-ido"))

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
         ("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         :map ivy-minibuffer-map
         ("TAB" . ivy-next-line))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-display-style 'fancy)
  (ivy-height 20)
  (ivy-use-virtual-buffers t) ;; this is great!! it shows me recently used buffers in ivy-switch-buffer

  ;; needed for fuzzy matching (see https://oremacs.com/2016/01/06/ivy-flx/)
  ;; (ivy-re-builders-alist
  ;;  ;; '((t . ivy--regex-fuzzy)))
  ;;  '((t . ivy--subseq-fuzzy)))
  (ivy-re-builders-alist
      '((read-file-name-internal . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))
  (ivy-initial-inputs-alist nil)
  ;; todo not really needed?
  (ivy-use-selectable-prompt t)
  :config (progn
            (ivy-mode)
            ;; see https://oremacs.com/2016/06/27/ivy-push-view/
            (global-set-key (kbd "C-c v") 'ivy-push-view)
            (global-set-key (kbd "C-c V") 'ivy-pop-view)))

(use-package ivy-hydra
  ;; type C-o to see hydra help in completion list
  :ensure t
  :after ivy)

(use-package ivy-rich
  :ensure t
  :after ivy
  :custom
  (ivy-rich-path-style 'abbrev) ;; To abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)
  ;;                         ivy-rich-switch-buffer-align-virtual-buffer t ;; obsolete since 0.1
  :config
  (progn
    (ivy-rich-mode t)
    (setq ivy-virtual-abbreviate 'full) ;; shows the full filename of the virtual/recentf files/buffers.
    (setq ivy-rich--display-transformers-list
          '(ivy-switch-buffer
            (:columns
             (;;(ivy-rich-switch-buffer-icon (:width 2))
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand))))))
  ;; no longer works due to API change
  ;; (ivy-set-display-transformer 'ivy-switch-buffer
  ;;                              'ivy-rich-switch-buffer-transformer))
  )


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

(use-package json-mode :ensure t :defer t)

(use-package link-hint
  :ensure t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

(use-package magit
  ;; bindings: C-c M-g: magit-file-popup (use it in a buffer)
  :ensure t
  :custom
  (magit-repository-directories `(("~/gitrepos" . 1)
                                  ("~/.emacs.d" . 0)))
  (magit-completing-read-function 'ivy-completing-read)
  :bind
  (("C-x g" . magit-status))
  :config
  (defun magit-gpr ()
    "Run git pull --rebase in current repo."
    (interactive)
    (magit-git-command "git pull --rebase")
    ))

(use-package magit-libgit
  :ensure t)

(use-package multiple-cursors :ensure t :defer t
  :bind (("C->"       . mc/mark-next-like-this)
         ("C-<"       . mc/mark-previous-like-this)
         ("C-c C-<"   . mc/mark-all-like-this)

         ("C-c m e"   . mc/mark-more-like-this-extended)
         ("C-c m h"   . mc/mark-all-like-this-dwim)
         ("C-c m l"   . mc/edit-lines)
         ("C-c m n"   . mc/mark-next-like-this)
         ("C-c m p"   . mc/mark-previous-like-this)
         ("C-c m r"   . vr/mc-mark)
         ("C-c m C-a" . mc/edit-beginnings-of-lines)
         ("C-c m C-e" . mc/edit-ends-of-lines)
         ("C-c m C-s" . mc/mark-all-in-region)))

(use-package org
  :ensure t
  :custom
  (org-directory "~/org")
  (org-default-notes-file (concat org-directory "/gtd.org"))
  (org-capture-templates
   '(("t" ; hotkey
      "Todo list item" ; name
      entry ; type
       (file+headline org-default-notes-file "Tasks") ; heading type and title
      "* TODO %U %?")
     ("j"
      "Journal entry"
      entry
      (file+olp+datetree (concat org-directory "journal.org")
      (file "~/.emacs.d/org-templates/journal.oRgcaptmpl")))
   ))
  :bind (("C-c c" . org-capture))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . nil)
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

(use-package package-lint :ensure t :defer t)

(use-package page-break-lines :ensure t :defer t
  :config
  (global-page-break-lines-mode))

;; (use-package paradox :ensure t :defer t
;;   :custom
;;   (paradox-github-token t)
;;   :config
;;   (setq paradox-execute-asynchronously t))

(use-package paredit :ensure t)

(use-package pdf-tools
  :ensure t
  :defer t
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

;; (use-package persp-mode :ensure t
;;   ; is a fork of perspective.el (they can't be installed at the same time.
;;   :config
;;   (persp-mode)
;;   )

(use-package perspective :ensure t
  ;; Prefix key is C-x x
  ;; s — persp-switch: Query a perspective to switch to, or create
  ;; k — persp-remove-buffer: Query a buffer to remove from current perspective
  ;; c — persp-kill : Query a perspective to kill
  ;; r — persp-rename: Rename current perspective
  ;; a — persp-add-buffer: Query an open buffer to add to current perspective
  ;; A — persp-set-buffer: Add buffer to current perspective and remove it from all others
  ;; i — persp-import: Import a given perspective from another frame.
  ;; n, <right> — persp-next: Switch to next perspective
  ;; p, <left> — persp-prev: Switch to previous perspective
  ;; C-s — persp-state-save: Save all perspectives in all frames to a file
  ;; C-l — persp-state-load: Load all perspectives from a file

  ;; replace buffermenu with ibuffer
  :bind (("C-x C-b" . persp-ibuffer)

         ;; this doesn't yet work as expected:
         ;; * the ivy-virtual-buffers are not shown in persp-counsle-switch-buffer
         ;; * the nice table is also not shown - this has to do with ivy-rich I guess
         ;; ("C-x b" . persp-counsel-switch-buffer)
         )
  ;; (bind-keys :map projectile-mode-map
  ;;       ("s-s" . projecile-persp-switch-project))
  :config
  (persp-mode)
  ;; (project-persist-mode 1) ;; C-c P n; C-c P f
  )

(use-package pip-requirements :ensure t :defer t)

(use-package projectile :ensure t
  :custom (projectile-completion-system 'ivy)
  :bind (("C-x f" . projectile-find-file))
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
            ;; (setq projectile-find-dir-includes-top-level t)))
            ))


(use-package project-persist :ensure t :defer t
  :config
  (project-persist-mode t) ;; C-c P n; C-c P f
)
;; (use-package protobuf-mode
;;   :ensure t

;;   :config
;;   (add-hook 'protobuf-mode-hook
;;             (lambda ()
;;               (progn
;;                 (setq c-basic-offset 4)
;;                 (setq tab-width 4)))))

(use-package pyimport :ensure t)

(use-package python
  :custom
  (python-fill-docstring-style 'pep-257)
  (python-indent-guess-indent-offset-verbose nil)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  ;; TODO support jumping in multi git-repo project
  (add-hook 'python-mode-hook 'dumb-jump-mode)

  (defun ims-yapf()
    (interactive)
    (when (string-match "sandbox" buffer-file-name)
      (yapf-mode)))

  (add-hook 'python-mode-hook 'ims-yapf)
  (define-key python-mode-map (kbd "M-.") #'dumb-jump-go)
  (define-key python-mode-map (kbd "M-,") #'dumb-jump-back)
  (define-key python-mode-map (kbd "C-c C-i") #'pyimport-insert-missing)
  (define-key python-mode-map (kbd "C-c e") #'edit-indirect-rst)

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

(use-package python-docstring
  :ensure t
  :config
  (python-docstring-install))

(use-package python-pytest
  :ensure t
  :after python
  ;; see https://shahinism.com/en/posts/emacs-python-pytest/
  :custom
  (python-pytest-arguments
   '("--color"          ;; colored output in the buffer
     "--failed-first"   ;; run the previous failed tests first
     "--maxfail=5"))    ;; exit in 5 continuous failures in a run
  ;; TODO create key bindings for those functions
  ;; :config
  ;; (which-key-declare-prefixes-for-mode 'python-mode "SPC pt" "Testing")
  ;; (evil-leader/set-key-for-mode 'python-mode
  ;;   "ptp" 'python-pytest-popup
  ;;   "ptt" 'python-pytest
  ;;   "ptf" 'python-pytest-file
  ;;   "ptF" 'python-pytest-file-dwim
  ;;   "ptm" 'python-pytest-function
  ;;   "ptM" 'python-pytest-function-dwim
  ;;   "ptl" 'python-pytest-last-failed)
  )

(use-package python-switch-quotes :ensure t :defer t)

(use-package rainbow-delimiters :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package rg :ensure t
  ;; https://github.com/dajva/rg.el
  ;; select word and type "M-s d"
  ;; In *rg* buffer:
  ;;   C-c C-p start wgrep mode
  ;;           to exit it type wgrep-exit
  ;;   C-c c   increase context (toggle)
  :hook wgrep-ag-setup
  :config
  (progn
    (rg-define-search thi::gg :files "*")

    (rg-enable-default-bindings (kbd "M-s"))
    ;; (setq rg-show-header nil)
    (rg-define-toggle "--context 3" (kbd "C-c c"))
  ))

(use-package rust-mode :ensure t)

(use-package smart-mode-line :ensure t
  :custom
  (sml/no-confirm-load-theme t)
  :config
  (defun thi::sml-setup ()
    (sml/setup)
    (sml/apply-theme 'respectful)
    ;; (setq sml/mode-width 'right
    ;;       sml/name-width 60)

    (setq-default mode-line-format
                  '("%e"
                    ;; (:eval (format "[%d] " exwm-workspace-current-index))
                    mode-line-front-space
                    mode-line-mule-info
                    mode-line-client
                    mode-line-modified
                    mode-line-remote
                    mode-line-frame-identification
                    mode-line-buffer-identification
                    sml/pos-id-separator
                    (vc-mode vc-mode)
                    " "
                    mode-line-position
                    evil-mode-line-tag
                    sml/pre-modes-separator
                    mode-line-modes
                    " "
                    mode-line-misc-info))

    )

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                '(lambda (f)
                   (with-selected-frame f
                     (when (window-system f)
                       (tool-bar-mode -1)
                       (menu-bar-mode -1)
                       (scroll-bar-mode -1)
                       (set-frame-font (format "JetBrains Mono:size=%d"
                                               (if (string= (system-name) "PC-16609.ims.co.at")
                                                   12
                                                 34)))
                       (load-theme thi::theme t)
                       (thi::sml-setup)))))
    (progn
      (load-theme thi::theme t)
      (thi::sml-setup)
      (tool-bar-mode -1)
      (menu-bar-mode -1))))

(use-package smex :ensure t :config (load "thi-ido.el"))

;; (use-package spaceline
;;   :ensure t
;;   :demand t
;;   :init
;;   (setq powerline-default-separator 'arrow-fade)
;;   :config
;;   (require 'spaceline-config)
;;   (spaceline-spacemacs-theme))

(use-package sphinx-doc
  ;; TODO this package contains support for generating docstrings in sphinx
  ;; parasble formater for python functions/methods/classes

  ;; TODO does not yet support numpy style docstings  https://github.com/naiquevin/sphinx-doc.el/issues/19
  ;; TODO is there a yasnippet for this already? (yes there are  https://github.com/AndreaCrotti/yasnippet-snippets/search?q=numpy&unscoped_q=numpy
  ;;            snippets: idn, fdn, mdn,
  ;;       TODO they do not support type annotations
  ;;       TODO sync not supported?
  :ensure t


  :config
  (progn

    ;; TODO get region of current signatrue automatically
    (defun thi::python-args-to-docstring-numpy (start end)
      ;; this was taken from https://github.com/AndreaCrotti/yasnippet-snippets/blob/43624cad757a6bd9380c8f79406b1e74b80478f7/snippets/python-mode/.yas-setup.el#L24
      ;; which is used for the docstring snippets in yasnippet

      ;; this function does not depend on yas-text
      "return docstring format for the python arguments in yas-text"
      (interactive "r")
      (let* ((args (python-split-args (buffer-substring-no-properties start end)))
             (format-arg (lambda(arg)
                           (concat (nth 0 arg) " : " (if (nth 1 arg) ", optional") "\n")))
             (formatted-params (mapconcat format-arg args "\n"))
             (formatted-ret (mapconcat format-arg (list (list "out")) "\n")))
        (unless (string= formatted-params "")
          ;; TODO insert it in docstring
          (message (mapconcat 'identity
                     (list "\nParameters\n----------" formatted-params
                           "\nReturns\n-------" formatted-ret)
                     "\n")))))
    ))

(use-package ssh-config-mode :ensure t)

;; requires semantic-mode to be enabled
(use-package stickyfunc-enhance :ensure t)

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-*" . swiper-thing-at-point))

  :config
  (progn
    (defun ivy-with-thing-at-point (cmd)
      (let ((ivy-initial-inputs-alist
             (list
              (cons cmd (thing-at-point 'symbol)))))
        (funcall cmd)))

    (defun swiper-thing-at-point ()
      ;; similar to * in vim command mode
      (interactive)
      (ivy-with-thing-at-point 'swiper))
    ))
  ;; (bind-keys :map swiper-map
  ;;            ("C-." (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol))))))
  ;;            ((kbd "M-.") (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'word))))))

(use-package typescript-mode :ensure t)

(use-package thi-projects
  :after (hydra helm)
  :bind (
         ("<f3>" . thi::hydra-dev-reopen-file-different-sandbox/body)
         ("<f4>" . thi::hydra-project-find-file/body)
         ("S-<f4>" . thi::dev-find-file-in-docker-container)
         ("<f12>" . thi::directorychooser)))

(use-package undo-tree
  ;; C-/ undo (without the undo tree graph) !!!
  ;; M-_ redo (without the undo tree graph) !!!
  :ensure t
  :defer t
  :diminish
  :config (add-hook 'after-init-hook #'global-undo-tree-mode))

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

(use-package visual-fill-column :ensure t)

(use-package wgrep-ag :ensure t)

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

(use-package yapfify :ensure t :defer t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (progn
    (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode)
    )
  )

;; for pdb snippet type tr[TAB] in a python-mode buffer
(use-package yasnippet-snippets :ensure t)

(use-package zop-to-char
  :ensure t
  :defer t
  :init
  (progn
    (bind-key "M-z" 'zop-to-char)))

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


(load "thi-defuns")
(load "thi-global")
(load "thi-progmodes")
(load "thi-projects")
(load "thi-danjou")
(load "thi-recentf")
(load "thi-bindings")
(load "thi-mail")
(load "thi-ccmode")
;; TODO eval-after-loadify
;; (load "thi-latex")
(load "thi-nxml")
(load "thi-compilation")
(load "thi-term")

(load "ims-jira")


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

(require 'url-tramp)

;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'after-init-hook #'yas-global-mode 1)
(add-hook 'after-init-hook #'global-prettify-symbols-mode 1)
;; ;; (add-hook 'after-init-hook #'global-hungry-delete-mode 1)
;; (add-hook 'after-init-hook #'global-discover-mode)
;; (add-hook 'after-init-hook #'helm-projectile-on)

;; this fixes ansii escape seq issues: https://emacs.stackexchange.com/questions/37310/ansi-colors-for-bash-process
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)

;; testing https://github.com/abo-abo/swiper/pull/1518
;; (try "https://raw.githubusercontent.com/MaskRay/swiper/8ca04e88c0c536e6ac8b169b37eba74892678f82/ivy.el")
;; (require 'ivy)
;; (setq ivy-re-builders-alist
;;    '((t . ivy--subseq-fuzzy)))

;; TODO move this into the use-package macros above
;; (with-eval-after-load 'flycheck
;;    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

;; FIXME segfaults
;;(add-to-list 'load-path "/home/thomas/gitrepos/emacs-libvterm")
;;(require 'vterm)


;;; init.el ends here
(put 'downcase-region 'disabled nil)
(setq org-image-actual-width nil)
(put 'dired-find-alternate-file 'disabled nil)
