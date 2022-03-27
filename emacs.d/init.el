;;; init.el --- personal config by thisch -*- lexical-binding:t -*-
;;; Code:
;; (setq debug-on-error t)

(require 'seq)
(require 'xdg)
(require 'cl-lib) ;; cl-loop
(require 'package)
(require 'bug-reference)

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
  ;; TODO battery information + safe shutdown when level is critical (shouldn't this be handled by gnome-flashback?)
  ;; TODO does it make sense to use an external tool for displaying a bar (polybar: https://github.com/ch11ng/exwm/issues/716#issuecomment-593100329)
  ;; https://tech.toryanderson.com/posts/exwm_laptop/

  ;; docking station - multiple monitor support

  ;; TODO get rid of perspectives once exwm is properly configured
  ;; TODO start emacs in daemon mode and enable exwm once first frame is created
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
  ;; TODO needed for which systray icons? (I would expect the
  ;; network-manager applet in the systray, but it is not there)
  (exwm-systemtray-enable)

  ;; TODO minimal workspace number must be 1 and not 0
  (setq exwm-workspace-number 4) ;; what does this var affect?

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  ;; Global keybindings.
  (setq exwm-input-global-keys
        `(
          ;; 's-r': Reset (to line-mode).
          ([?\s-r] . exwm-reset)
          ;; 's-w': Switch workspace.
          ([?\s-w] . exwm-workspace-switch)
          ;; 's-&': Launch application.
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ;; 's-N': Switch to certain workspace.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

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
      '(
        ("celpa" . "https://celpa.conao3.com/packages/") ;; contains new origami.el
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("elpy" . "https://jorgenschaefer.github.io/packages/")))

;; (load custom-file 'noerror)
(mkdir thi::cache-file-dir t)
(mkdir (concat thi::cache-file-dir "/backups") t)


(defvar thi::jira-rnd-projects '("CTB" "RD" "DT" "HPC" "SD" "RS"))
(defvar thi::jira-service-projects '("RHI" "SER" "FRCIP"))
(defvar thi::font "JetBrains Mono") ;; "IBM Plex Mono") ;; "JetBrains Mono", "Iosevka"
(defvar thi::font "Iosevka")

(defun thi::set-font-size (size)
  ;; note that there is also x-select-frame, that pops up a font dialog
  ;; see also https://www.gnu.org/software/emacs/manual/html_node/elisp/Low_002dLevel-Font.html how the returned object can be converted to an alist:
  ;; see font-face-attributes

  (set-frame-font (format
                   "%s:size=%d"
                   thi::font size)))

(defvar thi::at-work (or (string= (system-name) "PC-16609.ims.co.at")
                         (string= (system-name) "NBPF1PQX4B")))
;; (setq thi::at-work nil)
(if thi::at-work
  ;; for isort/yapf/pylint
  ;; TODO do we want to set exec-path globally, or just for certain parts?
  ;; buffer-local
  ;; IMO better is to have sth like https://github.com/flycheck/flycheck/pull/272
    (add-to-list 'exec-path (expand-file-name "~/miniconda3/bin"))
  (add-to-list 'exec-path (expand-file-name "~/miniconda/envs/py37/bin")))


;; references to jira/redmine/gerrit tickets/changes
;; for github issues see `bug-reference-github`
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/bug-reference.el
;; see also goto-address-mode
;; see also https://emacs.stackexchange.com/questions/35878/multiple-url-formats-for-bug-reference-mode

(defun thi::bug-reference-url ()
  "Return a gerrit/jira/redmine URL.
Intended as a value for `bug-reference-url-format'."

  (let ((issue-prefix (match-string-no-properties 2))
        (issue-number (match-string-no-properties 3)))
    ;; (message "prefix %s; number %s" issue-prefix issue-number)
    (cond
     ((string= "g" issue-prefix) ;; gerrit
      (format "https://gerrit.ims.co.at/c/%s" issue-number))
     ((string= "#" issue-prefix) ;; redmine
      (format "http://bugs.ims.co.at/issues/%s" issue-number))
     ((member issue-prefix
              (seq-map (lambda (elt) (concat elt "-")) thi::jira-rnd-projects))
      (format "https://jira.rnd.ims.co.at/browse/%s%s" issue-prefix issue-number))
     ((member issue-prefix
              (seq-map (lambda (elt) (concat elt "-")) thi::jira-service-projects))
      (format "https://service.ims.co.at/browse/%s%s" issue-prefix issue-number))
     (t
      (format "https://gitlab.example.com/group/project/%s/%s"
              (if (string-suffix-p "!" issue-prefix)
                  "merge_requests"
                "issues")
              issue-number)))))

;; FOR DEBUGGING
;; (with-current-buffer-window
;;  (generate-new-buffer-name "tmp") () nil
;;  (emacs-lisp-mode)
;;  (setq-local bug-reference-bug-regexp
;;              (rx (group (| ?g ;; gerrit change prefix
;;                            ?# ;; redmine issue prefix
;;                            (: (| "RD" "SD" "DT") ?-)

;;                            ;; from the example
;;                            (: (in ?I ?i) "ssue" (? ?\s) ?#)
;;                            (: "MR" (? ?\s) ?!)))
;;                  (group (+ digit))))
;;  (setq-local bug-reference-url-format #'thi::bug-reference-url)
;;  (insert ";; RD-3 MR!101 solves issue #100 in g123, RD-1234, DT-224, SD-93 redmine #99\n")
;;  (bug-reference-prog-mode))


(defvar thi::bug-reference-bug-regexp
  ;; Type C-c RET to goto reference
  (rx (group
       (group (| ?g ;; gerrit change prefix
                 ?# ;; redmine issue prefix

                 ;; see https://francismurillo.github.io/2017-03-30-Exploring-Emacs-rx-Macro/
                 (: (eval `(| ,@thi::jira-rnd-projects)) ?-)
                 (: (eval `(| ,@thi::jira-service-projects)) ?-)
                 ))
       (group (+ digit)))))


(defun thi::activate-ticket-and-gerrit-links ()
  (interactive)
  (setq-local bug-reference-bug-regexp thi::bug-reference-bug-regexp)
  (setq-local bug-reference-url-format #'thi::bug-reference-url)
  (bug-reference-prog-mode 1) ;; only in comments
  )

(defun thi::activate-ticket-and-gerrit-links-text-modes ()
  (interactive)
  ;; in text mode we want to use (bug-reference-mode) and not the
  ;; (bug-refernece-prog-mode)
  (thi::activate-ticket-and-gerrit-links)
  (bug-reference-prog-mode 0)
  (bug-reference-mode 1)
  )

(add-hook 'prog-mode-hook #'thi::activate-ticket-and-gerrit-links)
(add-hook 'org-mode-hook #'thi::activate-ticket-and-gerrit-links-text-modes)
(add-hook 'vc-git-log-view-mode-hook #'thi::activate-ticket-and-gerrit-links-text-modes)
(add-hook 'magit-revision-mode-hook #'thi::activate-ticket-and-gerrit-links-text-modes)

;; https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
;; Don't show "Symbolic link to Git-controlled source file; follow link? (y or n)"
(setq vc-follow-symlinks t)

;; To log warnings without popping up the @*Warnings* buffer, set this
;; variable to 'silent.
(setq native-comp-async-report-warnings-errors 'silent)

;; show current function information in header-line
;; see https://emacsredux.com/blog/2014/04/05/which-function-mode/
;; (which-function-mode)
;; (setq which-func-modes '(python-mode))
;; (setq mode-line-misc-info
;;             ;; We remove Which Function Mode from the mode line, because it's mostly
;;             ;; invisible here anyway.
;;             (assq-delete-all 'which-function-mode mode-line-misc-info))
;; (add-hook 'python-mode-hook (lambda ()
;;                               (make-variable-buffer-local 'header-line-format)
;;                               (setq-default header-line-format
;;                                             '((which-function-mode ("" which-func-format " "))))))

;; (defun thi::display-header ()
;;   "Create the header string and display it."
;;   ;; The dark blue in the header for which-func is terrible to read.
;;   ;; However, in the terminal it's quite nice
;;   (if header-line-format
;;       nil
;;     (if window-system
;;         (custom-set-faces
;;          '(which-func ((t (:foreground "#8fb28f")))))
;;       (custom-set-faces
;;        '(which-func ((t (:foreground "blue"))))))
;;     ;; Set the header line
;;     (setq header-line-format
;;           (list "-"
;;                 '(which-func-mode ("" which-func-format))
;;                 '("" ;; invocation-name
;;                   (:eval (if (buffer-file-name)
;;                              (concat "[" (sl/make-header) "]")
;;                            "[%b]")))
;;                 )
;;           )
;;     )
;;   )


;; I don't want to use TAGS files - remove it from the list of backend
;; functions
(require 'xref)
(setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions))


;; see http://stackoverflow.com/questions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr
;; (setq solarized-high-contrast-mode-line t) ;; this fixes the spurious underline in the modeline
;; (defvar thi::theme 'doom-wilmersdorf)
;; (defvar thi::theme 'solarized-light)
;; (defvar thi::theme 'modus-operandi)
;; (defvar thi::theme 'dracula)
;; (defvar thi::theme 'tango-dark)
(defvar thi::theme 'sanityinc-tomorrow-night)
;; (defvar thi::theme 'leuven)

;; (defvar thi::theme
;;   (if (string= system-name "dirac")
;;       'solarized-dark
;;     'solarized-light))

(set-face-attribute 'mode-line nil :inherit 'variable-pitch)

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

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; this was only needed because
;; https://github.com/Malabarba/smart-mode-line/pull/249 was not merged
;; (quelpa '(smart-mode-line :fetcher github :repo "thisch/smart-mode-line"))

(quelpa '(numpydoc :fetcher github :repo "douglasdavis/numpydoc.el"))
;; (quelpa '(nano-theme :fetcher github :repo "rougier/nano-theme"))

;; (quelpa '(pdf-tools :fetcher file :path "/home/thomas.hisch/gitrepos/pdf-tools/lisp"))

(setq use-package-ensure-function 'quelpa)

(use-package quelpa-use-package :ensure t)


;; (use-package bookmark+
;;   :quelpa (bookmark+ :fetcher wiki
;;                      :files
;;                      ("bookmark+.el"
;;                       "bookmark+-mac.el"
;;                       "bookmark+-bmu.el"
;;                       "bookmark+-1.el"
;;                       "bookmark+-key.el"
;;                       "bookmark+-lit.el"
;;                       "bookmark+-doc.el"
;;                       "bookmark+-chg.el"))
;;   :defer 2)

;; byte compiler warnings
;; (use-package 2048
;;   :ensure t)

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

(use-package autorevert
  :diminish)

(use-package avy
  :ensure t
  :bind
  (("M-g f" . avy-goto-line))
  )

;; (use-package better-shell
;;     :ensure t
;;     :bind (("C-'" . better-shell-shell)
;;            ("C-;" . better-shell-remote-open)))

(use-package bicycle
  ;; for emacs-lisp buffers
  :ensure t
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)
              ([S-tab] . bicycle-cycle-global)))

;; (use-package blamer
;;   :quelpa ((blamer :fetcher github :repo "artawower/blamer.el") :upgrade t)
;;   :custom
;;   (blamer-idle-time 0.3)
;;   (blamer-min-offset 70)
;;   :custom-face
;;   (blamer-face ((t :foreground "#7a88cf"
;;                     :background nil
;;                     :height 140
;;                     :italic t)))
;;   :config
;;   (global-blamer-mode 1))

;; (use-package bpr :ensure t
;;   :config
;;   (setq bpr-colorize-output t) ;; use -color-apply-on-region function on output buffer
;;   (setq bpr-process-mode #'comint-mode))

;; (use-package bufler
;;   :quelpa (bufler :fetcher github :repo "alphapapa/bufler.el"))

(use-package bug-reference-github
  ;; Automatically set `bug-reference-url-format' and enable
  ;; `bug-reference-prog-mode' in Emacs buffers from Github repositories.
  ;; https://github.com/arnested/bug-reference-github
  :ensure t
  :disabled t
  :config
  (add-hook 'prog-mode-hook 'bug-reference-github-set-url-format)
  )

(use-package cargo :ensure t)
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
  :bind (("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-h s" . counsel-info-lookup-symbol)
         ("C-x C-f" . counsel-find-file))

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

  ;; you can check whether rg has built with pcre2 by calling
  ;; rg --pcre2-version

  ;; https://github.com/BurntSushi/ripgrep/releases/download/11.0.2/ripgrep_11.0.2_amd64.deb

  ;; TODO try (grep-apply-setting
  ;; 'grep-find-command
  ;;'("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)
  ;;)
  ;; taken from https://stegosaurusdormant.com/emacs-ripgrep/

  :after (dumb-jump hydra)
  :bind (("C-c d" . deadgrep)
         ("C-c D" . hydra-deadgrep/body)
         )
  :config

  (set-face-attribute 'deadgrep-filename-face  'nil :inherit 'magit-section-heading)

  (setq dumb-jump-project-denoters (delete "Makefile" dumb-jump-project-denoters))

  ;; override deadgrep--project-root to include support for dumb-jump files (.dumbjump, .dumbjumpignore)
  (defun deadgrep--project-root ()
    "Guess the project root of the given FILE-PATH."
    (let ((root default-directory)
          (project (locate-dominating-file default-directory #'dumb-jump-get-config)))
      (when project
        (setq root project))
      (when root
        (deadgrep--lookup-override root))))

  (defun thi::deadgrep--project-root-current-repo ()
    (let ((root default-directory)
          (project (cdr (project-current))))
      (when project
        (setq root project))
      (when root
        (deadgrep--lookup-override root))))

  (defvar thi::deadgrep-without-tests-glob nil)

  (defun deadgrep-python (search-term)
    (interactive (list (deadgrep--read-search-term)))
    (let ((deadgrep--file-type '(type . "py")))
      (deadgrep search-term)))

  (defun deadgrep-python-without-tests (search-term)
    (interactive (list (deadgrep--read-search-term)))
    (let ((deadgrep--file-type '(type . "py"))
          (thi::deadgrep-without-tests-glob t))
      (deadgrep search-term)))

  (defun deadgrep-python-without-tests-in-current-repo (search-term)
    (interactive (list (deadgrep--read-search-term)))
    (let ((deadgrep-project-root-function #'thi::deadgrep--project-root-current-repo))
      (deadgrep-python-without-tests search-term)))

  (defun deadgrep-in-current-repo (search-term)
    (interactive (list (deadgrep--read-search-term)))
    (let ((deadgrep-project-root-function #'thi::deadgrep--project-root-current-repo))
      (deadgrep search-term)))

  (defhydra hydra-deadgrep (:color blue)
    "thi/deadgrep"
      ("p" deadgrep-python "py")
      ("t" deadgrep-python-without-tests "py witout tests")
      ("T" deadgrep-python-without-tests-in-current-repo "py witout tests")
      ("r" deadgrep-in-current-repo "in current repo"))

  ;; exclude tests from search results
  ;; (this works: rg --files -tpy -g '!tests')
  (defun deadgrep--arguments (search-term search-type case context)
    "Return a list of command line arguments that we can execute in a shell
to obtain ripgrep results."
    (let (args)
      (push "--color=ansi" args)
      (push "--line-number" args)
      (push "--no-heading" args)
      (push "--with-filename" args)

      (cond
       ((eq search-type 'string)
        (push "--fixed-strings" args))
       ((eq search-type 'words)
        (push "--fixed-strings" args)
        (push "--word-regexp" args))
       ((eq search-type 'regexp))
       (t
        (error "Unknown search type: %s" search-type)))

      (cond
       ((eq case 'smart)
        (push "--smart-case" args))
       ((eq case 'sensitive)
        (push "--case-sensitive" args))
       ((eq case 'ignore)
        (push "--ignore-case" args))
       (t
        (error "Unknown case: %s" case)))

      (cond
       ((eq deadgrep--file-type 'all))
       ((eq (car-safe deadgrep--file-type) 'type)
        (push (format "--type=%s" (cdr deadgrep--file-type)) args))
       ((eq (car-safe deadgrep--file-type) 'glob)
        (push (format "--type-add=custom:%s" (cdr deadgrep--file-type)) args)
        (push "--type=custom" args))
       (t
        (error "Unknown file-type: %S" deadgrep--file-type)))

      (when context
        (push (format "--before-context=%s" (car context)) args)
        (push (format "--after-context=%s" (cdr context)) args))

      ;; TODO add support for toggling this (see https://github.com/Wilfred/deadgrep/issues/75)
      (when thi::deadgrep-without-tests-glob
        (push "--glob=!tests" args))

      (push "--" args)
      (push search-term args)
      (push "." args)

      (nreverse args)))

  )

;; see https://ligerlearn.com/using-emacs-edit-files-within-docker-containers/
(use-package docker-tramp :ensure t :defer t)

(use-package dockerfile-mode :ensure t :defer t)

(use-package doom-themes :ensure t)

(use-package diminish :ensure t)

(use-package dired :ensure nil
  :config
  ;; human readable sizes
  (setq dired-listing-switches "-alh")
  )

(use-package dracula-theme :ensure t)

(use-package debbugs :ensure t)

(use-package dumb-jump
  :ensure t
  :demand t
  :custom
  ;; the problem with this setting is that too many build artifacts are
  ;; shown in the dumb-jump completions. I would only like to un-ignore a
  ;; specific folder in ripgrep, is this possible?
  ;; see https://github.com/BurntSushi/ripgrep/issues/969#issuecomment-400336680
  ;; (dumb-jump-rg-cmd "rg --no-ignore") ;; --no-ignore is needed here if some definitons
  ;;                                     ;; are only part of automatically
  ;;                                     ;; generated files (e.g. the pb2.py
  ;;                                     ;; files)
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-selector 'ivy)
  :config
  ;;
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)

  ;; TODO adapt this s.t. it can be used instead of the xref-completion buffer
  ;;   (defun my-xref--show-defs-minibuffer (fetcher alist)
  ;;   (let* ((xrefs (funcall fetcher))
  ;;          (xref-alist (xref--analyze xrefs))
  ;;          (xref (if (not (cdr xrefs))
  ;;                    (car xrefs)
  ;;                  (cadr (assoc (completing-read "Jump to definition: " xref-alist)
  ;;                               xref-alist)))))
  ;;     (xref-pop-to-location xref (assoc-default 'display-action alist))))

  ;; (setq xref-show-definitions-function 'my-xref--show-defs-minibuffer)
)
  ;; :bind (("M-g o" . dumb-jump-go-other-window)
  ;;        ("M-g j" . dumb-jump-go)
  ;;        ("M-g i" . dumb-jump-go-prompt)
  ;;        ("M-g x" . dumb-jump-go-prefer-external)
  ;;        ("M-g z" . dumb-jump-go-prefer-external-other-window)
  ;;        ("M-g p" . dumb-jump-back)
  ;;        ;; :map python-mode-map
  ;;        ;; ("M-." . dumb-jump-go)
  ;;        ;; ("M-," . dumb-jump-back)
  ;;        ;; :map c++-mode-map
  ;;        ;; ("M-." . dumb-jump-go)
  ;;        ;; ("M-," . dumb-jump-back)
  ;;        )
  ;; )

(use-package cc-mode
  :config
  (add-hook 'cc-mode-hook 'dumb-jump-mode)
  )

(use-package code-review
  :ensure t
  :config
  ;; do not open a new window
  (setq code-review-new-buffer-window-strategy #'switch-to-buffer)
  )


(use-package ediff
  :ensure t
  :custom
  ;;https://oremacs.com/2015/01/17/setting-up-ediff/
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-diff-options "-w"))

(use-package edit-indirect
  ;; Edit regions in separate buffers, like `org-edit-src-code' but for arbitrary
  ;; regions

  ;; copied from https://gist.github.com/fleimgruber/cd2386d8326fad3e0e2b99ebc2f05739
  ;; referenced in https://github.com/jorgenschaefer/elpy/issues/498#issuecomment-324145571
  ;; needed for editing python docstring in new buffers that are in rst-mode
  ;; TODO add some rst validitiy checks before commiting change in rst-mode buffer.
  ;; FIXME indendation is not preserved

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

(use-package envrc :ensure t
  ;;A GNU Emacs library which uses direnv to set environment variables on a
  ;;per-buffer basis. This means that when you work across multiple projects
  ;;which have .envrc files, all processes launched from the buffers "in"
  ;;those projects will be executed with the environment variables specified
  ;;in those files. This allows different versions of linters and other
  ;;tools to be used in each project if desired.
  ;;https://github.com/purcell/envrc
  :config
  (envrc-global-mode)
  )

(use-package equake
    :ensure t
    :config  ; some examples of optional settings follow:
    (global-set-key (kbd "C-x C-c") 'equake-check-if-in-equake-frame-before-closing) ; prevent accidental frame-closure
    (setq equake-size-width 0.99) ; set width a bit less than full-screen (prevent 'overflow' on multi-monitor)
    ;; set distinct face for Equake: white foreground with dark blue background, and different font
    (set-face-attribute 'equake-buffer-face 'nil :inherit 'default :family "DejaVu Sans Mono" :background "#000022" :foreground "white"))

(use-package evil
  :ensure t
  :config (progn
            (setq evil-default-state 'emacs)
            (cl-loop for (mode . state) in
                     '(
                       (help-mode . emacs)
                       (term-mode . emacs)
                       (helpful-mode . emacs)
                       (eshell-mode . emacs)
                       (inferior-emacs-lisp-mode . emacs)
                       )
                     do (evil-set-initial-state mode state))))

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
  (flycheck-python-mypy-executable (expand-file-name "~/miniconda3/bin/mypy"))
  (flycheck-python-pylint-executable (expand-file-name "~/miniconda3/bin/python"))
  :init (progn
          (setq flycheck-highlighting-mode 'lines)
          (setq flycheck-display-errors-delay 0.4)
          (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
          (add-hook 'python-mode-hook
                    (lambda () (flycheck-select-checker 'python-pylint)))
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
    :command ("python"
              (eval (flycheck-python-module-args 'python-pylint "pylint"))
              "--reports=n"
              "--output-format=json"
              (config-file "--rcfile=" flycheck-pylintrc concat)
              "--from-stdin" source-original)
    :standard-input t
    :error-parser flycheck-parse-pylint
    :enabled (lambda ()
               (or (not (flycheck-python-needs-module-p 'python-pylint))
                   (flycheck-python-find-module 'python-pylint "pylint")))
    :verify (lambda (_) (flycheck-python-verify-module 'python-pylint "pylint"))
    :modes python-mode
    ;; :next-checkers ((warning . python-mypy))
    ))


(use-package flycheck-package
  :ensure t)

(use-package flycheck-pycheckers
  :ensure t
  :disabled t
  ;; TODO replace source-inplace in definition of pycheckers checker by source-original
  :init (setq flycheck-pycheckers-checkers '(
                                             pylint
                                             ;; flake8
                                             )))

(use-package forge
  :ensure t
  :after magit)

(use-package dashboard-table
  :load-path "~/gitrepos/dashboard-table.el"
  :config
  (require 'dashboard-table)
  )

(use-package gerrit
  :load-path "~/gitrepos/gerrit.el"
  :custom
  (gerrit-host
     (if thi::at-work
         "gerrit.rnd.ims.co.at"
       ;; (gerrit-host "gerrit.googlesource.com")
       "review.gerrithub.io"
       ;; "review.opendev.org"
       ))
  (gerrit-save-file (concat thi::cache-file-dir "/git-review"))
  (gerrit-use-gitreview-interface nil)
  :config
  (progn
    ;; (add-hook 'magit-status-sections-hook #'gerrit-magit-insert-status t)
    (global-set-key (kbd "C-x i") 'gerrit-upload)
    (global-set-key (kbd "C-x o") 'gerrit-download)

    ;; (with-eval-after-load 'auth-source
    ;;   ;; using authinfo.gpg always requires me to specify the gpg passowrd
    ;;   ;; in a system/gnome dialog
    ;;   (setq auth-sources '("~/.authinfo.gpg"
    ;;                        "secrets:Login")))

    (defun gerrit-add-verify-comment ()
      (interactive)
      (gerrit-rest-change-add-comment
       (gerrit-get-unique-changeid-from-current-commit)
        "/verify"))

    (defun thi::upload-and-verify (&optional args)
      (interactive
       (list (transient-args 'gerrit-upload-transient)))
      (gerrit-upload:--action args)
      ;; wait until the push action is ready (this is only needed if
      ;; the change didn't exist on the server before thi::upload-and-verify
      ;; was called.
      ;; -> wait until magit-this-process is nil
      ;; TODO let bind (inhibit-magit-process t) before running
      ;; gerrit-upload:--action (then magit-this-process can be removed)
      (while magit-this-process
        (message "magit process still active")
        (sleep-for 0.3))
      (gerrit-add-verify-comment))

    ;; https://github.com/magit/magit/wiki/Converting-popup-modifications-to-transient-modifications
    (transient-append-suffix 'gerrit-upload-transient "v"
      '("V" "upload and verify" thi::upload-and-verify))

    (defun thi::gerrit-topic-remove-all-my-votes (topicname)
      (interactive "sEnter a topic: \n")
      (gerrit-rest-topic-delete-cr-vote topicname "self")
      (gerrit-rest-topic-delete-verified-vote topicname "self"))

    (defun thi::gerrit-topic-all-votes (topicname)
      ;; TODO and submit
      (interactive "sEnter a topic: \n")
      (cl-loop for change-info in (gerrit-rest-get-topic-info topicname) do
               (let* ((changenr (gerrit-rest--change-info-to-unique-changeid change-info))
                      (labels (alist-get 'labels change-info))
                      (all-cr-labels (alist-get 'all (alist-get 'Code-Review labels)))
                      (all-verified-labels (alist-get 'all (alist-get 'Verified labels))))
                 ;; CR votes
                 ;; --------
                 (seq-do (lambda(x)
                           (when (eq -2 (alist-get 'value x))
                             (message "remove CR -2")
                             (gerrit-rest-change-delete-CR-vote
                              changenr (alist-get 'username x))))
                         all-cr-labels)
                 ;; if there is no +2 we have to add it
                 (unless (member 2 (seq-map
                                    (lambda(x) (alist-get 'value x))
                                    all-cr-labels))
                   (message "add CR +2")
                   (gerrit-rest-change-set-cr-vote changenr "+2" ""))

                 ;; Verified votes
                 ;; --------------
                 (seq-do (lambda(x)
                           (when (eq -1 (alist-get 'value x))
                             (message "remove Verified -1")
                             (gerrit-rest-change-delete-verified-vote
                              changenr (alist-get 'username x))))
                         all-verified-labels)
                 ;; if there is no +1 we have to add it
                 (unless (member 1 (seq-map
                                    (lambda(x) (alist-get 'value x))
                                    all-verified-labels))
                   (message "add Verified +1")
                   (gerrit-rest-change-set-verified-vote changenr "+1" "")))))

    ;; TODO support code threads: on a change basis or topic basis and add
    ;; keybindings that allow commenting on them and resolving them

    (defun gerrit-section-filter (message-info)
      "Filter function run for every gerrit comment.

This function is called for every comment MESSAGE-INFO of a
gerrit change.  If the function returns t, the comment is shown
in the section buffer. This is useful for filtering out e.g.
comments from CI tools."
      (not (or (s-starts-with? "jenkins" (alist-get 'name (alist-get 'author message-info)))
          (s-ends-with? "/verify" (alist-get 'message message-info)))))

    ;; make the number col a bit smaller
    (aset gerrit-dashboard-columns 0 '("Number" 6))
    (aset gerrit-dashboard-columns 1 '("Subject" 20))

    (defun gerrit-dashboard-sd-odd ()
      (interactive)
      (let* (
            (group-members '(
                             "emanuel.avasalcai"
                             "peter.dzubaj"
                             "matthias.madzak"
                             "patrik.puchala"
                             "thomas.hisch"
                             "dmyto.bondal"
                             "sergey.borovikov"
                             "antoine.rougier"
                             "mihail.georgescu"
                             "lubomir.panak"))

            (group-addresses
             (seq-map (lambda (elt) (concat elt "@" "ims.co.at")) group-members))
            (gerrit-dashboard-query-alist
             '(
               ("Waiting for +1" . "is:open assignee:sd-odd label:Code-Review=0")
               ("Waiting for +2" . "is:open assignee:sd-odd label:Code-Review=1")
               ("Waiting for SD-even" . (concat "is:open assignee:sd-even ("
                                                (s-join " OR " group-addresses)
                                                ")"))
               )
             )
            (gerrit-dashboard-columns
             (seq-into
              (seq-filter (lambda (elt) (not (string= (car elt) "Assignee")))
                          gerrit-dashboard-columns)
              'vector))
            (gerrit-dashboard-buffer-name "*gerrit-odd-standup*")
            ;; this is a workaround for the text-scale increase bug/feature
            ;; see debbugs 41852
            (tabulated-list-use-header-line nil)
            )
        (gerrit-dashboard)))

    (defun gerrit-dashboard-mine()
      (interactive)
      (let*
          ((gerrit-dashboard-query-alist
             '(
               ("owned" . "is:open owner:me")
               )
             )
            (gerrit-dashboard-buffer-name "*gerrit-dashboard-mine*")
            ;; this is a workaround for the text-scale increase bug/feature
            ;; see debbugs 41852
            (tabulated-list-use-header-line nil)
            )
        (gerrit-dashboard)))


    (setq gerrit-project-to-local-workspace-alist
          '(
            ;; TODO write a function that generates this map
            (("software/ipycore" "main") "~/sandbox/main/ipycore")
            (("software/optics/cec" "main") "~/sandbox/main/cec")
            (("software/common" "main") "~/sandbox/main/common")
            (("software/tec" "main") "~/sandbox/main/tec")
            (("software/integrationtests" "main") "~/sandbox/main/integrationtests")
            (("software/blocks" "main") "~/sandbox/main/blocks")
            (("software/jobqueue" "main") "~/sandbox/main/jobqueue")
            (("software/expses" "main") "~/sandbox/main/expses")
            ))

    ;; TODO think about using `gerrit-project-to-local-workspace-alist' here
    (setq gerrit-interesting-open-changes-filter (concat
                                                  "is:open ("
                                                  ;;TODO use s-join
                                                  "project:software/ipycore OR "
                                                  "project:software/common OR "
                                                  "project:software/optics/cec OR "
                                                  "project:software/jobqueue OR "
                                                  "project:software/expses OR "
                                                  "project:software/blocks OR "
                                                  "project:software/integrationtests)"))
    ;; debug commands
    (defun thi::gerrit-mywip ()
      (interactive)
      (profiler-start 'cpu)
      (gerrit-dashboard--get-data "is:open is:WIP limit:3")
      (profiler-stop)
      ;; run profiler-report manually
      )

    (defun thi::gerrit-dashboard-gerritforge ()
      (interactive)
      (let ((gerrit-dashboard-query-alist
             '(
               ("Open" . "is:open -is:wip limit:15")
               ("Unassigned / Unreviewed" . "-assignee label:Code-Review=0 limit:15")
               )
             )
            (gerrit-dashboard-buffer-name "*gerrit-gerritforge*")
            ;; this is a workaround for the text-scale increase bug/feature
            ;; see debbugs 41852
            (tabulated-list-use-header-line nil)
            )
        (with-current-buffer (get-buffer-create gerrit-dashboard-buffer-name)
          (text-scale-set -3))
        (gerrit-dashboard)))
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

    ;; (defun git-commit-read-jira-ticket ()
    ;;   (list (read-string "Jira Ticket: ")))

    (defun git-commit-read-jira-ticket ()
      (list (car (s-match thi::bug-reference-bug-regexp (firefox-places)))))

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

(use-package git-modes
  ;; Previously the contents of the git-modes repository were available as three
  ;; separate packages gitattributes-mode, gitconfig-mode and gitignore-mode.
  ;; now they are part of one pkg.
  :ensure t)

(use-package grip-mode
  ;; preview markdown files
  ;; https://github.com/seagle0128/grip-mode
  ;; requires: pip install grip
  :ensure t
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

(use-package groovy-mode
  :ensure t
  :defer t
  :mode "Jenkinsfile")

;; TODO help-mode+ is unavailable??
;; (use-package help-mode+ :ensure t)

(use-package helpful :ensure t
  :disabled t
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

(use-package help
  :custom
  (describe-bindings-outline t))

(use-package hydra :ensure t
  :init
  (progn
    (defun thi::text-scale-reset ()
      (interactive)
      (text-scale-set 0))

    (defun thi::frame-font-increase ()
      (interactive)
      (thi::set-font-size 18))

    (defun thi::frame-font-reset ()
      (interactive)
      (thi::set-font-size 14))

    (defhydra hydra-zoom-winner (global-map "<f5>")
      ;; Now, <f5> g 4g 2l will zoom in 5 times, and zoom out 2 times for a
      ;; total of +3 zoom.
      "zoom/winner"
      ("G" thi::frame-font-increase "zoom frame font")
      ("L" thi::frame-font-reset "reset frame font")
      ("g" text-scale-increase "zoom in")
      ("l" text-scale-decrease "zoom out")
      ("R" thi::text-scale-reset "zoom reset")
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

(use-package idle-highlight-mode
  :ensure t
  :config
  ;; this mode highlights selected (via point) symbols
  ;; TODO give https://github.com/wolray/symbol-overlay a try (does it do the same?)
  (add-hook 'prog-mode-hook #'idle-highlight-mode)
  )

(use-package image-mode
  :ensure nil
  :bind (:map image-mode-map
              ("<right>" . image-next-file)
              ("<left>" . image-previous-file)
              ("<DEL>" . thi::go-back-to-dired))
  :config
  (defun thi::go-back-to-dired ()
    (interactive)
    (dired-at-point default-directory)))

(use-package imenu-anywhere
  :ensure t
  :defer t
  :init (global-set-key (kbd "C-.") 'imenu-anywhere)
  :config (defun jcs-use-package ()
            (add-to-list 'imenu-generic-expression
             '("Used Packages"
               "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
  (add-hook 'emacs-lisp-mode-hook #'jcs-use-package))

(use-package immortal-scratch
  :ensure t
  :config
  (immortal-scratch-mode))

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
  (ivy-initial-inputs-alist nil)
  ;; todo not really needed?
  (ivy-use-selectable-prompt t)
  :config (progn
            (ivy-mode)
            ;; see https://oremacs.com/2016/06/27/ivy-push-view/
            (global-set-key (kbd "C-c v") 'ivy-push-view)
            (global-set-key (kbd "C-c V") 'ivy-pop-view)

            ;; doesn't work if set in :custom for whatever reason
            (setq ivy-re-builders-alist
                  '((read-file-name-internal . ivy--regex-fuzzy)
                    (counsel-M-x . ivy--regex-fuzzy) ;; I'm not satisfied with this. "lsp"
                    ;; should give me "lsp" and "conda"
                    ;; should expand to
                    ;; "thi::.....conda-py37". In smex
                    ;; everything worked as expected. Is
                    ;; there a smex backend for
                    ;; counsel-M-x?
                    (counsel-describe-variable . ivy--regex-fuzzy)
                    (counsel-describe-function . ivy--regex-fuzzy)
                    (t . ivy--regex-plus)))
            ))

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


;; (use-package kubernetes
;;   :ensure t)


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

(use-package jiralib2 :ensure t


  ;; (let ((request-backend 'url-retrieve))
  ;;   (request (concat jiralib2-url "/rest/api/2/myself")
  ;;     :sync t
  ;;     :parser 'json-read
  ;;     :headers `(("Content-Type" . "application/json")
  ;;                ("Authorization" . ,(format "Basic %s"  jiralib2--session)))))

  :init
  (setq jiralib2-url             "https://jira.rnd.ims.co.at"
        jiralib2-auth            'token             ;; 'cookie, 'token or 'basic
        jiralib2-user-login-name "I010229"  ;; email for JIRA Cloud
        jiralib2-token            nil))              ;; for token auth with JIRA Cloud

;; this depends on an outdated json pkg
;; (use-package json-mode :ensure t :defer t)

(use-package link-hint
  :ensure t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

(use-package lsp-mode
  :ensure t
  :init
  ;; Set it to big number(100mb) like most of the popular starter kits like
  ;; Spacemacs/Doom/Prelude, etc do:
  (setq gc-cons-threshold 100000000)
  ;; Increase the amount of data which Emacs reads from the process. Again
  ;; the emacs default is too low 4k considering that the some of the
  ;; language server responses are in 800k - 3M range.
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  :hook
  ;; how do I test that lsp works?
  ;; (lsp-decribe-session) ;; does this have a refresh keybindin? like g in magit-status?
  ((prog-mode-hook . lsp))
  :config
  (setq gc-cons-threshold (* 100 1024 1024) ;; see performance section in lsp-mode documentation
        read-process-output-max (* 1024 1024) ;; same
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        create-lockfiles nil ;; lock files will kill `npm start'
        lsp-headerline-breadcrumb-enable t)

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

  (defun thi::lsp-python-use-conda-py37-settings ()
    (interactive)
    (setq lsp-pyls-server-command "~/miniconda/envs/py37/bin/pyls"
          lsp-clients-python-library-directories '("~/miniconda/envs/py37/"))
    )
  )

(use-package lsp-ui
  :ensure t
  :config
  ;; make sure we have lsp-imenu everywhere we have LSP
  (require 'lsp-ui-imenu)
  ;; (add-hook 'lsp-after-open-hook 'lsp-ui-imenu)
  )

(use-package magit
  ;; TODO also study vc-annotate (l - opens commit under point)

  ;; TODO how can magit-blame be used?

  ;; bindings: C-c M-g: magit-file-dispatch (use it in a buffer)
  ;;           C-x g: magit-status
  ;;           C-x M-g: magit-dispatch
  :ensure t
  :bind (
         (("C-x g" . magit-status)) ;; someone bound C-x g to revert-buffer ....
         ("C-c g" . 'magit-file-dispatch))
  :custom
  (magit-repository-directories `(("~/gitrepos" . 1)
                                  ("~/.emacs.d" . 0)
                                  ("~/sandbox/main" . 1)))
  (magit-completing-read-function 'ivy-completing-read)
  :config
  (defun magit-gpr ()
    (interactive)
    (magit-git-command "git pull --rebase --autostash")
    )
  (defun magit-switch-to-main-gpr ()
    (interactive)
    (magit-git-command "git checkout main")
    (magit-git-command "git pull --rebase --autostash")
    )
  (defun magit-reset-to-upstream ()
    (interactive)
    ;; TODO confirmation
    (magit-git-command "git reset --hard @{upstream}")
    )
  (defun magit-switch-to-upstream-branch ()
    (interactive)
    ;; the git ref-parse command always outputs the remote name (in my case origin). let's remove it
    (magit-git-command "git switch `git rev-parse --abbrev-ref --symbolic-full-name @{u} | sed 's|^origin/||'`")))

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
  ;; helpful key-bindings:
  ;; C-c C-o open link at point

  ;; <C-S-return> org insert todo heading respsect content
  ;; <M-S-return> org insert todo heading  (immediately below point)

  ;; <C-return> org insert heading respsect content
  ;; <M-return> org insert heading  (immediately below point)
  ;; TODO BEGIN_SRC
  ;; <S-left/right> switch to the next TODO,DONE,.. keyword
  ;; alternative: C-c C-t
  ;; <C-c C-,> insert +BEGIN_SRC block see https://emacs.stackexchange.com/a/47370/2761
  ;; narrow/widen:
  ;;     org-narrow-to-subtree: (C-x n s) will display only the current heading.
  ;;     It does however include the heading itself, not just the text.
  ;;     widen: (C-x n w) will widen the view again.

  :ensure t
  :custom
  ;; this will turn off asking for a confirmation
  ;; (org-agenda-files (quote ("~/gitrepos/orgnotes/studium.org"
  ;;                           "~/gitrepos/orgnotes/laserPA.org"
  ;;                           "~/gitrepos/orgnotes/diplomarbeit.org"
  ;;                           "~/gitrepos/orgnotes/physik.org"
  ;;                           "~/gitrepos/orgnotes/ims.org"
  ;;                           "~/gitrepos/orgnotes/emacs.org")))
  (org-confirm-babel-evaluate nil)
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
  :bind (("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ;; ("C-c b" . org-iswitchb)
         :map org-mode-map
         ("C-`" . helm-semantic-or-imenu)
         )
  :config

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)


  ;; fontify src blocks, doesn't work with fixed-pitch blocks :(
  ;;(setq org-src-fontify-natively t)

  ;; (setq org-hide-leading-stars t)
  ;; (setq org-use-speed-commands t)
  ;; (setq org-speed-commands-user (quote (("0" . delete-window)
  ;;                                       ("1" . delete-other-windows)
  ;;                                       ("2" . split-window-vertically)
  ;;                                       ("3" . split-window-horizontally)
  ;;                                       ("h" . hide-other)
  ;;                                       ("k" . org-kill-note-or-show-branches)
  ;;                                       ("r" . org-reveal)
  ;;                                       ("s" . org-save-all-org-buffers)
  ;;                                       ("z" . org-add-note)
  ;;                                       ("c" . self-insert-command)
  ;;                                       ("C" . self-insert-command)
  ;;                                       ("J" . org-clock-goto))))
  ;;

  ;; taken from http://tincman.wordpress.com/2011/01/04/research-paper-management-with-emacs-org-mode-and-reftex/
  ;;

  ;; needed because every time when i load a org file a get asked for a master file name ?!?!!
  ;; (setq reftex-default-bibliography
  ;;       '("~/gitrepos/orgnotes/refs.bib"))

  ;; (setq org-link-abbrev-alist
  ;;       '(("bib" . "~/gitrepos/orgnotes/refs.bib::%s")
  ;;         ("diplomarbeit" . "~/gitrepos/orgnotes/diplomarbeit.org::#%s")
  ;;         ("papers" . "~/Dropbox/research/%s.pdf")))

  ;; (defun org-mode-reftex-search ()
  ;;   ;;jump to the notes for the paper pointed to at from reftex search
  ;;   (interactive)
  ;;   (org-open-link-from-string (format "[[diplomarbeit:%s]]" (reftex-citation t))))

  ;; (defun org-mode-reftex-setup ()
  ;;   (load-library "reftex")
  ;;   (and (buffer-file-name) (file-exists-p (buffer-file-name))
  ;;        (progn
  ;;          ;enable auto-revert-mode to update reftex when bibtex file changes on disk
  ;;          (global-auto-revert-mode t)

  ;;          ;; this prevent reftex asking for the master file
  ;;          (make-local-variable 'TeX-master)
  ;;          (setq TeX-master t)

  ;;          (reftex-parse-all)

  ;;          ;add a custom reftex cite format to insert links
  ;;          (reftex-set-cite-format
  ;;           '((?b . "[[bib:%l][%l-bib]]")
  ;;             (?n . "[[diplomarbeit:%l][%l-diplomarbeit]]")
  ;;             (?p . "[[papers:%l][%l-paper]]")
  ;;             (?c . "\\cite{%l}")
  ;;             (?t . "%t")
  ;;             (?h . "\n** [[papers:%l][%t]]\n  :PROPERTIES:\n  :Custom_ID: %l\n  :END:\n")))))

  ;;   (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  ;;   (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

  ;; (add-hook 'org-mode-hook 'org-mode-reftex-setup)
  ;; (add-hook 'org-mode-hook 'turn-on-visual-line-mode)

  ;;beamer

  ;; allow for export=>beamer by placing

  ;; #+LaTeX_CLASS: beamer in org files
  ;; (add-to-list 'org-latex-export-classes
  ;;              ;; beamer class, for presentations
  ;;              '("beamer"
  ;;                "\\documentclass[11pt]{beamer}\n
  ;;       \\mode<{{{beamermode}}}>\n
  ;;       \\usetheme{{{{beamertheme}}}}\n
  ;;       \\usecolortheme{{{{beamercolortheme}}}}\n
  ;;       \\beamertemplateballitem\n
  ;;       \\setbeameroption{show notes}\n
  ;;       \\usepackage[utf8]{inputenc}\n
  ;;       \\usepackage[T1]{fontenc}\n
  ;;       \\usepackage{hyperref}\n
  ;;       \\usepackage{color}\n
  ;;       \\usepackage{verbatim}\n
  ;;       \\newcommand{\\xvec}{\\mathbf{x}}\n
  ;;       \\institute{{{{beamerinstitute}}}}\n
  ;;       \\subject{{{{beamersubject}}}}\n"

  ;;                ("\\section{%s}" . "\\section*{%s}")

  ;;                ("\\begin{frame}[fragile]\\frametitle{%s}"
  ;;                 "\\end{frame}"
  ;;                 "\\begin{frame}[fragile]\\frametitle{%s}"
  ;;                 "\\end{frame}")))

  ;; ;; letter class, for formal letters

  ;; (add-to-list 'org-latex-export-classes
  ;;              '("letter"
  ;;                "\\documentclass[11pt]{letter}\n
  ;;                 \\usepackage[utf8]{inputenc}\n
  ;;                 \\usepackage[T1]{fontenc}\n
  ;;                 \\usepackage{color}"

  ;;                ("\\section{%s}" . "\\section*{%s}")
  ;;                ("\\subsection{%s}" . "\\subsection*{%s}")
  ;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((emacs-lisp . nil)
  ;;    (python . t))))
  )

;; (use-package org-pdftools
;;   :ensure t)

;; (use-package org-projectile
;;   :bind (("C-c n p" . org-projectile-project-todo-completing-read)
;;          ("C-c c" . org-capture))
;;   :config
;;   (progn
;;     (setq org-projectile-projects-file "~/projects.org")
;;     (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
;;     (push (org-projectile-project-todo-entry) org-capture-templates))
;;   :ensure t)

(use-package origami
  :quelpa (origami :fetcher github :repo "emacs-origami/origami.el")
  :bind
  ("C-M-s" . origami-toggle-node)
  ("C-M-h" . origami-close-all-nodes)
  :hook
  (python-mode . origami-mode)
  ;;(origami-mode . origami-close-all-nodes)
  )

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
  :quelpa (pdf-tools :fetcher github :repo "vedang/pdf-tools")
  ;; new maintainer: https://github.com/vedang/pdf-tools
  ;; old maintainer/author: https://github.com/politza/pdf-tools

  ;; Notes:

  ;; "s" in pdf file opens occur mode (also shows line numbers in new occur buffer/window)
  ;;       -> to quickly jump to the desired line in the occur buffer, use avy-goto-line M-g f
  ;; "C-s" is mapped to isearch-forward, because swiper doesn't only shows raw pdf file contents
  ;;       https://github.com/abo-abo/swiper/issues/1407

  ;; horizontal scrolling: either the mouse (see mouse-wheel settings in
  ;; init.el) or use C-a, C=e.

  ;; DONE when opening pdf file show the last visited page
  ;;      - https://github.com/politza/pdf-tools/issues/18
  ;;      - see https://github.com/nicolaisingh/saveplace-pdf-view
  ;; TODO dual view mode (there is a project, which adds support for it, but which?)
  ;; TODO smooth scrolling: https://github.com/dalanicolai/pdf-continuous-scroll-mode.el
  ;;      -> IMO this package needs to be added to upstream pdf-tools
  ;; TODO minimap - like in evince (the column on the left with the thumbnails)

  :commands (pdf-view-mode) ;; otherwise void-function pdf-view-mode is
                            ;; raised when a PDF file is opened
  :ensure t
  :bind (:map pdf-view-mode-map
        ("C-s" . isearch-forward)
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
        ("r"  . pdf-view-reset-slice)
        ;; pdf-occur-buffer-mode-map - not defined at this point
        ;; :map pdf-occur-buffer-mode-map
        ;; ;; allows clicking on the pdf occur matches with the mouse
        ;; ;; an alternative is to use "RET" or use the "peek" feature C-o,
        ;; ;; where the active buffer remains the occur buffer. C-n C-o
        ;; ("<mouse-1>" . pdf-occur-goto-occurrence)
        )
  :config

  (with-eval-after-load 'pdf-occur
    (define-key pdf-occur-buffer-mode-map
      ;; allows clicking on the pdf occur matches with the mouse
      ;; an alternative is to use "RET" or use the "peek" feature C-o,
      ;; where the active buffer remains the occur buffer. C-n C-o
      (kbd "<mouse-1>") 'pdf-occur-goto-occurrence))

  (use-package saveplace-pdf-view ;; remembers the last location in the pdf file
    :ensure t)

  (pdf-tools-install)

  ;; I want to set the slice from the bounding box by default for all pdfs
  (push 'pdf-view-auto-slice-minor-mode pdf-tools-enabled-modes)

  (setq-default pdf-view-display-size 'fit-page))

  ;; TODO enable pdf-view-auto-slice-minor-mode


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

(use-package prog-mode
  :config

  (defun thi::customize-programming-language-mode ()
    (font-lock-add-keywords
     nil
     '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\|NOTE\\|REFACTOR\\|FIX\\)"
        1
        '(:box (:color "grey10" :line-width 2) :background "red" :foreground "yellow")
        prepend)))
    (setq show-trailing-whitespace t)
    (eldoc-mode -1)
    ;; (rainbow-delimiters-mode 1)
    ;; (idle-highlight-mode 1)
    ;; temporarily disabled the rainbow modes as i think they cause speed problems
    ;; (rainbow-mode 1)
    ;; (flyspell-prog-mode)
    )

  (dolist (mode
           '(prog-mode     ; This is the mode perl, makefile, lisp-mode, scheme-mode,
                           ; emacs-lisp-mode, sh-mode, java-mode, c-mode, c++-mode,
                           ; python-mode inherits from.
             protobuf-mode
             lua-mode
             cmake-mode
             tex-mode      ; LaTeX inherits
             sgml-mode     ; HTML inherits
             css-mode
             nxml-mode
             diff-mode
             haskell-mode
             rst-mode))
    (add-hook
     (intern (concat (symbol-name mode) "-hook"))
     #'thi::customize-programming-language-mode))

  (add-hook 'prog-mode-hook #'outline-minor-mode)
  (add-hook 'prog-mode-hook #'hs-minor-mode))

(use-package projectile :ensure t
  :custom (projectile-completion-system 'ivy)
  :bind (("C-x f" . projectile-find-file))
  :config (progn
            ;; deprecated in favor of projectile-mode
            ;; (projectile-global-mode t)

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

(use-package protobuf-mode
  :ensure t
  :config
  ;; (define-key protobuf-mode-map (kbd "M-.") #'dumb-jump-go)
  ;; (define-key protobuf-mode-map (kbd "M-,") #'dumb-jump-back)
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (progn
                (setq c-basic-offset 4)
                (setq tab-width 4)))))

(use-package pyimport :ensure t) ;; is solely (rip)grep based
;; https://github.com/anachronic/importmagic.el requires a python interpreter

(use-package python
  :custom
  (python-fill-docstring-style 'pep-257)
  (python-indent-guess-indent-offset-verbose nil)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  ;; TODO support jumping in multi git-repo project
  ;; (add-hook 'python-mode-hook 'dumb-jump-mode)

  (defun ims-yapf()
    (interactive)
    (when (string-match "sandbox" buffer-file-name)
      (add-hook 'before-save-hook #'py-isort-before-save nil t)
      ;; instead of doing (add-hook 'python-mode-hook 'yapf-mode), as
      ;; suggested in the README of yapfify we do
      (yapf-mode)))

  (defun thi::save-buffer-maybe-show-errors ()
    "Save buffer and show errors if any."
    (interactive)
    (save-buffer)
    (when (flycheck-has-current-errors-p)
      (flycheck-list-errors)))

  (when thi::at-work
    (add-hook 'python-mode-hook 'ims-yapf))
  ;; (define-key python-mode-map (kbd "M-.") #'dumb-jump-go)
  ;; (define-key python-mode-map (kbd "M-,") #'dumb-jump-back)
  (define-key python-mode-map (kbd "C-c C-i") #'pyimport-insert-missing)
  (define-key python-mode-map (kbd "C-c e") #'edit-indirect-rst)

  (define-key python-mode-map (kbd "C-x C-s") #'thi::save-buffer-maybe-show-errors)

  ;; :bind ;; see http://tuhdo.github.io/helm-intro.html#sec-6
  ;; (("C-`" . 'helm-semantic-or-imenu))
  (with-eval-after-load 'helm
    (bind-key "C-`" #'helm-semantic-or-imenu 'python-mode-map))
  )

(use-package python-docstring
  ;; this package should font-lock keywords in the docstrings, but it
  ;; currently doesn't support numpydoc keywords.

  ;; it also supports intelligent re-filling of docstrings.
  :ensure t
  :custom
  (python-docstring-sentence-end-double-space nil)
  ;; :config
  ;; ;; this is the same as adding a (python-docstring-mode) to
  ;; ;; python-mode-hook
  ;; (python-docstring-install))
)
;; (use-package python-pytest
;;   :ensure t
;;   :after python
;;   ;; see https://shahinism.com/en/posts/emacs-python-pytest/
;;   :custom
;;   (python-pytest-arguments
;;    '("--color"          ;; colored output in the buffer
;;      "--failed-first"   ;; run the previous failed tests first
;;      "--maxfail=5"))    ;; exit in 5 continuous failures in a run
;;   ;; TODO create key bindings for those functions
;;   ;; :config
;;   ;; (which-key-declare-prefixes-for-mode 'python-mode "SPC pt" "Testing")
;;   ;; (evil-leader/set-key-for-mode 'python-mode
;;   ;;   "ptp" 'python-pytest-popup
;;   ;;   "ptt" 'python-pytest
;;   ;;   "ptf" 'python-pytest-file
;;   ;;   "ptF" 'python-pytest-file-dwim
;;   ;;   "ptm" 'python-pytest-function
;;   ;;   "ptM" 'python-pytest-function-dwim
;;   ;;   "ptl" 'python-pytest-last-failed)
;;   )

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
    (rg-define-search thi::gg-no-tests :files "py" :flags ("--glob=!tests"))

    (rg-enable-default-bindings (kbd "M-s"))
    ;; (setq rg-show-header nil)
    (rg-define-toggle "--context 3" (kbd "C-c c"))
  ))

(use-package rust-mode :ensure t)

;; Persist history over Emacs restarts.
(use-package savehist
  :init
  (savehist-mode))

;; Package by Ian Eure (ieure on GitHub)
(use-package scratch
  :ensure
  :config
  (defun prot/scratch-buffer-setup ()
    ;; taken from https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/
    "Add contents to `scratch' buffer and name it accordingly."
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (concat "*Scratch for " mode "*") t)))
  :hook (scratch-create-buffer-hook . prot/scratch-buffer-setup)
  :bind ("C-c s" . scratch))

(use-package smart-mode-line
  :ensure t
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
                (lambda (f)
                  (with-selected-frame f
                    (when (window-system f)
                      (tool-bar-mode -1)
                      (menu-bar-mode -1)
                      (scroll-bar-mode -1)
                      (thi::set-font-size
                       (if thi::at-work
                           18
                         22))
                      (load-theme thi::theme t)
                      (thi::sml-setup)))))
    (progn
      (thi::set-font-size
       (if thi::at-work
           18
         22))
      (load-theme thi::theme t)
      (thi::sml-setup)
      (tool-bar-mode -1)
      (menu-bar-mode -1))))


;; does this improve counsel-M-x?
(use-package smex :ensure t)

;; (use-package spaceline
;;   :ensure t
;;   :demand t
;;   :init
;;   (setq powerline-default-separator 'arrow-fade)
;;   :config
;;   (require 'spaceline-config)
;;   (spaceline-spacemacs-theme))

(use-package spell-fu
  ;; spell fu uses the aspell database, which is stored in
  ;; /usr/lib64/aspell-0.60/en_US*  (contained in apsell-en fedora pkg)

  ;; https://gitlab.com/ideasman42/emacs-spell-fu

  ;; TODO use avy to select words that should be added to the local dictionary
  ;; TODO keybindigs for spell-fu-word-add
  ;; TODO ignore URLs
  ;; TODO ignore commented-out code - possible?
  :ensure t
  :custom
  (
   ;; alternative in https://gitlab.com/ideasman42/emacs-spell-fu/-/issues/4
   (ispell-personal-dictionary "~/.emacs.d/spell-fu/personal-dict"))
  :config
  ;; to add support for disabling the minor mode spell-fu in major modes do
  ;; (see https://stackoverflow.com/questions/6837511/automatically-disable-a-global-minor-mode-for-a-specific-major-mode)
  ;; (define-global-minor-mode thi::spell-fu-mode spell-fu-mode
  ;;   (lambda ()
  ;;     (when (and (not (memq major-mode
  ;;                           (list
  ;;                            'minibuffer-mode
  ;;                            'vterm-mode
  ;;                            'gerrit-dashboard-mode
  ;;                            'magit-status-mode
  ;;                            'magit-section-mode
  ;;                            'dired-mode
  ;;                            'deadgrep-mode
  ;;                            'pdf-occur-buffer-mode
  ;;                            'minibuffer-inactive-move)))
  ;;                (not (window-minibuffer-p)))
  ;;       (spell-fu-mode))))

  ;; (thi::spell-fu-mode 1)
  )

(use-package sphinx-doc
  ;; TODO this package contains support for generating docstrings in a sphinx
  ;; parsable format for python functions/methods/classes

  ;; TODO does not yet support numpy style docstrings  https://github.com/naiquevin/sphinx-doc.el/issues/19  (but numpydoc.el supports it)
  ;; TODO is there a yasnippet for this already? (yes there are  https://github.com/AndreaCrotti/yasnippet-snippets/search?q=numpy&unscoped_q=numpy
  ;;            snippets: idn, fdn, mdn,
  ;;       TODO they do not support type annotations
  ;;       TODO sync not supported?
  :ensure t


  :config
  (progn

    ;; TODO get region of current signature automatically
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

;; (use-package nano-theme
;;   :enusre t)

(use-package numpydoc
  :ensure t
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))

(use-package ssh-config-mode :ensure t)

;; requires semantic-mode to be enabled
(use-package stickyfunc-enhance :ensure t)

(use-package svg-lib :ensure t
  ;; this pkg is on elpa, but I want the bleeding edge version of it
  :quelpa (svg-lib :fetcher github :repo "rougier/svg-lib")
  )

(use-package svg-tag-mode :ensure t)

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ;; inside swiper it is possible to preview search result in separate buffer
         ;; -> C-c C-o
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

(use-package transient
  :ensure t
  :custom
  ;; for gerrit.el
  (transient-history-limit 100))

;; (use-package tree-sitter
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook #'tree-sitter-mode)

;;   ;; Smart f-strings
;;   ;; https://github.com/ubolonton/emacs-tree-sitter/issues/52
;;   (defun fk/python-f-string-ify (&rest _)
;;     ;; Does nothing if major-mode is not python or point is not on a string.
;;     (when-let* ((python-mode-p (eq major-mode 'python-mode))
;;                 (str (tree-sitter-node-at-point 'string))
;;                 (text (ts-node-text str)))
;;       (let ((is-f-string (string-match-p "^[bru]*f+[bru]*\\(\"\\|'\\)" text))
;;             (should-f-string (and (s-contains-p "{" text)
;;                                   (s-contains-p "}" text))))
;;         (if should-f-string
;;             (unless is-f-string
;;               (save-excursion
;;                 (goto-char (ts-node-start-position str))
;;                 (insert "f")))
;;           (when is-f-string
;;             (save-excursion
;;               (goto-char (ts-node-start-position str))
;;               (when (char-equal (char-after) ?f)
;;                 (delete-char 1))))))))

;;   ;; When not using wrap-region or anything that changes "{" keybinding
;;   ;; (define-key python-mode-map (kbd "{") (lambda ()
;;   ;;                                         (interactive)
;;   ;;                                         (call-interactively 'self-insert-command)
;;   ;;                                         (fk/python-f-string-ify)))

;;   (defadvice wrap-region-trigger (after smart-f-string activate)
;;     (fk/python-f-string-ify))
;;   (defadvice delete-char (after smart-f-string activate)
;;     (fk/python-f-string-ify))
;;   (defadvice delete-active-region (after smart-f-string activate)
;;     (fk/python-f-string-ify))

;;   )

;; (use-package tree-sitter-langs
;;   :ensure t)

(use-package undo-tree
  ;; C-/ undo (without the undo tree graph) !!!
  ;; M-_ redo (without the undo tree graph) !!!
  :ensure t
  :custom
  (undo-tree-visualizer-mode t)
  ;; don't litter the workspace with ~undo-tree~ files
  (undo-tree-auto-save-history nil)
  :diminish
  :config
  (global-undo-tree-mode))

;; (add-hook 'org-mode-hook
;;             (lambda ()
;;               (mapc
;;                (lambda (face)
;;                  (set-face-attribute face nil :inherit 'fixed-pitch))
;;                (list 'org-code
;;                      'org-link
;;                      'org-block
;;                      'org-table
;;                      'org-block-begin-line
;;                      'org-block-end-line
;;                      'org-meta-line
;;                      'org-document-info-keyword))))

(use-package visual-fill-column :ensure t)

(use-package visual-regexp :ensure t
  ;; this package was mentioned in prod's talk about regexp in emacs:
  ;; https://protesilaos.com/codelog/2020-01-23-emacs-regexp-primer/
  :config
  (define-key global-map (kbd "C-c r") 'vr/replace)
  (define-key global-map (kbd "C-c q") 'vr/query-replace)
  ;; if you use multiple-cursors, this is for you:
  (define-key global-map (kbd "C-c m") 'vr/mc-mark)
  )

(use-package vterm
  :ensure t)

(use-package vterm-toggle
  :ensure t
  :config
  (global-set-key [f2] #'vterm-toggle)
  (global-set-key [C-f2] #'vterm-toggle-cd))

(use-package wgrep-ag :ensure t)

(use-package which-key :ensure t
  :config
    (setq which-key-paging-key "<f5>")
  )

(use-package winum
  :ensure t
  :init
  (setq winum-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-`") 'winum-select-window-by-number)
      (define-key map (kbd "C-²") 'winum-select-window-by-number)
      (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
      (define-key map (kbd "M-1") 'winum-select-window-1)
      (define-key map (kbd "M-2") 'winum-select-window-2)
      (define-key map (kbd "M-3") 'winum-select-window-3)
      (define-key map (kbd "M-4") 'winum-select-window-4)
      (define-key map (kbd "M-5") 'winum-select-window-5)
      (define-key map (kbd "M-6") 'winum-select-window-6)
      (define-key map (kbd "M-7") 'winum-select-window-7)
      (define-key map (kbd "M-8") 'winum-select-window-8)
      map))
  :config
  (progn
    ;; (custom-set-faces '(window-numbering-face
    ;;                     ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)))))
    (winum-mode))
  )

(use-package yaml-mode :ensure t :defer t)

(use-package yasnippet
  ;; what do I expect from this config?
  ;; pdb [TAB] in python buffers expands
  ;; ifm [TAB] in python buffers expands
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


;;; Code formatting

;; I want to run yapf, isort and unify (unify can also be called as part of
;; the pre-commit hook) ideally using a single elisp pkg in my python mode
;; buffers in my sandboxes.  Sometimes I also want to use it in python
;; buffers outside the sandbox.
;;
;; Currently I use py-isort and yapfify
;;
;; TODO pick either format-all or reformatter.el from spurcell
;; there is also: https://github.com/raxod502/apheleia
;; https://github.com/raxod502/apheleia/issues/31#issuecomment-813065581

(use-package apheleia
    :quelpa (apheleia :fetcher github :repo "raxod502/apheleia"))

(use-package reformatter
  :ensure t
  )

(use-package format-all
  :ensure t
  ;; this package should soon support chaining of multiple formatters
  ;; e.g. isort, yapf
  )

(use-package py-isort :ensure t)

(use-package yapfify :ensure t :defer t
  :custom
  (yapfify-executable (expand-file-name "~/miniconda3/bin/yapf"))
  )

;;; hot cool new stuff

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :config

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))


;;; Other

(load "thi-defuns")
(load "thi-global")
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


;; horizontal scrolling (useful e.g. in pdf-view-mode)
(setq mouse-wheel-flip-direction t) ;; fix flipped horizontal scrolling direction
(setq mouse-wheel-tilt-scroll t)    ;; enable left-right scroll from trackpad


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

;;; init.el ends here
(put 'downcase-region 'disabled nil)
(setq org-image-actual-width nil)
(put 'dired-find-alternate-file 'disabled nil)
