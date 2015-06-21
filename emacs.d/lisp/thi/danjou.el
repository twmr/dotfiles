;; taken from Julien Danjou
(setq frame-title-format '("" invocation-name ": %b"))
;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
;; (setq visible-bell t)
(setq-default fill-column 76)

(set-default 'indicate-buffer-boundaries '((up . nil) (down . nil) (t . left)))
(setq next-screen-context-lines 5)      ; I want to keep more lines when
                                        ; switching pages
(setq use-dialog-box nil)               ; Seriously…
(put 'narrow-to-region 'disabled nil)
(set-default 'indent-tabs-mode nil)    ; always use spaces to indent, no tab

;; (display-time-mode 1)
(global-hi-lock-mode 1)                 ; highlight stuff
(savehist-mode 1)

(delete-selection-mode 1)               ; Transient mark can delete/replace
(global-hl-line-mode 1)                 ; Highlight the current line
(make-variable-buffer-local 'global-hl-line-mode) ; this makes it possible to disable hl-line mode for certain modes (see http://stackoverflow.com/questions/9990370/how-to-disable-hl-line-feature-in-specified-mode)

;; (windmove-default-keybindings)        ; Move between frames with Shift+arrow
(show-paren-mode t)
;; (add-hook 'minibuffer-setup-hook '(lambda () (set (make-local-variable 'show-paren-mode) nil)))
;; (add-hook 'minibuffer-exit-hook #'show-paren-mode t)
(url-handler-mode 1)                    ; Allow to open URL
(mouse-avoidance-mode 'animate)         ; Move the mouse away
(ffap-bindings)                         ; Use ffap
;; (iswitchb-mode 1)
;; (browse-kill-ring-default-keybindings)
;; (which-func-mode 1)


;; from better-defaults
;; https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(setq x-select-enable-clipboard t ;; emacs default values is already t
      x-select-enable-primary t ;; emacs default is nil
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)
