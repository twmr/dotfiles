;:::::::::::::::::::::::::::::::::::::::::::::::
;; Vi Mode : Viper and Vimpulse

(setq vimpulse-want-vi-keys-in-apropos nil)
(setq vimpulse-want-vi-keys-in-buffmenu nil)
(setq vimpulse-want-vi-keys-in-dired nil)
(setq vimpulse-want-vi-keys-in-help nil)
(setq vimpulse-want-vi-keys-in-Info nil)
(setq vimpulse-want-change-undo nil)


(setq-default viper-auto-indent t)

(setq viper-change-notification-threshold 0
      viper-expert-level 5
      viper-inhibit-startup-message t
      viper-vi-style-in-minibuffer nil
      viper-want-ctl-h-help t
      viper-want-emacs-keys-in-insert t)

(setq-default viper-ex-style-editing nil)
(setq-default viper-ex-style-motion nil)
(setq-default viper-delete-backwards-in-replace t)

;;;; If you don't have this line, C-d will not delete in insert state,
;;;; which can be confusing...  The default binding is to back tab.
;;;; If you really like the back tab function, either re-bind it or
;;;; give up the C-d deletion in insert state...arguably you should be
;;;; using 'x' in vi state to do deletion anyway.  Personally I delete
;;;; every third character I type, reflexively, and so this is
;;;; non-optional.
(define-key viper-insert-global-user-map "\C-d" 'delete-char)

;;;; Viper maps C-w in insert state to delete back one word.  If you
;;;; are coming from emacs, you'll be expecting this to kill the
;;;; region into the ring.  The next line adjusts that.
(define-key viper-insert-global-user-map "\C-w" 'kill-region)

;;;; redefine (equiv. of the famous `map Y y$')
(defun viper-yank-line (arg)
  "Delete to the end of line."
  (interactive "P")
  (viper-goto-eol (cons arg ?y)))

;;;; Change the background color of the mode-line, according to the
;;;; current viper input state.
;;;; from http://retroj.net/git/dot-emacs/viper.el
(defvar jjf-viper-normal-mode-line-background (face-background 'mode-line))
(defvar jjf-viper-normal-mode-line-foreground (face-foreground 'mode-line))
(defun jjf-viper-set-mode-line-color (&rest after-which-mode)
  (set-face-background
   'mode-line
   (cond
    ((eq viper-current-state 'replace-state)
     "cornflower blue")
    ((eq viper-current-state 'emacs-state)
     jjf-viper-normal-mode-line-background)
    ((eq viper-current-state 'insert-state)
     "cornflower blue")
    ((eq viper-current-state 'vi-state)
     "DarkGoldenrod1")))
  (set-face-foreground
   'mode-line
   (cond
    ((eq viper-current-state 'replace-state)
     "black")
    ((eq viper-current-state 'emacs-state)
     jjf-viper-normal-mode-line-foreground)
    ((eq viper-current-state 'insert-state)
     "black")
    ((eq viper-current-state 'vi-state)
     "black"))))

(eval-after-load 'viper
  `(progn
     (add-hook 'viper-vi-state-hook 'jjf-viper-set-mode-line-color)
     (add-hook 'viper-insert-state-hook 'jjf-viper-set-mode-line-color)
     (add-hook 'viper-replace-state-hook 'jjf-viper-set-mode-line-color)
     (add-hook 'viper-emacs-state-hook 'jjf-viper-set-mode-line-color)
     (add-hook 'window-configuration-change-hook 'jjf-viper-set-mode-line-color)))

(remove-hook 'minibuffer-setup-hook 'viper-minibuffer-setup-sentinel)
;; post-ddfcd16209c575 Vimpulse makes also these two necessary:
(defadvice viper-set-minibuffer-overlay (around fuck-viper activate)
 nil)
(defadvice viper-has-face-support-p (around fuck-viper activate)
 nil)

;; Vimpulse/Viper bindings

(define-key viper-vi-basic-map "gs" 'magit-status)

;; Don't mess with my Esc key in Vi state, bad, bad Viper!
;; (for example Esc-w doesnt copy lines correctly);
;; but exiting visual mode requires this;
;; also ESC and then S-v or C-v requires this to
;; correctly go to visual mode/state (define-key
;; viper-vi-intercept-map viper-ESC-key nil) ; viper-intercept-ESC-key

;; ;; Don't mess with my input methods, bad, bad Viper!
(defalias 'viper-set-input-method 'ignore)
(ad-deactivate 'activate-input-method)
(ad-deactivate 'inactivate-input-method)
(ad-deactivate 'toggle-input-method)
