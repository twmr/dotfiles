(require 'auto-complete-config)
(global-auto-complete-mode t)
;;(setq ac-auto-start nil)
(setq ac-dwim t)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-complete-mode-map (kbd "TAB") 'ac-expand)

;; Autocomplete defaults
;; ESC to get out of autocomplete menu
(ac-config-default)
(define-key ac-completing-map (kbd "ESC") 'ac-stop)
(setq ac-auto-show-menu 0.2
      ac-auto-start 3
      ac-quick-help-delay 2.0
      ac-ignore-case nil
      ac-candidate-menu-min 2
      ac-use-quick-help nil
      ac-limit 10)

;;----------------------------------------------------------------------------
;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
;;----------------------------------------------------------------------------
(setq tab-always-indent 'complete)  ;; use 'complete when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)

;; hook AC into completion-at-point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)


(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(dolist (mode '(awk-mode magit-log-edit-mode log-edit-mode org-mode
                text-mode haml-mode sass-mode yaml-mode csv-mode
                espresso-mode haskell-mode html-mode matlab-mode
                nxml-mode sh-mode Cojure-mode
                lisp-mode latex-mode textile-mode markdown-mode
                tuareg-mode))
  (add-to-list 'ac-modes mode))

;; Exclude very large buffers from dabbrev
(defun smp-dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'smp-dabbrev-friend-buffer)
