(setq evil-leader/leader "," evil-leader/in-all-states t)
(global-evil-leader-mode)

;; see https://raw.github.com/redguardtoo/emacs.d/master/init-evil.el
(evil-leader/set-key
  "eb" 'eval-buffer
  "ee" 'eval-expression
  "cx" 'copy-to-x-clipboard
  "bb" 'evil-scroll-page-up
  "ff" 'evil-scroll-page-down
  "0" 'select-window-0
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "4" 'select-window-4
  "5" 'select-window-5
  "6" 'select-window-6
  "7" 'select-window-7
  "8" 'select-window-8
  "9" 'select-window-9
  "oo" 'new-frame
  "qq" 'other-frame
  )
