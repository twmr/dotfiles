(require 'color-theme)
(setq color-theme-load-all-themes nil)
(add-to-list 'load-path "~/.emacs.d/colors")
;;(require 'color-theme-tango-2)
;;(color-theme-tango-2)
;;(require 'color-theme-tangotango)
;;(color-theme-tangotango)
(load-file "~/.emacs.d/colors/color-theme-wombat/color-theme-wombat.el")
(color-theme-wombat)

(require 'diff-mode)
;; Diffing (see diff-mode !!)
;; FIXME use color theme colors!!
(set-face-foreground 'diff-added "green1")
(set-face-foreground 'diff-removed "red3")

(provide 'init-theme)
