(require 'color-theme)
(setq color-theme-load-all-themes nil)

;; add all subdirs of colors-dir to loadpath
(let* ((my-colors-dir "~/.emacs.d/colors/")
       (default-directory my-colors-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-colors-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

(autoload 'color-theme-tango-2 "color-theme-tango-2" nil t)
(autoload 'color-theme-tangotango "color-theme-tangotango" nil t)
(autoload 'color-theme-zenburn "zenburn" nil t)
(autoload 'color-theme-wombat "color-theme-wombat" nil t)
(autoload 'color-theme-twilight "color-theme-twilight" nil t)

(color-theme-wombat)

(require 'diff-mode)
;; Diffing (see diff-mode !!)
;; FIXME use color theme colors!!
(set-face-foreground 'diff-added "green1")
(set-face-foreground 'diff-removed "red3")

(provide 'init-theme)
