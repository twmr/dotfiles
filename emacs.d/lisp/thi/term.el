;; fixes non working tab-key in ansi-term
;; see http://stackoverflow.com/questions/18278310/emacs-ansi-term-not-tab-completing
(add-hook 'term-mode-hook
          (lambda() (progn
                      (setq yas-dont-activate t)
                      (setq global-hl-line-mode nil) ;; disable hl-line-mode
                      )))
