;; fixes non working tab-key in ansi-term
;; see http://stackoverflow.com/questions/18278310/emacs-ansi-term-not-tab-completing
(add-hook 'term-mode-hook
          (lambda() (setq yas-dont-activate t)))
