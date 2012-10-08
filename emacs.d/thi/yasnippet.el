;;; Snippets make typing fun

(require 'dropdown-list)

(yas/global-mode 1)

(yas/load-directory "~/.emacs.d/el-get/yasnippet/snippets")

(setq yas/prompt-functions '(yas/ido-prompt
                             yas/dropdown-prompt
                             yas/completing-prompt))

;; Replace yasnippets's TAB
;; TODO study this
;; (add-hook 'yas/minor-mode-hook
;;           (lambda () (define-key yas/minor-mode-map
;;                        (kbd "TAB") 'smart-tab))) ; was yas/expand
