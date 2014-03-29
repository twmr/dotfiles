;; fixes non working tab-key in ansi-term
;; see http://stackoverflow.com/questions/18278310/emacs-ansi-term-not-tab-completing
(add-hook 'term-mode-hook
          (lambda() (progn
                      (setq yas-dont-activate t)
                      (setq global-hl-line-mode nil) ;; disable hl-line-mode
                      (goto-address-mode) ;; clickable urls
                      )))

;; force ansi-term to be utf-8 after it launches
(defadvice ansi-term (after advise-ansi-term-coding-system activate)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

;; http://echosa.github.io/blog/2012/06/06/improving-ansi-term/
;; close the terminal buffer automatically on exit
;; DOES NOT WORK ?!??!
;; (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
;;   (if (memq (process-status proc) '(signal exit))
;;       (let ((buffer (process-buffer proc)))
;;         ad-do-it
;;         (kill-buffer buffer))
;;     ad-do-it))
;; (ad-activate 'term-sentinel)
