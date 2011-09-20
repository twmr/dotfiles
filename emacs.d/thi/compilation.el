;; the compilation buffer will scroll automatically to follow the
;; output as it comes in.
;; - is not a good idea if you want to see and jump to the firs error
;;   so comment this out
;;(setq compilation-scroll-output t)

;; is there some hook to jump to the first error ?  then I suggesst to let the compilation
;; buffer scroll automatically, if something fails jump to the first error and if not kill
;; the buffer

(defun my-compile ()
  "Run compile and resize the compile window"
  (interactive)
  (progn
    (call-interactively 'compile)
    (setq cur (selected-window))
    (setq w (get-buffer-window "*compilation*"))
    (select-window w)
    (setq h (window-height w))
    (shrink-window (- h 20))
    (select-window cur)
    )
  )

(global-set-key [f9] 'my-compile)

;; (defun my-compilation-hook ()
;;   "Make sure that the compile window is splitting vertically"
;;   (progn
;;     (if (not (get-buffer-window "*compilation*"))
;;         (progn
;;           (split-window-horizontally)
;;           )
;;       )
;;     ;; (tabbar-mode 0)
;;     )
;;   )
;; (add-hook 'compilation-mode-hook 'my-compilation-hook)


;; Helper for compilation. Close the compilation window if
;; there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)
