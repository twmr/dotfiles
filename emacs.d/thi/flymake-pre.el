(defun flymake-display-warning (warning)
  "Display a warning to the user, using message"
  (message warning))

;; Nope, I want my copies in the system temp dir.
(setq flymake-run-in-place nil)
;; This lets me say where my temp dir is.
(setq temporary-file-directory "~/.emacs.d/tmp/")

;; I want to see at most the first 4 errors for a line.
(setq flymake-number-of-errors-to-display 4)
