;;; flycheck.el --- On-the-fly syntax checking -*- lexical-binding: t; -*-

(require 'flycheck)

(flycheck-define-checker python-pylint
      "A Python syntax and style checker using Pylint.

This syntax checker requires Pylint 1.0 or newer.

See URL `https://www.pylint.org/'."
  ;; --reports=n disables the scoring report.
  ;; Not calling pylint directly makes it easier to switch between different
  ;; Python versions; see https://github.com/flycheck/flycheck/issues/1055.
  :command ("python"
            (eval (flycheck-python-module-args 'python-pylint "pylint"))
            "--reports=n"
            "--output-format=text"
            (eval (if flycheck-pylint-use-symbolic-id
                      "--msg-template={path}:{line}:{column}:{C}:{symbol}:{msg}"
                    "--msg-template={path}:{line}:{column}:{C}:{msg_id}:{msg}"))
            (config-file "--rcfile=" flycheck-pylintrc concat)
            "--from-stdin" source-original)
  :standard-input t
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":"
          (or "E" "F") ":"
          (id (one-or-more (not (any ":")))) ":"
          (message) line-end)
   (warning line-start (file-name) ":" line ":" column ":"
            (or "W" "R") ":"
            (id (one-or-more (not (any ":")))) ":"
            (message) line-end)
   (info line-start (file-name) ":" line ":" column ":"
         (or "C" "I") ":"
         (id (one-or-more (not (any ":")))) ":"
         (message) line-end))
  :enabled (lambda ()
             (or (not (flycheck-python-needs-module-p 'python-pylint))
                 (flycheck-python-find-module 'python-pylint "pylint")))
  :verify (lambda (_) (flycheck-python-verify-module 'python-pylint "pylint"))
  :modes python-mode)
