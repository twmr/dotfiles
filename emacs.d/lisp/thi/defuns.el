;; For loading libraries from the vendor directory
;; Modified from defunkt's original version to support autoloading.
;; http://github.com/defunkt/emacs/blob/master/defunkt/defuns.el
(defun vendor (library &rest autoload-functions)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/vendor/" file))
         (lnormal (concat normal "/lisp"))
         (suffix (concat normal ".el"))
         (personal (concat "~/.emacs.d/lisp/thi/" file))
         (found nil))
    (cond
     ((file-directory-p lnormal) (add-to-list 'load-path lnormal) (set 'found t))
     ((file-directory-p normal) (add-to-list 'load-path normal) (set 'found t))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (set 'found t))
     ((file-exists-p suffix)  (set 'found t)))
    (when (file-exists-p (concat personal "-pre.el"))
      (load (concat personal "-pre")))
    (when found
      (if autoload-functions
          (dolist (autoload-function autoload-functions)
            (autoload autoload-function (symbol-name library) nil t))
        (require library)))
    (when (file-exists-p (concat personal ".el"))
      (load personal))))

(defun elpa-vendor (library vnum &rest autoload-functions)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/el-get/package/elpa/" file "-" vnum))
         (lnormal (concat normal "/lisp"))
         (suffix (concat normal ".el"))
         (personal (concat "~/.emacs.d/lisp/thi/" file))
         (found nil))
    (cond
     ((file-directory-p lnormal) (add-to-list 'load-path lnormal) (set 'found t))
     ((file-directory-p normal) (add-to-list 'load-path normal) (set 'found t))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (set 'found t))
     ((file-exists-p suffix)  (set 'found t)))
    (when (file-exists-p (concat personal "-pre.el"))
      (load (concat personal "-pre")))
    (when found
      (if autoload-functions
          (dolist (autoload-function autoload-functions)
            (autoload autoload-function (symbol-name library) nil t))
        (require library)))
    (when (file-exists-p (concat personal ".el"))
      (load personal))))

(defun turn-on-outline-minor-mode ()
  (outline-minor-mode 1))

(defun turn-on-visual-line-mode ()
  (visual-line-mode 1))

(defun turn-on-longlines-mode ()
  (longlines-mode 1))

(defun my-count-words-region (posBegin posEnd)
  "Print number of words and chars in region."
  (interactive "r")
  (message "Counting \u2026")
  (save-excursion
    (let (wordCount charCount)
      (setq wordCount 0)
      (setq charCount (- posEnd posBegin))
      (goto-char posBegin)
      (while (and (< (point) posEnd)
                  (re-search-forward "\\w+\\W*" posEnd t))
        (setq wordCount (1+ wordCount)))

      (message "Words: %d. Chars: %d." wordCount charCount)
      )))

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))


(defun open-file-at-cursor ()
    "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path is starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
This command is similar to `find-file-at-point' but without prompting for confirmation.
"
    (interactive)
    (let ( (path (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (thing-at-point 'filename) ) ))
      (if (string-match-p "\\`https?://" path)
          (browse-url path)
        (progn ; not starting “http://”
          (if (file-exists-p path)
              (find-file path)
            (if (file-exists-p (concat path ".el"))
                (find-file (concat path ".el"))
              (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" path) )
                (find-file path )) ) ) ) ) ))

;; from https://gist.github.com/sabof/5383987
(defun wc/narrow-window ()
  (let (( new-right
          (max 0 (+ (or (cdr (window-margins)) 0)
                    (- (window-body-width) fill-column)))))
    (set-window-margins nil (car (window-margins)) new-right)
    (set-window-fringes nil (car (window-fringes)) 2)))

(define-minor-mode wrap-column-mode
    "Wrap the text at `fill-column'.
Works by adjusting the right margin."
  nil nil nil
  (if wrap-column-mode
      (progn
        (visual-line-mode 1)
        (add-hook 'window-configuration-change-hook
                  'wc/narrow-window nil t)
        (wc/narrow-window))
      (progn
        (remove-hook 'window-configuration-change-hook
                     'wc/narrow-window t)
        (set-window-margins nil (car (window-margins)) nil)
        (set-window-fringes nil (car (window-fringes))
                                (car (window-fringes))))))

;; see http://www.enigmacurry.com/category/emacs/2/
(require 'term)
;; (defun visit-ansi-term ()
;;   "If the current buffer is:
;;      1) a running ansi-term named *ansi-term*, rename it.
;;      2) a stopped ansi-term, kill it and create a new one.
;;      3) a non ansi-term, go to an already running ansi-term
;;         or start a new one while killing a defunt one"
;;   (interactive)
;;   (let ((is-term (string= "term-mode" major-mode))
;;         (is-running (term-check-proc (buffer-name)))
;;         (term-cmd "/bin/zsh")
;;         (anon-term (get-buffer "*ansi-term*")))
;;     (if is-term
;;         (if is-running
;;             (if (string= "*ansi-term*" (buffer-name))
;;                 (call-interactively 'rename-buffer)
;;               (if anon-term
;;                   (switch-to-buffer "*ansi-term*")
;;                 (ansi-term term-cmd)))
;;           (kill-buffer (buffer-name))
;;           (ansi-term term-cmd))
;;       (if anon-term
;;           (if (term-check-proc "*ansi-term*")
;;               (switch-to-buffer "*ansi-term*")
;;             (kill-buffer "*ansi-term*")
;;             (ansi-term term-cmd))
;;         (ansi-term term-cmd)))))

;; more advanced visit-ansi term from one of the comment of the above blog post
(defun visit-ansi-term (arg)
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one
     4) on a named term (i.e., not *ansi-term*) create a new one
     4) if called with CTRL-u create a new ansi-term regardless
     5) C-u C-u create a new ansi-term and prompt for name
   Within an existing ansi-term one need to use C-x C-u F2 for a new term"
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (list nil))
    ((equal current-prefix-arg '(4))
     (list "*ansi-term*"))
    ((equal current-prefix-arg '(16))
     (list (read-string "Name (*ansi-term*):" nil nil "*ansi-term*")))
    ))
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/zsh")
        (anon-term (get-buffer "*ansi-term*")))
    (cond
     ((string= arg nil)
      (if is-term
          (if is-running
              (if (string= "*ansi-term*" (buffer-name))
                  (call-interactively 'rename-buffer)
                (if anon-term
                    (switch-to-buffer "*ansi-term*")
                  (ansi-term term-cmd)))
            (kill-buffer (buffer-name))
            (ansi-term term-cmd))
        (if anon-term
            (if (term-check-proc "*ansi-term*")
                (switch-to-buffer "*ansi-term*")
              (kill-buffer "*ansi-term*")
              (ansi-term term-cmd))
          (ansi-term term-cmd))))
     ((string= arg "*ansi-term*")
      (ansi-term term-cmd))
     (t
      (ansi-term term-cmd arg)))))
