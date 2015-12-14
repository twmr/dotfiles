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
        ;; todo use this variable
        ;; (cpersp-name (persp-name persp-curr))
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

;; ses-csv.el -- Read/Write CSV file for SES
;; Author: Takashi Hattori (hattori@sfc.keio.ac.jp)
;; Requires: Ruby

(defun ses-read-from-csv-file (file)
  "Insert the contents of a CSV file named FILE into the current position."
  (interactive "fCSV file: ")
  (let ((buf (get-buffer-create "*ses-csv*"))
	text)
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (process-file "ruby" file buf nil "-e" "require 'csv'; CSV::Reader.parse(STDIN) { |x| puts x.join(\"\\t\") }")
      (setq text (buffer-substring (point-min) (point-max))))
    (ses-yank-tsf text nil)))

(defun ses-write-to-csv-file (file)
  "Write the values of the current buffer into a CSV file named FILE."
  (interactive "FCSV file: ")
  (push-mark (point-min) t t)
  (goto-char (- (point-max) 1))
  (ses-set-curcell)
  (ses-write-to-csv-file-region file))

(defun ses-write-to-csv-file-region (file)
  "Write the values of the region into a CSV file named FILE."
  (interactive "FCSV file: ")
  (ses-export-tab nil)
  (let ((buf (get-buffer-create "*ses-csv*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (yank)
      (call-process-region (point-min) (point-max) "ruby" t buf nil "-e" "require 'csv'; w = CSV::Writer.create(STDOUT); STDIN.each { |x| w << x.chomp.split(/\\t/) }")
      (write-region (point-min) (point-max) file))))

(defun replace-in-buffer ()
  (interactive)
  (save-excursion
    (if (equal mark-active nil) (mark-word))
    (setq curr-word (buffer-substring-no-properties (mark) (point)))
    (setq old-string (read-string "OLD string: " curr-word))
    (setq new-string (read-string "NEW string: " old-string))
    (query-replace old-string new-string nil (point-min) (point-max))
    )
  )

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)

;; taken from https://gist.github.com/mooz/890562
;; (defun my-reb-query-replace-regexp ()
;;   "Call `query-replace-regexp' with current regexp of RE-builder"
;;   (interactive)
;;   (reb-update-regexp)
;;   (let ((re (reb-target-binding reb-regexp)))
;;     (flet ((query-replace-read-from
;;             (prompt regexp-flag)
;;             ;; body
;;             re))
;;       (pop-to-buffer reb-target-buffer)
;;       (call-interactively 'query-replace-regexp))))

;; Stolen from Magnar's code to do the same thing for dired
;; http://whattheemacsd.com/setup-dired.el-02.html
(defun ibuffer-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (next-line 3))

(defun ibuffer-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (next-line -2)
  (beginning-of-line))


(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))


(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))


(defun join-line-vim-style ()
  (interactive)
  (save-excursion
    (let ((start (progn
                   (end-of-line)
                   (insert " ")
                   (point-at-eol)))
          (end (progn
                 (forward-line)
                 (back-to-indentation)
                 (point))))
         (delete-region start end))))
