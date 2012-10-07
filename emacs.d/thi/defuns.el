;; For loading libraries from the vendor directory
;; Modified from defunkt's original version to support autoloading.
;; http://github.com/defunkt/emacs/blob/master/defunkt/defuns.el
(defun vendor (library &rest autoload-functions)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/vendor/" file))
         (lnormal (concat normal "/lisp"))
         (suffix (concat normal ".el"))
         (personal (concat "~/.emacs.d/thi/" file))
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
         (normal (concat "~/.emacs.d/elpa/" file "-" vnum))
         (lnormal (concat normal "/lisp"))
         (suffix (concat normal ".el"))
         (personal (concat "~/.emacs.d/thi/" file))
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

;; start hl-line in each buffer where the major modes are note in hl-line-disabled-modes-list
(defun thi-turn-hl-line-mode-on ()
  (interactive)
  (if (not (member major-mode hl-line-disabled-modes-list))
      (hl-line-mode 1)))

(defun thi-turn-hl-line-mode-off ()
  (interactive)
  (if (not (member major-mode hl-line-disabled-modes-list))
      (hl-line-mode -1)))

(defun thi-turn-hl-line-mode-on2 ()
  (interactive)
  (if (member major-mode hl-line-enabled-modes-list)
      (hl-line-mode 1)))

(defun thi-turn-hl-line-mode-off2 ()
  (interactive)
  (if (member major-mode hl-line-enabled-modes-list)
      (hl-line-mode -1)))



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
