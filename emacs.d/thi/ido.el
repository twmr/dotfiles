;;(require 'ido)
(ido-mode 'both) ;; for buffers and files
(setq
  ido-save-directory-list-file (concat thi::cache-file-dir "/ido.last")

  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"

     "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
  ido-case-fold  t                 ; be case-insensitive

  ;; Display ido results vertically, rather than horizontally
  ;; see http://www.emacswiki.org/emacs/InteractivelyDoThings#toc17
  ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
                          " [No match]" " [Matched]" " [Not readable]"
                          " [Too big]" " [Confirm]"))
  ;; the following two variables where commented out because
  ;; they are not useful in for diplaying completions vertically
  ido-max-prospects 50              ; don't spam my minibuffer
  ;;ido-max-window-height 2

  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-directory-size 100000 ; should be enough
  ido-max-work-directory-list 50   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)

  ido-enable-flex-matching t     ; don't try to be too smart
  ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

(defun ido-disable-line-trucation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(defun ido-my-edit-input () "bla" (interactive)
  (setq ido-current-directory
        (concat (abbreviate-file-name ido-current-directory) ido-text ))
  (setq ido-text "")
  (ido-edit-input))

(defun ido-my-keys ()
  "Add my keybindings for ido."
  (when (eq ido-cur-item 'file)
    (define-key ido-completion-map (kbd "C-e") 'ido-my-edit-input)
    ;;(define-key ido-completion-map (kbd "<backspace>") 'ido-my-edit-input)
    )
  (define-key ido-completion-map "\t" 'ido-next-match)
  (define-key ido-completion-map (kbd "<backtab>") 'ido-complete) ;; use \C-g to exit minibuffer
  )
(add-hook 'ido-setup-hook 'ido-my-keys)

;; sort ido filelist by mtime instead of alphabetically
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (time-less-p
                 (sixth
                  (file-attributes (concat ido-current-directory b)))
                 (sixth
                  (file-attributes (concat ido-current-directory a)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (and (char-equal
                                (string-to-char x) ?.) x))
              ido-temp-list))))
