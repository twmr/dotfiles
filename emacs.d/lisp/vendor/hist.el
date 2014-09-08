;; see https://mail.google.com/mail/u/0/#search/label%3Amyemacs-emacs-devel+replace-regexp/147b7489206c3deb
(defvar histpairs-history nil)

(defvar histpairs-history-current nil)


(defun histpairs-query-replace-p ()
  (string-match "Query replace\\( regexp\\)?\\( (\\|:\\)" (minibuffer-prompt)))


(defun histpairs-update-history-if-necessary ()
  (unless (eq (cdr histpairs-history) command-history)
    (setq histpairs-history
          (cons
           (delete-if 'null
                      (mapcar
                       (lambda (item)
                         (if (or (eq (car item) 'query-replace)
                                 (eq (car item) 'query-replace-regexp))
                             (cons (second item) (third item))))
                       command-history))
           command-history))))


(defun histpairs-update-minibuffer ()
  (delete-minibuffer-contents)
  (insert (caar histpairs-history-current)
          (propertize " -> " 'face 'highlight
                      'intangible t 'rear-nonsticky t)

          (if (stringp (cdar histpairs-history-current))
              (cdar histpairs-history-current)
            (concat "can't handle this history element yet, "
                    "probably a regexp lisp expression"))))


(defun histpairs-previous-history-element (n)
  (interactive "p")
  (if (not (histpairs-query-replace-p))
      (previous-history-element n)

    (histpairs-update-history-if-necessary)

    (if (not histpairs-history-current)
        (setq histpairs-history-current (car histpairs-history))

      (setq histpairs-history-current (cdr histpairs-history-current)))


    (if (not histpairs-history-current)
        (message "No previous element.")

      (histpairs-update-minibuffer))))


(defun histpairs-next-history-element (n)
  (interactive "p")
  (if (not (histpairs-query-replace-p))
      (next-history-element n)

    (histpairs-update-history-if-necessary)

    (if histpairs-history-current
        (let ((items (car histpairs-history)))
          (while (and items
                      (not (eq (cdr items) histpairs-history-current)))
            (setq items (cdr items)))
          (setq histpairs-history-current items)))

    (if (not histpairs-history-current)
        (message "No next element.")

      (histpairs-update-minibuffer))))


(defun histpairs-finish ()
  (interactive)
  (if (not (histpairs-query-replace-p))
      (exit-minibuffer)

    (setq histpairs-history-current nil)

    (let ((pos (next-single-property-change 0 'intangible
                                            (minibuffer-contents))))
      (if (not pos)
          (exit-minibuffer)  ;; normal execution

        (if (get-text-property 0 'intangible (minibuffer-contents))
            (message "FROM is empty.")

          (let* ((end (next-single-property-change pos 'intangible
                                                   (minibuffer-contents)))
                 (from (substring (minibuffer-contents) 0 pos))
                 (to (if end
                         (substring (minibuffer-contents) end)
                       "")))
            (delete-minibuffer-contents)
            (insert from)

            (push 'return unread-command-events)
            (dolist (c (nreverse (string-to-list to)))
              (push c unread-command-events))

            (exit-minibuffer)))))))



(define-key minibuffer-local-map
  (kbd "<up>") 'histpairs-previous-history-element)
(define-key minibuffer-local-map
  (kbd "<down>") 'histpairs-next-history-element)
(define-key minibuffer-local-map
  (kbd "<return>") 'histpairs-finish)
