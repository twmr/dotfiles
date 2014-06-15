(define-key notmuch-show-mode-map "d" 
  (lambda () 
    (interactive) 
    (notmuch-show-tag-message "+deleted")))

(setq-default notmuch-search-oldest-first nil)
