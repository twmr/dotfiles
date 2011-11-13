;;; screen.el --- define function key sequences and standard colors for screen

;; Author: Michael Raitza
;; Keywords: terminals

;;; Commentary:

;;; screen (and tmux) itself do not specify own colouring settings because they
;;; always run on-top of another terminal (emulator). Thus we cannot make any
;;; assumption about default colours. We relay this to the other terminal's
;;; initialisation functions. We inspect the COLORTERM environment variable
;;; for another terminal that is supported by emacs and actually specifies a
;;; color mapping on its own. So this only works if the COLORTERM
;;; environment variable is set appropriately. 
;;;
;;; In short, for maximum performance set:
;;;
;;; TERM=screen-256color COLORTERM={rxvt,xterm}

;;; Code:

(defvar screen-function-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\e[A" [up])
    (define-key map "\e[B" [down])
    (define-key map "\e[C" [right])
    (define-key map "\e[D" [left])
    (define-key map "\e[2~" [insert])
    (define-key map "\e[3~" [delete])
    (define-key map "\e[4~" [end]) ; everybody else seems to bind this to [select]. why?
    (define-key map "\e[1~" [home])
    (define-key map "\e[5~" [prior])
    (define-key map "\e[6~" [next])
    (define-key map "\eOP" [f1])
    (define-key map "\eOQ" [f2])
    (define-key map "\eOR" [f3])
    (define-key map "\eOS" [f4])
    (define-key map "\e[15~" [f5])
    (define-key map "\e[17~" [f6])
    (define-key map "\e[18~" [f7])
    (define-key map "\e[19~" [f8])
    (define-key map "\e[20~" [f9])
    (define-key map "\e[21~" [f10])
    map)
  "Function key overrides for screen. They should correspond to vt100's.")

(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  ;; screen runs inside another terminal emulator maybe providing
  ;; coloured output, but some of the keybindings might be incompatible
  ;; with screen's. (cf. xterm.el)
  ;; So try to load whatever terminal is providing the COLORTERM environment
  ;; variable.
  (if (getenv "COLORTERM" (selected-frame))
      (tty-run-terminal-initialization (selected-frame) (getenv "COLORTERM" (selected-frame))))

  (let ((m (copy-keymap screen-function-map)))
    (set-keymap-parent m (keymap-parent input-decode-map))
    (set-keymap-parent input-decode-map m))
  
  (tty-set-up-initial-frame-faces))

;;; screen.el ends here