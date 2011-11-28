;;; wombat-theme.el --- Custom face theme for Emacs  -*-coding: utf-8 -*-

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Kristoffer Grönlund <krig@koru.se>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme wombat
  "Medium-contrast faces with a dark gray background.
Adapted, with permission, from a Vim color scheme by Lars H. Nielsen.
Basic, Font Lock, Isearch, Gnus, Message, and Ansi-Color faces
are included.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'wombat
   `(default ((,class (:background "#242424" :foreground "#f6f3e8"))))
   `(cursor ((,class (:background "#64a8d8"))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#303030"))))
   `(highlight ((,class (:background "#454545" :foreground "#ffffff"
			 :underline t))))
   `(region ((,class (:background "#444444" :foreground "#f6f3e8"))))
   `(secondary-selection ((,class (:background "#333366" :foreground "#f6f3e8"))))
   `(isearch ((,class (:background "#343434" :foreground "#857b6f"))))
   `(lazy-highlight ((,class (:background "#384048" :foreground "#a0a8b0"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "#444444" :foreground "#f6f3e8"))))
   `(mode-line-inactive ((,class (:background "#444444" :foreground "#857b6f"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground "#e5786d"))))
   `(escape-glyph ((,class (:foreground "#ddaa6f" :weight bold))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#e5786d"))))
   `(font-lock-comment-face ((,class (:foreground "#99968b"))))
   `(font-lock-constant-face ((,class (:foreground "#e5786d"))))
   `(font-lock-function-name-face ((,class (:foreground "#cae682"))))
   `(font-lock-keyword-face ((,class (:foreground "#8ac6f2" :weight bold))))
   `(font-lock-string-face ((,class (:foreground "#95e454"))))
   `(font-lock-type-face ((,class (:foreground "#92a65e" :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground "#cae682"))))
   `(font-lock-warning-face ((,class (:foreground "#ccaa8f"))))
   ;; Button and link faces
   `(link ((,class (:foreground "#8ac6f2" :underline t))))
   `(link-visited ((,class (:foreground "#e5786d" :underline t))))
   `(button ((,class (:background "#333333" :foreground "#f6f3e8"))))
   `(header-line ((,class (:background "#303030" :foreground "#e7f6da"))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-news-1-low ((,class (:foreground "#95e454"))))
   `(gnus-group-news-2 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-2-low ((,class (:foreground "#cae682"))))
   `(gnus-group-news-3 ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-news-3-low ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-news-4 ((,class (:weight bold :foreground "#99968b"))))
   `(gnus-group-news-4-low ((,class (:foreground "#99968b"))))
   `(gnus-group-news-5 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-5-low ((,class (:foreground "#cae682"))))
   `(gnus-group-news-low ((,class (:foreground "#99968b"))))
   `(gnus-group-mail-1 ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-mail-1-low ((,class (:foreground "#95e454"))))
   `(gnus-group-mail-2 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-mail-2-low ((,class (:foreground "#cae682"))))
   `(gnus-group-mail-3 ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-mail-3-low ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-mail-low ((,class (:foreground "#99968b"))))
   `(gnus-header-content ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-from ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-header-subject ((,class (:foreground "#cae682"))))
   `(gnus-header-name ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-newsgroups ((,class (:foreground "#cae682"))))
   ;; Message faces
     `(magit-header ((,class (:foreground "#edd400" :inherit variable-pitch))))
     `(magit-diff-add ((,class (:foreground "#729fcf"))))
     `(magit-item-highlight ((,class (:inverse-video nil))))
     `(diff-header ((,class (:background "gray30" :inherit variable-pitch))))
     `(diff-index ((,class (:foreground "#edd400" :inherit variable-pitch))))
     `(diff-file-header ((,class (:foreground "#eeeeec" :height 1.1 :inherit variable-pitch))))
     `(diff-hunk-header ((,class (:foreground "#edd400" :inherit variable-pitch))))
     `(diff-added ((,class (:foreground "#8ae234"))))
     `(diff-removed ((,class (:foreground "#f57900"))))
     `(diff-context ((,class (:foreground "#888a85"))))
     `(diff-refine-change ((,class (:bold t :background "gray30"))))
     `(ediff-current-diff-A ((,class (:background "#555753"))))
     `(ediff-current-diff-Ancestor ((,class (:background "#555753"))))
     `(ediff-current-diff-B ((,class (:background "#555753"))))
     `(ediff-current-diff-C ((,class (:background "#555753"))))
     `(ediff-even-diff-A ((,class (:background "gray30"))))
     `(ediff-even-diff-Ancestor ((,class (:background "gray30"))))
     `(ediff-even-diff-B ((,class (:background "gray30"))))
     `(ediff-even-diff-C ((,class (:background "gray30"))))
     `(ediff-odd-diff-A ((,class (:background "gray30"))))
     `(ediff-odd-diff-Ancestor ((,class (:background "gray30"))))
     `(ediff-odd-diff-B ((,class (:background "gray30"))))
     `(ediff-odd-diff-C ((,class (:background "gray30"))))
     `(ediff-fine-diff-A ((,class (:background "#222222"))))
     `(ediff-fine-diff-Ancestor ((,class (:background "#222222"))))
     `(ediff-fine-diff-B ((,class (:background "#222222"))))
     `(ediff-fine-diff-C ((,class (:background "#222222"))))
   `(message-header-name ((,class (:foreground "#8ac6f2" :weight bold))))
   `(message-header-cc ((,class (:foreground "#95e454"))))
   `(message-header-other ((,class (:foreground "#95e454"))))
   `(message-header-subject ((,class (:foreground "#cae682"))))
   `(message-header-to ((,class (:foreground "#cae682"))))
   `(message-cited-text ((,class (:foreground "#99968b"))))
   `(message-separator ((,class (:foreground "#e5786d" :weight bold))))))

(custom-theme-set-variables
 'wombat
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682"
			    "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]))

(provide-theme 'wombat)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; wombat-theme.el ends here
