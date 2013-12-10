(defcustom graphene-speedbar-auto t
  "Whether graphene should open sr-speedbar when a project is loaded."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-project-pin-speedbar t
  "Pin the speedbar directory when opening a project."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-speedbar-refresh-hooks '(after-save-hook)
  "List of hooks which on being run will cause speedbar to refresh."
  :type 'sexp
  :group 'graphene)

(load "vendor/graphene-projects")
(when window-system
  (load "vendor/graphene-theme")
  (load-theme 'graphene t))
