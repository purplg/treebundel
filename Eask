(source "gnu")
(source "melpa")

(package "treebundel"
         "0.1.0"
         "Bundle related git-worktrees together")

(website-url "https://github.com/purplg/treebundel")
;; (keywords "")

(package-file "treebundel.el")

(depends-on "emacs" "27.1")
(depends-on "compat" "29.1.4.2")

(development
 (depends-on "buttercup")
 (script "test" "eask exec buttercup -L . --traceback pretty"))
