;;; treebundel.el --- Bundle related git-worktrees together -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1") (compat "29.1.4.2"))
;; Version: 0.2.0
;; Author: Ben Whitley
;; Homepage: https://github.com/purplg/treebundel
;; Keywords: convenience vc
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This package is used for bundling related git-worktrees from multiple
;; repositories together.  This helps switch quickly between repositories and
;; ensure you're on the correct branch.  When you're done with your changes, you
;; can use the repositories in the workspace and know which ones were modified
;; to simplify the process of getting the changes merged in together.
;;
;; Additionally, git metadata (the =.git= directory) is shared between all
;; projects.  You can stash, pop, and pull changes in from the same repository in
;; other workspaces thanks to the power of git-worktrees.


;;;; Terminology:

;; Bare
;;   A bare repository used as a source to create a 'PROJECT's git-worktree.
;;
;; Project
;;   A git-worktree checked out from a 'BARE' stored in a 'WORKSPACE'.
;;
;; Workspace
;;   A collection of 'PROJECT's created from 'BARE's.


;;;; Structure:

;; The workspaces directory is structured as such:
;;
;; `treebundel-workspace-root' (default: "~/workspaces/")
;;    |
;;    L workspace1
;;    |    L project-one   (branch: "feature/workspace1")
;;    |    L project-two   (branch: "feature/workspace1")
;;    |    L project-three (branch: "feature/workspace1")
;;    |
;;    L workspace2
;;         L project-one   (branch: "feature/workspace2")
;;         L project-two   (branch: "feature/workspace2")
;;         L project-three (branch: "feature/workspace2")


;;;; Quick start:

;; Assuming default configuration, the following will create a bare clone of the
;; provided repo URL to '~/workspaces/.bare/<repo-name>.git', then create and
;; open a worktree for a new branch called 'feature/<workspace>'.
;;
;; 1. Interactively call `treebundel-add-project'.
;; 2. Enter name for the new (or existing) workspace.
;; 3. Select '[ clone ]'.
;; 4. Enter the URL to clone for the repository to be added to the workspace.


;;;; Configuration:

;; `treebundel-branch-prefix'
;;  Default: 'feature/'
;;
;; This is probably the most subjective variable you'd want to customize.  With
;; its default value, when you add a project to a workspace named, for example,
;; 'new-protocol', the new project will be checked out to a new branch called
;; 'feature/new-protocol'.

;; `treebundel-workspace-root'
;;  Default: '~/workspaces/'
;;
;; This one is also very subjective.  It's where all of your workspaces will
;; exist on your file-system.

;; `treebundel-project-open-function'
;;  Default: `project-switch-project'
;;
;; This is the function called when a project is opened.  You could also just
;; make this `find-file' to just open the file instantly or any other function
;; that takes a file path.

;; `treebundel-before-workspace-open-functions'
;; `treebundel-before-project-open-functions'
;; `treebundel-after-project-open-hook'
;; `treebundel-after-workspace-open-hook'
;;
;; These hooks are called before or after a project or workspace is
;; opened.  `treebundel-before-workspace-open-functions' receives the
;; name of the workspace to be opened as a single argument and
;; `treebundel-before-project-open-functions' receives the workspace
;; and project name opened.

;;;; Usage:
;;
;; The following functions are the commands you should use (and
;; probably bind) to make use of this package.
;;
;; `treebundel-open'
;;   Open a project in a workspace.
;;
;; `treebundel-open-project'
;;   Open other project within current workspace.
;;
;; `treebundel-add-project'
;;   Add a project to a workspace.
;;
;; `treebundel-remove-project'
;;   Remove a project from a workspace.  This will check if the project
;;   has any changes before removing it.
;;
;; `treebundel-delete-workspace'
;;   Delete a workspace.  This will also remove all projects in a
;;   workspace if they don't have any changes.

;;; Code:
(require 'subr-x)
(require 'compat)
(require 'vc-git)


;;;; Customization

(defgroup treebundel nil
  "Exploit git-worktrees to create inter-related project workspaces."
  :group 'convenience
  :prefix "treebundel-")

(defcustom treebundel-branch-prefix "feature/"
  "The string prefix before every new project branch."
  :group 'treebundel
  :type 'string)

(defcustom treebundel-workspace-root "~/workspaces/"
  "The path where all workspaces are stored."
  :group 'treebundel
  :type 'string
  :set (lambda (option value)
         (set option (file-name-as-directory (expand-file-name
                                              value)))))

(defcustom treebundel-bare-dir (file-name-concat treebundel-workspace-root ".bare")
  "The path where bare repositories are stored."
  :group 'treebundel
  :type 'string
  :set (lambda (option value)
         (set option (file-name-as-directory (expand-file-name
                                              value)))))

(defcustom treebundel-project-open-function
  (if (version< emacs-version "28")
      (lambda (dir)
        (find-file (read-file-name
                    "Find file: "
                    (file-name-as-directory dir))))
    'project-switch-project)
  "Function called to switch to a new project."
  :group 'treebundel
  :type 'function)

(defcustom treebundel-fetch-on-add t
  "When t, perform a git-fetch before adding a project to a workspace.
This allows the latest branches on remote to appear when selecting a branch to
checkout.

Set to nil when you don't want to make network requests or just to reduce git
operations when adding projects to your workspaces."
  :group 'treebundel
  :type 'boolean)

;; Hooks
(defcustom treebundel-before-project-open-functions nil
  "Hook which is run before a project is opened.
A single argument is passed which is the path to the project to
be opened."
  :group 'treebundel
  :type '(list function))

(defcustom treebundel-after-project-open-hook nil
  "Hook which is run after a project is opened."
  :group 'treebundel
  :type 'hook)

(defcustom treebundel-before-workspace-open-functions nil
  "Hook which is run before a workspace is opened.
A single argument is passed which is the path to the workspace to
be opened."
  :group 'treebundel
  :type '(list function))

(defcustom treebundel-after-workspace-open-hook nil
  "Hook which is run after a workspace is opened."
  :group 'treebundel
  :type 'hook)


;;;; Logging
(defface treebundel--gitlog-heading
  '((t (:inherit outline-1 :box t :extend t)))
  "Face for widget group labels in treebundel's dashboard."
  :group 'treebundel)

(defvar treebundel--gitlog-buffer "*treebundel-git*")

(defun treebundel--gitlog-buffer ()
  "Return the git history log buffer for treebundel."
  (or (get-buffer treebundel--gitlog-buffer)
      (let ((buf (get-buffer-create treebundel--gitlog-buffer)))
        (with-current-buffer buf
          (read-only-mode 1)
          (set (make-local-variable 'window-point-insertion-type) t))
        buf)))

(defun treebundel--gitlog (type &rest msg)
  "Insert a message in the treebundel git log buffer.
TYPE is the type of log message.  Can be either \\='command or \\='output.

MSG is the text to be inserted into the log."
  (with-current-buffer (treebundel--gitlog-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (cond ((eq 'command type)
             (insert (propertize (string-join (append '("git") msg '("\n")) " ") 'face 'treebundel--gitlog-heading)))
            ((eq 'output type)
             (let ((msg (apply #'format msg)))
               (when (= (length msg) 0)
                 (setq msg " "))
               (insert msg))
             (newline 2))))))

(defun treebundel--message (&rest args)
  "Format a message with a treebundel prefix.
ARGS is same arguments as `message'."
  (message "treebundel: %s" (apply #'format args)))

(define-error 'treebundel-error "treebundel error")

(defun treebundel--error (format &rest args)
  "Print and error with ARGS formatted with FORMAT."
  (signal 'treebundel-error (list (apply #'format-message format args))))


;;;; Git operations

;; Parse the git output into something more useful.
;;
;; The two macros below should be used only by the functions within this
;; section. Additional git operations should have their own accompanying
;; function instead of using a macro directly.

(defmacro treebundel--git (&rest args)
  "Base macro for all treebundel git commands.
ARGS are the arguments passed to git."
  (declare (indent defun))
  `(with-temp-buffer
     (treebundel--gitlog 'command (string-join (list ,@args) " "))
     (let ((result (vc-git--call t ,@args))
           (output (string-trim-right (buffer-string))))
       (treebundel--gitlog 'output (string-replace "%" "%%" output))
       (when (> result 0)
         (user-error "Git command error.  See %s: %s" treebundel--gitlog-buffer output))
       output)))

(defmacro treebundel--git-with-repo (repo-path &rest args)
  "Run a command on a specific git repository.
REPO-PATH is the repository to pass to git with the '-C' switch.

ARGS are the arguments passed to git."
  (declare (indent defun))
  `(treebundel--git "-C" ,repo-path ,@args))

(defun treebundel--branches (repo-path &optional omit-main)
  "Return a list of branches for repository at REPO-PATH.
When OMIT-MAIN is non-nil, exclude the default branch."
  (let ((branches (split-string (treebundel--git-with-repo repo-path
                                  "branch" "--format=%(refname:short)")
                                "\n")))
    (seq-remove #'string-empty-p
                (if omit-main
                    (let ((main-branch (treebundel--branch-default repo-path)))
                      (remove main-branch branches))
                  branches))))

(defun treebundel--worktree-remove (repo-path)
  "Remove the worktree at REPO-PATH."
  (treebundel--git-with-repo (treebundel-bare-path (treebundel--bare repo-path))
    "worktree" "remove" repo-path))

(defun treebundel--worktree-add (bare worktree-path branch-name)
  "Create a worktree.
BARE is the main repository the worktree is being created from.

WORKTREE-PATH is the path where the new worktree will be created.

BRANCH-NAME is the name of branch to be created and checked out at
WORKTREE-PATH.

Returns the path to the newly created worktree."
  (let ((bare-path (treebundel-bare-path bare)))
    (if (member branch-name (treebundel--branches bare-path))
        (treebundel--git-with-repo bare-path
          "worktree" "add" worktree-path branch-name)
      (treebundel--git-with-repo bare-path
        "worktree" "add" worktree-path "-b" branch-name)))
  worktree-path)

(defun treebundel--worktree-list (repo-path)
  "Return a list of worktrees for REPO-PATH."
  (seq-map
   (lambda (worktree)
     (split-string worktree "\0" t))
   (split-string (treebundel--git-with-repo repo-path
                   "worktree" "list" "-z" "--porcelain")
                 "\0\0"
                 t)))

(defun treebundel--branch (repo-path)
  "Return the branch checked out REPO-PATH."
  (treebundel--git-with-repo repo-path
    "branch" "--show-current"))

(defun treebundel--clone (url)
  "Clone a repository from URL to the bare repo directory.
Place the cloned repository as a bare repository in the directory declared in
`treebundel-bare-dir' so worktrees can be created from it as workspace projects."
  (let* ((name (car (last (split-string url "/"))))
         (dest (treebundel-bare-path name)))
    (when (file-exists-p dest)
      (user-error "Repository with this name is already cloned"))
    (treebundel--git "clone" url "--bare" dest)
    (treebundel--git-with-repo dest "config" "remote.origin.fetch" "+refs/heads/*:refs/remotes/origin/*")
    (treebundel--git-with-repo dest "fetch")
    dest))

(defun treebundel--rev-count (repo-path commit-a &optional commit-b)
  "Return the number of commits between COMMIT-A and COMMIT-B at REPO-PATH.
If COMMIT-B is nil, count between HEAD Of default branch and COMMIT-A."
  (unless commit-b
    (setq commit-b commit-a)
    (setq commit-a (treebundel--branch-default repo-path)))
  (string-to-number
   (treebundel--git-with-repo repo-path
     "rev-list" (concat commit-a ".." commit-b) "--count")))

(defun treebundel--bare (repo-path)
  "Return the name of the bare repo related to REPO-PATH."
  (thread-first (treebundel--git-with-repo repo-path
                  "rev-parse" "--path-format=absolute" "--git-common-dir")
                (directory-file-name)
                (file-name-base)))

(defun treebundel--worktree-count (repo-path)
  "Return the number of worktrees that exist for REPO-PATH."
  (seq-count
   (lambda (str) (not (member "bare" str)))
   (treebundel--worktree-list repo-path)))

(defun treebundel--has-worktrees-p (repo-path)
  "Return t if REPO-PATH has any worktrees."
  (> (treebundel--worktree-count repo-path) 0))

(defun treebundel--repo-clean-p (repo-path)
  "Return t if there are no uncommitted modifications in project.
REPO-PATH is the absolute path of the repo to check."
  (length= (split-string
            (treebundel--git-with-repo repo-path
              "status" "-z" "--porcelain")
            "\0"
            t)
           0))


;;;; Workspace management

;; These functions provide useful functions for and the rules to enforce the
;; definitions of the terminology at the top of this package.

;; Bares
(defun treebundel-bare-path (bare)
  "Return the path of bare repository with BARE."
  (file-name-concat treebundel-bare-dir
                    (if (string= "git" (file-name-extension bare))
                        bare
                      (concat bare ".git"))))

(defun treebundel--bare-delete (bare)
  "Delete the bare repository at BARE."
  (delete-directory (treebundel-bare-path bare) t))

(defun treebundel--bare-list ()
  "Return a list of all existing bare repository directory names."
  (unless (file-exists-p treebundel-bare-dir)
    (make-directory treebundel-bare-dir))
  (directory-files treebundel-bare-dir nil "\\`[^.].*"))

(defun treebundel--bare-unpushed-commits-p (bare &optional branches)
  "Return t if there are commits not on remote.
BARE is the bare repo to check.

If BRANCH is nil, check all local BRANCHES.  If BRANCH is a string or list of
strings, only check these local branches."
  (when (eq 'string (type-of branches))
    (setq branches (list branches)))
  (> (length (treebundel--git-with-repo (treebundel-bare-path bare)
               "log" "--branches" "--not" "--remotes")) 0))

;; Workspaces
(defun treebundel-workspace-path (name)
  "Return the path of a workspace named NAME."
  (file-name-concat treebundel-workspace-root name))

(defun treebundel--workspace-projects (workspace)
  "Return a list of absolute paths to projects in WORKSPACE."
  (thread-last (directory-files (treebundel-workspace-path workspace) t "\\`[^\\.]")
               (seq-filter #'file-directory-p)
               (seq-map (lambda (path) (file-name-base path)))))

(defun treebundel--workspaces ()
  "Return a list of all existing workspace names."
  (seq-map #'file-name-nondirectory
           (seq-filter #'file-directory-p
                       (directory-files treebundel-workspace-root t "\\`[^.].*"))))

(defun treebundel-current-workspace (&optional file-path)
  "Return the name of the current workspace.
If FILE-PATH is non-nil, use the current buffer instead."
  (when-let ((file-path (or (and file-path (expand-file-name file-path))
                            default-directory)))
    (let ((workspace nil))
      ;; Traverse up parent directories until the workspace root is all that remains
      (while (string-prefix-p treebundel-workspace-root (directory-file-name file-path))
        (setq workspace (file-name-nondirectory (directory-file-name file-path)))
        (setq file-path (file-name-directory (directory-file-name file-path))))
      workspace)))

;; Projects
(defun treebundel--project-add (workspace bare &optional branch-name project)
  "Add a project to a workspace.
Defines the way project worktrees are added and named in workspaces.

WORKSPACE is the name of the workspace to place the new project in.

BARE is the name of the bare repository the worktree is being created from.

BRANCH-NAME is the name of branch to be created and checked out in the
workspace.

PROJECT is the name of the worktrees' directory in the workspace."
  (treebundel--project-name
   (treebundel--worktree-add bare
                             (treebundel-project-path workspace
                                                      (or project bare))
                             (or branch-name
                                 (treebundel--branch-name workspace)))))

(defun treebundel--project-current (&optional file-path)
  "Return the project path of FILE-PATH.
If FILE-PATH is non-nil, use the current buffer."
  (when-let* ((file-path (or (and file-path (expand-file-name file-path))
                             buffer-file-name))
              (workspace-path (treebundel-workspace-path
                               (treebundel-current-workspace file-path))))
    (let ((project nil))
      ;; Traverse up parent directories until the current workspace is all that remains
      (while (string-prefix-p workspace-path (directory-file-name file-path))
        (setq project (file-name-nondirectory (directory-file-name file-path)))
        (setq file-path (file-name-directory (directory-file-name file-path))))
      project)))

(defun treebundel--project-name (project-path)
  "Return the name of project at PROJECT-PATH."
  (file-name-nondirectory (directory-file-name project-path)))

(defun treebundel-project-path (workspace project)
  "Return the path of PROJECT in WORKSPACE."
  (file-name-concat treebundel-workspace-root workspace project))

;; Branches
(defun treebundel--branch-name (workspace)
  "Generate a branch name for WORKSPACE."
  (concat treebundel-branch-prefix workspace))

(defun treebundel--branch-default (repo-path)
  "Return the default branch at REPO-PATH.
The bare repository should have it's HEAD set to the HEAD of remote, which is
the default branch.  So this function just gets the branch that the HEAD of the
bare repo points to."
  (treebundel--branch (treebundel--bare repo-path)))


;;;; User functions

;; This section provides the stable user interface.

(defun treebundel-read-bare (&optional prompt clone omit)
  "Interactively find the path of a bare.
PROMPT is the text prompt presented to the user in the minibuffer.

When CLONE is non-nil, add a completion candidate named this
'[ clone ]' which will prompt the user to clone a new repository
instead.

When OMIT is non-nil, it should be a list of a candidates to be
excluded from the candidates."
  (let* ((candidates (mapcar (lambda (bare)
                               (let ((bare (replace-regexp-in-string "\\.git$" "" bare)))
                                 (cons bare 'existing)))
                             (treebundel--bare-list))))
    (when omit
      (setq candidates (seq-remove (lambda (bare) (member (car bare) omit))
                                   candidates)))
    (when clone
      (setq candidates (append candidates '(("[ clone ]" . clone)))))
    (let ((selection (assoc (completing-read (or prompt "Select project: ") candidates)
                            candidates)))
      (if (equal (cdr selection) 'clone)
          (call-interactively #'treebundel-clone)
        (car selection)))))

(defun treebundel-read-project (workspace &optional prompt add initial require-match)
  "Interactively find the path of a project.
WORKSPACE is the workspace to look for projects in.

PROMPT is the prompt to be presented to the user in the
minibuffer.

When ADD is non-nil, add an option for the user to add a project
that isn't in the workspace.

INITIAL is the default value for the name of the project that is
automatically inserted when the minibuffer prompt is shown.

REQUIRE-MATCH forces a valid workspace to be selected.  This removes
the ability to create a workspace with a new entry."
  (let* ((candidates (mapcar (lambda (project)
                               (cons project 'existing))
                             (treebundel--workspace-projects workspace))))
    (when add (setq candidates (append candidates '(("[ add ]" . add)))))
    (let ((selection (assoc (completing-read (or prompt "Project: ")
                                             candidates
                                             nil
                                             require-match
                                             initial)
                            candidates)))
      (if (equal (cdr selection) 'add)
          (let ((bare (treebundel-read-bare prompt t)))
            (treebundel--project-add workspace
                                     bare
                                     (treebundel--branch-name workspace)))
        (car selection)))))

(defun treebundel-read-branch (repo-path &optional prompt initial)
  "Interactively selected a branch to checkout for project.
REPO-PATH is the path to project to list available branches for.

PROMPT is the prompt to be presented to the user in the minibuffer.

INITIAL is the default value of the branch of the project that is automatically
inserted when the minibuffer prompt is shown."
  (when treebundel-fetch-on-add
    (treebundel--message "Fetching...")
    (treebundel--git-with-repo repo-path "fetch"))
  (completing-read (or prompt "Branch: ")
                   (treebundel--branches repo-path)
                   nil
                   nil
                   (or initial (treebundel--branch-name
                                (treebundel--bare
                                 repo-path)))))

(defun treebundel-read-workspace (&optional prompt require-match)
  "Interactively find the path of a workspace.
PROMPT is the prompt to be presented to the user in the
minibuffer.

REQUIRE-MATCH forces a valid workspace to be selected.  This removes the ability
to create a workspace with a new entry."
  (when (and (not (file-exists-p treebundel-workspace-root))
             (y-or-n-p (format "%s directory doesn't exist. Create?"
                               treebundel-workspace-root)))
    (make-directory treebundel-workspace-root))
  (let* ((candidates (mapcar (lambda (workspace)
                               (cons workspace 'existing))
                             (treebundel--workspaces)))
         (default (treebundel-current-workspace))
         (prompt (if default (format "%s [%s]: " (or prompt "Workspace") default) "Workspace: "))
         (read (completing-read prompt candidates nil require-match nil nil default))
         (selection (assoc read candidates)))
    (if (eq (cdr selection) 'existing)
        (car selection)
      (let ((workspace-path (treebundel-workspace-path read)))
        (when (y-or-n-p (format "Are you sure you want to create a new workspace '%s'?"
                                read))
          (make-directory workspace-path)
          read)))))

(defun treebundel--git-url-like-p (url)
  "Return non-nil if URL seems like a git-clonable URL.
The URL is returned for non-nil."
  (and (or (string-prefix-p "ssh://git@" url)
           (string-prefix-p "git@" url)
           (string-prefix-p "http://" url)
           (string-prefix-p "https://" url))
       (string-suffix-p ".git" url)
       url))

;;;###autoload
(defun treebundel-open (workspace project)
  "Open or create a workspace and a project within it.
This will always prompt for a workspace.  If you want to prefer your
current workspace, use `treebundel-open-project'.

WORKSPACE is the name of the workspace to open.

PROJECT is the name of the project within the workspace to open."
  (interactive
   (let ((workspace (treebundel-read-workspace "Open workspace")))
     (list workspace
           (treebundel-read-project workspace
                                    (format "Open project in %s: " workspace)
                                    t))))
  (let* ((new-workspace-p (not (string= (treebundel-current-workspace) workspace)))
         (new-project-p (not (string= (treebundel--project-current) project))))
    (when new-workspace-p (run-hook-with-args 'treebundel-before-workspace-open-functions workspace))
    (when new-project-p (run-hook-with-args
                         'treebundel-before-project-open-functions
                         workspace project))

    (funcall treebundel-project-open-function (treebundel-project-path workspace project))

    (when new-project-p (run-hooks 'treebundel-after-project-open-hook))
    (when new-workspace-p (run-hooks 'treebundel-after-workspace-open-hook))))

;;;###autoload
(defun treebundel-open-project (workspace project)
  "Open a project in some treebundel workspace.
This function will try to use your current workspace first if the
current buffer is in one.

WORKSPACE is the name of the workspace to open.

PROJECT is the name of the project within the workspace to open."
  (interactive
   (let ((workspace (or (treebundel-current-workspace)
                        (treebundel-read-workspace))))
     (list workspace (treebundel-read-project workspace
                                              (format "Open project in %s: " workspace)
                                              t))))
  (treebundel-open workspace project))

;;;###autoload
(defun treebundel-delete-workspace (workspace)
  "Delete workspace at WORKSPACE.
This will check if all projects within the workspace are clean and if so, remove
everything in the workspace.  Anything committed is still saved in the
respective projects' bare repository located at `treebundel-bare-dir'."
  (interactive
   (list (treebundel-read-workspace "Delete workspace" t)))
  (let* ((workspace-path (treebundel-workspace-path workspace))
         (project-paths (directory-files workspace-path t "\\`[^.].*")))
    (if (and (seq-every-p (lambda (project-path)
                            (treebundel--repo-clean-p project-path))
                          project-paths)
             (or (length= project-paths 0)
                 (y-or-n-p (format "Workspace '%s' has %s project%s. Delete all?"
                                   workspace
                                   (length project-paths)
                                   (if (length= project-paths 1) "" "s")))))
        (progn
          (dolist (project-path project-paths)
            (treebundel--worktree-remove project-path))
          (delete-directory workspace-path)
          (treebundel--message "Deleted workspace '%s'" workspace))
      (user-error "There must not be any unsaved changes to delete a workspace"))))

;;;###autoload
(defun treebundel-add-project (workspace bare project project-branch)
  "Add a project to a workspace.
This will create a worktree in WORKSPACE with a branch named
after the workspace with `treebundel-branch-prefix' prefixed.

WORKSPACE is the name of the workspace where the worktree will be
created.

BARE is the bare git repository where the worktree is derived.

PROJECT is the project where the worktree will be created.  The
provided project should be in workspace WORKSPACE.

PROJECT-BRANCH is the name of the branch to be checked out for
this project."
  (interactive
   (let* ((workspace (treebundel-read-workspace "Add project to" t))
          (bare (treebundel-read-bare (format "Add project to %s: "
                                              workspace)
                                      t
                                      (treebundel--workspace-projects workspace)))
          (project-branch (treebundel-read-branch (treebundel-bare-path bare)))
          (project (treebundel-read-project workspace "Project name: "
                                            nil
                                            bare)))
     (list workspace bare project project-branch)))
  (treebundel-open workspace (treebundel--project-add workspace
                                                      bare
                                                      project-branch
                                                      project)))

;;;###autoload
(defun treebundel-remove-project (workspace project)
  "Remove PROJECT from workspace WORKSPACE.
There must be no changes in the project to remove it."
  (interactive
   (let ((workspace (or (and (not current-prefix-arg)
                             (treebundel-current-workspace))
                        (treebundel-read-workspace "Remove project from" t))))
     (list workspace
           (treebundel-read-project workspace
                                    (format "Remove project from %s: "
                                            workspace)))))
  (let ((project-path (treebundel-project-path workspace project)))
    (if (and (treebundel--repo-clean-p project-path)
             (treebundel--worktree-remove project-path))
        (treebundel--message "Removed project '%s' from '%s'"
                             project
                             workspace)
      (treebundel--error "Cannot remove '%s/%s' because the project is dirty"
                         (treebundel-current-workspace project-path)
                         project))))

;;;###autoload
(defun treebundel-move-project (workspace project new-workspace)
  "Move a project from one workspace to another.
WORKSPACE is the name of the workspace that contains the project to be
moved.

PROJECT is name of the project to move to a new workspace.

NEW-WORKSPACE is the name of the workspace the project will be moved
into."
  (interactive
   (let* ((workspace (treebundel-read-workspace "Move project from workspace" t))
          (project (treebundel-read-project workspace
                                            (format "Move project from %s: " workspace)
                                            nil
                                            nil
                                            t))
          (new-workspace (treebundel-read-workspace
                          (format "Move %s to workspace" project)
                          t)))
     (list workspace project new-workspace)))
  (treebundel--git-with-repo (treebundel-project-path workspace project)
    "worktree"
    "move"
    (treebundel-project-path workspace project)
    (file-name-concat (treebundel-workspace-path new-workspace) project))
  (treebundel--message "Moved project '%s' from workspace '%s' -> '%s'"
                       project
                       workspace
                       new-workspace))

;;;###autoload
(defun treebundel-rename-project (workspace project new-name)
  "Rename a project.
WORKSPACE is the name of the workspace that contains the project to be
renamed.

PROJECT is name of the project in WORKSPACE to be renamed.

NEW-NAME is the new name PROJECT will be renamed to."
  (interactive
   (let* ((workspace (treebundel-read-workspace "Rename project in workspace" t))
          (project (treebundel-read-project workspace
                                            (format "Rename project in %s: " workspace)
                                            nil
                                            nil
                                            t))
          (new-name (read-string "New name: " project)))
     (list workspace project new-name)))
  (treebundel--git-with-repo (treebundel-project-path workspace project)
    "worktree"
    "move"
    (treebundel-project-path workspace project)
    (treebundel-project-path workspace new-name))
  (treebundel--message "Renamed project '%s' -> '%s'"
                       project
                       new-name))

;;;###autoload
(defun treebundel-clone (url)
  "Clone URL to the collection of bare repos.
Once a repository is in the bare repos collection, you can add it to a project
with `treebundel-add-project'"
  (interactive
   (list (read-string "URL: " (or (treebundel--git-url-like-p (gui-get-selection 'CLIPBOARD 'STRING))
                                  (treebundel--git-url-like-p (gui-get-selection 'PRIMARY 'STRING))))))
  (treebundel--message "Cloning %s..." url)
  (let ((bare-name (string-remove-suffix ".git"
                                         (file-name-nondirectory
                                          (directory-file-name
                                           (treebundel--clone url))))))
    (treebundel--message "Finished cloning %s." bare-name)
    bare-name))

;;;###autoload
(defun treebundel-delete-bare (bare &optional interactive)
  "Delete a bare repository BARE.
Existing worktrees or uncommitted changes will prevent you from deleting.

If INTERACTIVE is non-nil, prompt the user to force delete for any changes not
on remote."
  (interactive
   (list (treebundel-read-bare "Select repo to delete: ") t))
  (cond ((treebundel--has-worktrees-p (treebundel-bare-path bare))
         (treebundel--error "This repository has worktrees checked out"))
        ((and (treebundel--bare-unpushed-commits-p bare)
              (not (if interactive
                       (yes-or-no-p (format "%s has unpushed commits on some branches.  Delete anyway?" bare))
                     (treebundel--error (format "%s has unpushed commits on some branches" bare))))))
        (t (treebundel--bare-delete bare))))

(defun treebundel-fetch-bare (bare)
  "Perform a git-fetch on bare repo.
BARE is the name of the bare repo to fetch.

This command is normally not useful unless `treebundel-fetch-on-add' is
disabled.  Use this command to manually control when git-fetch operations are
performed."
  (interactive (list (treebundel-read-bare "Select repo to fetch: ")))
  (treebundel--message "Fetching...")
  (treebundel--git-with-repo bare "fetch")
  (treebundel--message "%s updated" bare))

(provide 'treebundel)

;;; treebundel.el ends here
