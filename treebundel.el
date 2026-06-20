;;; treebundel.el --- Bundle related git-worktrees together -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1") (transient "0.13.4"))
;; Version: 0.3.0
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
(require 'vc-git)
(require 'transient)


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

(defcustom treebundel-bare-dir ".bare"
  "The path where bare repositories are stored.
This is a relative path to `treebundel-workspace-root'."
  :group 'treebundel
  :type 'string)

(defcustom treebundel-project-open-function
  'project-switch-project
  "Function called to switch to a new project."
  :group 'treebundel
  :type 'function)

(defcustom treebundel-fetch-on-add nil
  "When t, perform a git-fetch before adding a project to a workspace.
This allows the latest branches on remote to appear when selecting a branch to
checkout.

Set to nil when you don't want to make network requests or just to reduce git
operations when adding projects to your workspaces."
  :group 'treebundel
  :type 'boolean)

;;;;; Hooks
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

;;;;; Faces
(defface treebundel-workspace '((t :inherit bold :foreground "#0098CF"))
  "Face used for workspaces."
  :group 'treebundel-faces)

(defface treebundel-bare '((t :inherit bold :foreground "#C300FF"))
  "Face used for projects."
  :group 'treebundel-faces)

(defface treebundel-project '((t :inherit bold :foreground "#24A600"))
  "Face used for projects."
  :group 'treebundel-faces)

(defface treebundel-error '((t :inherit transient-key-exit))
  "Face used for error things."
  :group 'treebundel-faces)

(defface treebundel-disabled '((t :inherit transient-inactive-value))
  "Face used for disabled things."
  :group 'treebundel-faces)

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
  (message "%s" (apply #'format args)))

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

;;;;; Git macros
(defmacro treebundel--git (&rest args)
  "Base macro for all treebundel git commands.
ARGS are the arguments passed to git."
  (declare (indent defun))
  `(with-temp-buffer
     (treebundel--gitlog 'command (string-join (list ,@args) " "))
     (let ((result (vc-git-command (current-buffer) nil nil ,@args))
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

(defun treebundel--bare-clone (url)
  "Clone a repository from URL to the bare repo directory.
Place the cloned repository as a bare repository in the directory declared in
`treebundel-bare-dir' within `treebundel-workspace-root' so worktrees can be
created from it as workspace projects."
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

;;;;; Branches
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

(defun treebundel--branch (repo-path)
  "Return the branch checked out REPO-PATH."
  (treebundel--git-with-repo repo-path
    "branch" "--show-current"))

;;;;; Utility
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

(defun treebundel-managed-p (repo-path)
  "Return t if the repo at REPO-PATH is compatible with treebundel."
  (and repo-path (treebundel--repo-bare repo-path) t))


;;;;; Worktrees
(defun treebundel--worktree-remove (project-path &optional force)
  "Remove the worktree at PROJECT-PATH.
If FORCE is t, then add --force to the command."
  (treebundel--git-with-repo (treebundel-bare-path (treebundel--repo-bare project-path))
    "worktree" "remove" (when force "--force") project-path))

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

(defun treebundel--git-url-like-p (url)
  "Return non-nil if URL seems like a git-clonable URL.
The URL is returned for non-nil."
  (and (or (string-prefix-p "ssh://git@" url)
           (string-prefix-p "git@" url)
           (string-prefix-p "http://" url)
           (string-prefix-p "https://" url))
       (string-suffix-p ".git" url)
       url))

;;;; Format
(defun treebundel--fmt-bare (&optional bare)
  "Format the text of a BARE name."
  (let ((bare (or bare (treebundel--bare-current))))
    (format "%s" (propertize bare 'face 'treebundel-bare))))

(defun treebundel--fmt-workspace (&optional workspace)
  "Format the text of a WORKSPACE name."
  (let ((workspace (or workspace (treebundel-current-workspace))))
    (format "%s/" (propertize workspace 'face 'treebundel-workspace))))

(defun treebundel--fmt-project (&optional project)
  "Format the text of a PROJECT name."
  (let ((project (or project (treebundel--project-current))))
    (format "%s" (propertize project 'face 'treebundel-project))))

(defun treebundel--fmt-workspace-project (&optional workspace project)
  "Format the text of a WORKSPACE and PROJECT pair."
  (let ((workspace (or workspace (treebundel-current-workspace)))
        (project (or project (treebundel--project-current))))
    (format "%s/%s"
            (propertize workspace 'face 'treebundel-workspace)
            (propertize project 'face 'treebundel-project))))

;;;; Workspace management

;; These functions provide useful functions for and the rules to enforce the
;; definitions of the terminology at the top of this package.

;;;;; Repos
(defun treebundel--repo-bare (repo-path)
  "Return the name of the bare repo related to REPO-PATH."
  (when repo-path
    (let ((bare-name (thread-first (treebundel--git-with-repo repo-path
                                     "rev-parse" "--path-format=absolute" "--git-common-dir")
                                   (directory-file-name)
                                   (file-name-base))))
      (unless (string= ".git" bare-name) bare-name))))

;;;;; Bares
(defun treebundel-bare-path (&optional bare)
  "Return the path of bare repository with BARE."
  (when-let* ((bare (or bare (treebundel--bare-current))))
    (file-name-concat treebundel-workspace-root treebundel-bare-dir
                      (if (string= "git" (file-name-extension bare))
                          bare
                        (concat bare ".git")))))

(defun treebundel--bare-delete (bare)
  "Delete the bare repository at BARE."
  (delete-directory (treebundel-bare-path bare) t))

(defun treebundel--bare-list ()
  "Return a list of all existing bare repository directory names."
  (let ((bare-dir  (file-name-concat treebundel-workspace-root treebundel-bare-dir)))
    (unless (file-exists-p bare-dir)
      (make-directory bare-dir))
    (directory-files bare-dir nil "\\`[^.].*")))

(defun treebundel--bare-current (&optional file-path)
  "Return the bare name of FILE-PATH."
  (when-let* ((workspace (treebundel-current-workspace file-path))
              (project (treebundel--project-current file-path)))
    (treebundel--repo-bare (treebundel--project-path workspace project))))

(defun treebundel--bare-unpushed-commits-p (bare &optional branches)
  "Return t if there are commits not on remote.
BARE is the bare repo to check.

If BRANCH is nil, check all local BRANCHES.  If BRANCH is a string or list of
strings, only check these local branches."
  (when (eq 'string (type-of branches))
    (setq branches (list branches)))

  (length>
   (treebundel--git-with-repo (treebundel-bare-path bare)
     "log" "--branches" "--not" "--remotes")
   0))

(defun treebundel--bare-read (prompt initial-input history)
  ""
  (let* ((candidates (mapcar (lambda (bare)
                               (let ((bare (replace-regexp-in-string "\\.git$" "" bare)))
                                 (cons bare 'existing)))
                             (treebundel--bare-list))))
    (let ((selection (assoc (completing-read prompt candidates nil nil initial-input history) candidates)))
      (car selection))))

;;;;; Branches
(defun treebundel--branch-name (workspace)
  "Generate a branch name for WORKSPACE."
  (concat treebundel-branch-prefix workspace))

(defun treebundel--branch-default (repo-path)
  "Return the default branch at REPO-PATH.
The bare repository should have it's HEAD set to the HEAD of remote, which is
the default branch.  So this function just gets the branch that the HEAD of the
bare repo points to."
  (treebundel--branch (treebundel--repo-bare repo-path)))


;;;;; Projects
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
                             (treebundel-project-path workspace (or project bare))
                             (or branch-name (treebundel--branch-name workspace)))))

(defun treebundel--project-current (&optional file-path)
  "Return the project name of FILE-PATH.
If FILE-PATH is non-nil, use the current buffer."
  (when-let* ((file-path (or (and file-path (expand-file-name file-path))
                             buffer-file-name))
              (file-path (directory-file-name file-path))
              (workspace-path (treebundel-workspace-path
                               (treebundel-current-workspace file-path))))
    (when (string-prefix-p workspace-path file-path)
      (let* ((relative (string-remove-prefix (file-name-directory workspace-path) file-path))
             (split (string-split relative "/")))
        (cadr split)))))

(defun treebundel--project-name (project-path)
  "Return the name of project at PROJECT-PATH."
  (file-name-nondirectory (directory-file-name project-path)))

(defun treebundel-project-path (&optional workspace project)
  "Return the path of PROJECT in WORKSPACE.
Leave either PROJECT or WORKSPACE nil to try to use current."
  (when-let* ((workspace (or workspace (treebundel-current-workspace)))
              (project (or project (treebundel--project-current))))
    (file-name-concat treebundel-workspace-root workspace project)))
(defalias 'treebundel--project-path 'treebundel-project-path)

;;;;; Workspaces
(defun treebundel-workspace-path (name)
  "Return the path of a workspace named NAME."
  (file-name-concat treebundel-workspace-root name))

(defun treebundel--workspace-projects (workspace)
  "Return a list of absolute paths to projects in WORKSPACE."
  (thread-last (directory-files (treebundel-workspace-path workspace) t "\\`[^\\.]")
               (seq-filter #'file-directory-p)
               (seq-map (lambda (path) (file-name-nondirectory path)))))

(defun treebundel--workspaces ()
  "Return a list of all existing workspace names."
  (seq-map #'file-name-nondirectory
           (seq-filter #'file-directory-p
                       (directory-files treebundel-workspace-root t "\\`[^.].*"))))

(defun treebundel-current-workspace (&optional file-path)
  "Return the name of the current workspace.
If FILE-PATH is non-nil, use the current buffer instead."
  (when-let* ((file-path (or (when file-path (expand-file-name file-path))
                             buffer-file-name
                             default-directory)))
    (let ((workspace nil))
      ;; Traverse up parent directories until the workspace root is all that remains
      (while (string-prefix-p treebundel-workspace-root (directory-file-name file-path))
        (setq workspace (file-name-nondirectory (directory-file-name file-path)))
        (setq file-path (file-name-directory (directory-file-name file-path))))
      workspace)))

;;;; User functions

;; This section provides the stable user interface.

;;;;; Top
;;;###autoload(autoload 'treebundel "treebundel" nil t)
(transient-define-prefix treebundel ()
  ["Quick"
   ("w" "Open workspace" treebundel-open-workspace)
   ("p" "Project" treebundel-open-project :if treebundel-current-workspace
    :description (lambda () (format "Open project in %s" (treebundel--fmt-workspace))))
   ("f" "Find file" project-find-file :if treebundel--project-current
    :description (lambda () (format "Find file in %s" (treebundel--fmt-workspace-project))))
   ("a" "Add project" treebundel-add-project :if treebundel-current-workspace
    :description (lambda () (format "Add project to %s" (treebundel--fmt-workspace))))]

  ["Configure"
   ("W" "Workspace" treebundel-workspace :if treebundel-current-workspace
    :description (lambda () (format "Workspace %s" (treebundel--fmt-workspace))))

   ("P" "Project" treebundel-project :if treebundel--project-current
    :description (lambda () (format "Project %s" (treebundel--fmt-workspace-project))))

   ("B" "Bare" treebundel-bare :if treebundel--bare-current
    :description (lambda () (format "Bare %s" (treebundel--fmt-bare))))]

  ["Debug" :level 6
   ("l" "Log" treebundel-open-gitlog)])

(defun treebundel--not-implemented ()
  "A placeholder command for unimplemented transient commands."
  (interactive)
  (treebundel--message "This command is not yet implemented"))

;;;;; Bare
(defvar treebundel--current-bare nil)

(transient-define-prefix treebundel-bare (bare-scope)
  "Prefix for working with bare repositories."
  [("-f" "Force" ("-f" "--force"))
   ("-F" "Force" ("-F" "--force-delete-unpushed-commits"))
   ("-y" "Yank From Clipboard" ("-y" "--yank"))]

  [:description (lambda () (treebundel--fmt-bare))
                ("b" "Bare" treebundel--bare-select)
                ("c" "Clone" treebundel-clone)]

  [:description (lambda () (treebundel--fmt-bare))

                ("k" "Delete" treebundel-delete-bare
                 :description (lambda ()
                                (format "Delete (in use by %s projects)"
                                        (propertize (format "%d" (treebundel--worktree-count (treebundel-project-path)))
                                                    'face 'transient-argument))))

                ;; Open a file in this bare's directory
                ("v" "Visit" treebundel-visit-bare)

                ("b" "Switch" treebundel--not-implemented
                 :description (lambda () (propertize "Switch (TODO)" 'face 'treebundel-disabled)))

                ;; Automatically fetch bare of current project
                ("f" "Fetch" treebundel--not-implemented
                 :description (lambda () (propertize "Fetch (TODO)" 'face 'treebundel-disabled)))

                ;; List projects checked out that are currently associated with this bare repo.
                ("p" "Projects" treebundel--not-implemented
                 :description  (lambda () (propertize "Projects (TODO)" 'face 'treebundel-disabled)))]
  (interactive "P")
  (transient-setup 'treebundel-bare nil nil :scope bare-scope))

(transient-define-infix treebundel--bare-select ()
  "The buffer to be acted on."
  :class 'transient-lisp-variable
  :description "bare"
  :prompt "Select bare: "
  :variable 'treebundel--current-bare
  :reader 'treebundel--bare-read
  :init-value (lambda (obj)
                (oset obj value (or treebundel--current-bare (treebundel--bare-current)))))

(transient-define-suffix treebundel-clone-bare (url)
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
                                           (treebundel--bare-clone url))))))
    (treebundel--message "Finished cloning %s." bare-name)))
(defalias 'treebundel-clone 'treebundel-clone-bare)

(transient-define-suffix treebundel-delete-bare ()
  "Delete a bare repository BARE.
Existing worktrees or uncommitted changes will prevent you from deleting.

If INTERACTIVE is non-nil, prompt the user to force delete for any changes not
on remote.

When FORCE is t, continue deleting even if"
  (interactive)
  (when-let* ((_ (message "scope: %s" (transient-scope)))
              (bare (treebundel-bare-path))
              (_ nil))
    (cond ((treebundel--has-worktrees-p (treebundel-bare-path bare))
           (treebundel--error "This repository has worktrees checked out"))

          ((and (treebundel--bare-unpushed-commits-p bare)
                (not (yes-or-no-p (format "%s has unpushed commits on some branches.  Delete anyway?" bare)))))

          (t (treebundel--bare-delete bare)))))

(transient-define-suffix treebundel-fetch-bare ()
  "Perform a git-fetch on bare repo.
BARE is the name of the bare repo to fetch.

This command is normally not useful unless `treebundel-fetch-on-add' is
disabled.  Use this command to manually control when git-fetch operations are
performed."
  (interactive)
  (let ((bare (treebundel-bare-path)))
   (treebundel--message "Fetching...")
   (treebundel--git-with-repo bare "fetch")
   (treebundel--message "%s updated" bare)))

(transient-define-suffix treebundel-visit-bare ()
  "Find a file in the bare repository at BARE."
  (interactive)
  (when-let* ((bare (treebundel-bare-path)))
    (funcall-interactively 'find-file bare)))

(defun treebundel-read-bare (&optional prompt)
  "Interactively find the path of a bare.
PROMPT is the text prompt presented to the user in the minibuffer."
  (treebundel--bare-read (or prompt "Select project: ") nil nil))

;;;;; Projects
(transient-define-prefix treebundel-project ()
  "Working with projects."
  [:description (lambda () (treebundel--fmt-workspace-project))
   ("k" "Remove" treebundel-remove-project
    :description  (lambda () (format "Remove%s" (propertize " (Dirty)" 'face 'treebundel-error))))
   ("m" "Move" treebundel-move-project)
   ("r" "Rename" treebundel-rename-project)])

(transient-define-suffix treebundel-add-project (workspace bare project project-branch)
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
   (let* ((workspace (or (treebundel-current-workspace) (treebundel-read-workspace "Add to workspace" t)))
          (bare (treebundel-read-bare (format "Add project to %s" (treebundel--fmt-workspace workspace))))
          (project-branch (treebundel-read-branch (treebundel-bare-path bare)))
          (project (treebundel-read-project workspace "Project name: ")))
     (list workspace bare project project-branch)))
  (treebundel-open-workspace workspace (treebundel--project-add workspace
                                                      bare
                                                      project-branch
                                                      project)))

(transient-define-suffix treebundel-open-project (workspace project)
  "Open a project in some treebundel workspace.
This function will try to use your current workspace first if the
current buffer is in one.

WORKSPACE is the name of the workspace to open.

PROJECT is the name of the project within the workspace to open."
  (interactive
   (let* ((workspace (or (treebundel-current-workspace) (treebundel-read-workspace)))
          (project (treebundel-read-project workspace
                                            (format "Open project in %s" (treebundel--fmt-workspace workspace))
                                            nil
                                            t)))
     (list workspace project)))
  (treebundel-open workspace project))

(transient-define-suffix treebundel-remove-project (workspace project)
  "Remove PROJECT from workspace WORKSPACE.
There must be no changes in the project to remove it."
  (interactive
   (let ((workspace (or (and (not current-prefix-arg)
                             (treebundel-current-workspace))
                        (treebundel-read-workspace "Remove project from workspace" t)))
         (project (treebundel--project-current)))
     (list workspace project)))
  (let ((project-path (treebundel-project-path workspace project)))
    (if (and (treebundel--repo-clean-p project-path)
             (treebundel--worktree-remove project-path))
        (treebundel--message "Removed %s" (treebundel--fmt-workspace-project project workspace))
      (treebundel--message "Cannot remove %s because the project is dirty"
                           (treebundel--fmt-workspace-project (treebundel-current-workspace project-path) project)))))

(transient-define-suffix treebundel-move-project (workspace project new-workspace)
  "Move a project from one workspace to another.
WORKSPACE is the name of the workspace that contains the project to be
moved.

PROJECT is name of the project to move to a new workspace.

NEW-WORKSPACE is the name of the workspace the project will be moved
into."
  (interactive
   (when-let* ((workspace (or (treebundel-current-workspace) (treebundel-read-workspace "Move project from %s" t)))
          (project (treebundel-read-project workspace
                                            (format "Move project from %s" (treebundel--fmt-workspace workspace))
                                            nil
                                            t))
          (new-workspace (treebundel-read-workspace (format "Move %s to: " (treebundel--fmt-workspace-project workspace project)) t)))
     (list workspace project new-workspace)))
  (treebundel--git-with-repo (treebundel-project-path workspace project)
    "worktree"
    "move"
    (treebundel-project-path workspace project)
    (file-name-concat (treebundel-workspace-path new-workspace) project))
  (treebundel--message "Moved project %s from workspace %s -> %s"
                       (treebundel--fmt-project project)
                       (treebundel--fmt-workspace workspace)
                       (treebundel--fmt-workspace new-workspace)))

(transient-define-suffix treebundel-rename-project (new-name)
  "Rename a project.
WORKSPACE is the name of the workspace that contains the project to be
renamed.

PROJECT is name of the project in WORKSPACE to be renamed.

NEW-NAME is the new name PROJECT will be renamed to."
  (interactive
   (list (read-string "New name: " (treebundel--project-current))))
  (let ((workspace (treebundel-current-workspace))
        (project (treebundel--project-current)) )
    (treebundel--git-with-repo (treebundel-project-path workspace project)
      "worktree"
      "move"
      (treebundel-project-path workspace project)
      (treebundel-project-path workspace new-name))
    (treebundel--message "Renamed project '%s' -> '%s'"
                         project
                         new-name)))

(defun treebundel-read-project (workspace &optional prompt initial require-match)
  "Interactively find the path of a project.
WORKSPACE is the workspace to look for projects in.

PROMPT is the prompt to be presented to the user in the
minibuffer.

INITIAL is the default value for the name of the project that is
automatically inserted when the minibuffer prompt is shown.

REQUIRE-MATCH forces a valid workspace to be selected.  This removes
the ability to create a workspace with a new entry."
  (let* ((candidates (mapcar (lambda (project)
                               (cons project 'existing))
                             (treebundel--workspace-projects workspace))))
    (car (assoc (completing-read (or prompt "Project: ")
                                 candidates
                                 nil
                                 require-match
                                 initial)
                candidates))))

(defun treebundel-read-branch (project-path &optional prompt initial)
  "Interactively selected a branch to checkout for project.
PROJECT-PATH is the path to project to list available branches for.

PROMPT is the prompt to be presented to the user in the minibuffer.

INITIAL is the default value of the branch of the project that is automatically
inserted when the minibuffer prompt is shown."
  (when treebundel-fetch-on-add
    (treebundel--message "Fetching...")
    (treebundel--git-with-repo project-path "fetch"))
  (completing-read (or prompt "Branch: ")
                   (treebundel--branches project-path)
                   nil
                   nil
                   (or initial (treebundel--branch-name
                                (treebundel--repo-bare project-path)))))

;;;;; Log
(transient-define-suffix treebundel-open-gitlog ()
  ""
  (interactive)
  (display-buffer (treebundel--gitlog-buffer)))

;;;;; Workspaces
(transient-define-prefix treebundel-workspace ()
  ""
  ["Arguments"
   ("-f" "Ignore errors" ("-f" "--force"))
   ("-r" "Recursive" ("-r" "--recursive"))
   ("--delete-all" (lambda () (propertize "Delete data" 'face 'transient-disabled)) (nil "--delete-all"))]

  [:description (lambda () (treebundel--fmt-workspace))
   ("k" "Delete" treebundel-delete-workspace)
   ("m" "Rename" treebundel--not-implemented
    :description  (lambda () (propertize "Rename (TODO)" 'face 'treebundel-disabled)))])

(transient-define-suffix treebundel-open-workspace (workspace project)
  "Open or create a workspace and a project within it.
This will always prompt for a workspace.  If you want to prefer your
current workspace, use `treebundel-open-project'.

WORKSPACE is the name of the workspace to open.

PROJECT is the name of the project within the workspace to open."
  (interactive
   (let* ((workspace (treebundel-read-workspace))
          (project (treebundel-read-project workspace (format "Open project in %s" (treebundel--fmt-workspace workspace)))))
     (list workspace project)))
  (let* ((new-workspace-p (not (string= (treebundel-current-workspace) workspace)))
         (new-project-p (or new-workspace-p
                            (not (string= (treebundel--project-current) project)))))
    (when new-workspace-p (run-hook-with-args 'treebundel-before-workspace-open-functions workspace))
    (when new-project-p (run-hook-with-args
                         'treebundel-before-project-open-functions
                         workspace project))

    (funcall treebundel-project-open-function (treebundel-project-path workspace project))

    (when new-project-p (run-hooks 'treebundel-after-project-open-hook))
    (when new-workspace-p (run-hooks 'treebundel-after-workspace-open-hook))))
(defalias 'treebundel-open 'treebundel-open-workspace)

(transient-define-suffix treebundel-delete-workspace (args)
  "Delete workspace at WORKSPACE.
This will check if all projects within the workspace are clean and if so, remove
everything in the workspace. Anything committed is still saved in the respective
projects' bare repository located at `treebundel-bare-dir' within
`treebundel-workspace-root'."
  (interactive (list (transient-args 'treebundel-workspace)))
  (when-let* ((workspace (or (treebundel-current-workspace)
                             (treebundel-read-workspace "Delete workspace: " t)))
              (workspace-path (treebundel-workspace-path workspace))
              (project-paths (directory-files workspace-path t "\\`[^.].*")))
    (let* ((ignore-errors (transient-arg-value "--force" args)))
      (if (and (seq-every-p (lambda (project-path)
                              (treebundel--repo-clean-p project-path))
                            project-paths)
               (or (length= project-paths 0)
                   (y-or-n-p (format "Workspace '%s' has %s project%s. Delete all?"
                                     workspace
                                     (length project-paths)
                                     (if (length= project-paths 1) "" "s")))))
          (progn
            (dolist (repo-path project-paths)
              (treebundel--worktree-remove repo-path ignore-errors))
            (delete-directory workspace-path)
            (treebundel--message "Deleted workspace '%s'" workspace))
        (user-error "There must not be any unsaved changes to delete a workspace")))))

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
  (let* ((candidates (mapcar (lambda (workspace) (cons workspace 'existing))
                             (treebundel--workspaces)))
         (prompt (or prompt "Workspace: "))
         (read (completing-read prompt candidates nil require-match))
         (selection (assoc read candidates)))

    (if (eq (cdr selection) 'existing)
        (car selection)
      (let ((workspace-path (treebundel-workspace-path read)))
        (when (y-or-n-p (format "Are you sure you want to create a new workspace '%s'?"
                                read))
          (make-directory workspace-path)
          read)))))

(provide 'treebundel)
;;; treebundel.el ends here
