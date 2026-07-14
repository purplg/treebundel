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

;;;; Usage:
;;
;; The following functions are the commands you should use (and
;; probably bind) to make use of this package.
;;
;; `treebundel-switch-project'
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

;;;; Dev notes:
;;
;; Some information about how this package is organized and common conventions
;; and patterns throughout to not only try to help others understand the code,
;; but also as a way to reference and aim for consistent usage.
;;
;;;;; Naming conventions:
;;
;;  This section describes the few rules on how symbols are named through this
;;  package.
;;
;; `treebundel--' private symbols
;;   Any symbol in this package that starts with `treebundel--'.
;;   The 2 dashes signifies these are functions that are not intended for
;;   external use. They may change or get removed at any time.
;;
;; `treebundel--symbols-private'
;;   These since these are internal, they are more useful to be more organized
;;   and hierachical. So these symbols generally follow a struct like
;;   `treebundel--subject-adjective'
;;
;; `treebundel-public-symbols'
;;   Public API's are intended to be more readable so they flip their order so
;;   it reads more like plain English. `treebundel-adjective-subject'
;;
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
         (set option (file-name-as-directory (expand-file-name value)))))

(defcustom treebundel-bare-dir ".bare"
  "The path where bare repositories are stored.
This is the directory name in `treebundel-workspace-root' where bare
repositories are stored and worktrees created from."
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

;;;;; Faces
(defface treebundel-workspace '((t :inherit bold :foreground "#0098CF"))
  "Face used for workspaces."
  :group 'treebundel-faces)

(defface treebundel-workspace-active '((t :inherit treebundel-workspace :box t))
  "Face used for workspaces."
  :group 'treebundel-faces)

(defface treebundel-workspace-inactive '((t :inherit treebundel-workspace :foreground "#004963"))
  "Face used for workspaces."
  :group 'treebundel-faces)

(defface treebundel-bare '((t :inherit bold :foreground "#C300FF"))
  "Face used for projects."
  :group 'treebundel-faces)

(defface treebundel-bare-active '((t :inherit treebundel-bare :box t))
  "Face used for projects."
  :group 'treebundel-faces)

(defface treebundel-project '((t :inherit bold :foreground "#24A600"))
  "Face used for projects."
  :group 'treebundel-faces)

(defface treebundel-project-active '((t :inherit treebundel-project :box t))
  "Face used for projects."
  :group 'treebundel-faces)

(defface treebundel-project-inactive '((t :inherit treebundel-project :foreground "#197400"))
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
         (dest (treebundel--bare-path name)))
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

(defun treebundel-managed-p (repo-path)
  "Return t if the repo at REPO-PATH is compatible with treebundel."
  (and repo-path (treebundel--repo-bare repo-path) t))

;;;;; Worktrees
(defun treebundel--worktree-remove (project-path &optional force)
  "Remove the worktree at PROJECT-PATH.
If FORCE is t, then add --force to the command."
  (treebundel--git-with-repo (treebundel--bare-path (treebundel--repo-bare project-path))
    "worktree" "remove" (when force "--force") project-path))

(defun treebundel--worktree-add (bare worktree-path branch-name)
  "Create a worktree.
BARE is the main repository the worktree is being created from.

WORKTREE-PATH is the path where the new worktree will be created.

BRANCH-NAME is the name of branch to be created and checked out at
WORKTREE-PATH.

Returns the path to the newly created worktree."
  (let ((bare-path (treebundel--bare-path bare)))
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
(defun treebundel--fmt-bare (bare &optional focus)
  "Format the text of a BARE name."
  (format "%s" (propertize (or bare "⸺")
                           'face (cond ((eq focus 'active) 'treebundel-bare-active)
                                       (t 'treebundel-bare)))))

(defun treebundel--fmt-workspace (workspace &optional focus)
  "Format the text of a WORKSPACE name.
Set INACTIVE to t to use the darker face."
  (concat (propertize (or workspace "⸺") 'face (cond ((eq focus 'active) 'treebundel-workspace-active)
                                                     ((eq focus 'inactive) 'treebundel-workspace-inactive)
                                                     (t 'treebundel-workspace)))
          "/"))

(defun treebundel--fmt-project (project &optional focus)
  "Format the text of a PROJECT name."
  (propertize (or project "⸺") 'face (cond ((eq focus 'active) 'treebundel-project-active)
                                           ((eq focus t) 'treebundel-project-active)
                                           ((eq focus 'inactive) 'treebundel-project-inactive)
                                           (t 'treebundel-project))))

(cl-defun treebundel--fmt-workspace-project (workspace project &key workspace-state project-state &allow-other-keys)
  "Format the text of a WORKSPACE and PROJECT pair."
  (concat (treebundel--fmt-workspace workspace (or workspace-state (and project (not project-state) 'inactive)))
          (treebundel--fmt-project project (or project-state (unless (or project workspace-state) 'inactive)))))

;;;; Workspace management

;; These functions provide useful functions for and the rules to enforce the
;; definitions of the terminology at the top of this package.

;;;;; Repos
(defun treebundel--repo-bare (repo-path)
  "Return the name of the bare repo related to REPO-PATH."
  (when (and (file-exists-p repo-path)
             (vc-git-root repo-path))
    (let ((bare-name (thread-first (treebundel--git-with-repo repo-path
                                     "rev-parse" "--path-format=absolute" "--git-common-dir")
                                   (directory-file-name)
                                   (file-name-base))))
      bare-name)))

;;;;; Bares
(defun treebundel--bare-path (bare)
  "Return the path of bare repository with BARE."
  (file-name-concat treebundel-workspace-root treebundel-bare-dir
                    (if (string= "git" (file-name-extension bare))
                        bare
                      (concat bare ".git"))))

(defun treebundel--bare-delete (bare)
  "Delete the bare repository at BARE."
  (delete-directory (treebundel--bare-path bare) t))

(defun treebundel--bare-list ()
  "Return a list of all existing bare repository directory names."
  (let ((bare-dir  (file-name-concat treebundel-workspace-root treebundel-bare-dir)))
    (unless (file-exists-p bare-dir)
      (make-directory bare-dir))
    (directory-files bare-dir nil "\\`[^.].*")))

(defun treebundel--bare-unpushed-commits-p (bare &optional branches)
  "Return t if there are commits not on remote.
BARE is the bare repo to check.

If BRANCH is nil, check all local BRANCHES.  If BRANCH is a string or list of
strings, only check these local branches."
  (when (eq 'string (type-of branches))
    (setq branches (list branches)))

  (length>
   (treebundel--git-with-repo (treebundel--bare-path bare)
     "log" "--branches" "--not" "--remotes")
   0))

(defun treebundel--bare-read (prompt initial-input history)
  "Lookup a bare.
PROMPT INITIAL-INPUT and HISTORY are all directly forwarded to
`completing-read'."
  (let* ((candidates (mapcar (lambda (bare)
                               (replace-regexp-in-string "\\.git$" "" bare))
                             (treebundel--bare-list))))
    (completing-read prompt candidates nil nil initial-input history)))

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
  (treebundel--project-of
   (treebundel--worktree-add bare
                             (treebundel--project-path workspace (or project bare))
                             (or branch-name (treebundel--branch-name workspace)))))

(defun treebundel--project-of (file-path)
  "Return the project name of FILE-PATH.
If FILE-PATH is non-nil, use the current buffer."
  (when-let* ((workspace (treebundel--workspace-of file-path))
              (workspace-path (treebundel-workspace-path workspace))
              (relative-path (when (string-prefix-p workspace-path file-path)
                               (string-remove-prefix workspace-path file-path)))
              (parts (split-string relative-path "/" :omit-empty)))
    (car parts)))

(defun treebundel-current-project (&optional file-path)
  "Return the project name of FILE-PATH or of current file.
If FILE-PATH is non-nil, use the current buffer."
  (when-let* ((file-path (or file-path buffer-file-name default-directory)))
    (treebundel--project-of file-path)))

(defun treebundel--project-move (src-path dst-path)
  "Move a repo from SRC-PATH to DST-PATH."
  (treebundel--git-with-repo (treebundel--bare-path (treebundel--repo-bare src-path))
    "worktree" "move" src-path dst-path)
  ;; Updated related open buffers file location
  (dolist (buf (buffer-list))
    (when-let* ((src-path (file-name-as-directory src-path))
                (suffix (and (string-prefix-p src-path (buffer-file-name buf))
                             (string-remove-prefix src-path (buffer-file-name buf)) ))
                (dst-path (file-name-concat dst-path suffix)))
      (with-current-buffer buf (set-visited-file-name dst-path nil t)))))

(defun treebundel--project-path (workspace project)
  "Return the path of PROJECT in WORKSPACE."
  (if (and (length> workspace 0) (length> project 0))
      (file-name-concat treebundel-workspace-root workspace project)
    (error "Missing workspace or project arguments")))

(defun treebundel-project-path (&optional workspace project)
  "Return the path of PROJECT in WORKSPACE.
Leave either PROJECT or WORKSPACE nil to try to use current."
  (if-let* ((workspace (or workspace (treebundel-current-workspace)))
            (project (or project (treebundel-current-project)))
            ((length> workspace 0))
            ((length> project 0)))
      (file-name-concat treebundel-workspace-root workspace project)
    (error "Missing workspace or project arguments")))

(defun treebundel--project-open (workspace project)
  "Call the project open function on WORKSPACE/PROJECT."
  (if (and workspace project)
    (funcall treebundel-project-open-function (treebundel--project-path workspace project))
    (error "Must specify workspace and project")))

(defun treebundel--project-clean-p (repo-path)
  "Return t if there are no uncommitted modifications in project.
REPO-PATH is the absolute path of the repo to check."
  (and (string-prefix-p treebundel-workspace-root repo-path)
       (length= (split-string
                 (treebundel--git-with-repo repo-path
                   "status" "-z" "--porcelain")
                 "\0"
                 t)
                0)))

;;;;; Workspaces
(defun treebundel-workspace-path (name)
  "Return the path of a workspace named NAME."
  (file-name-concat treebundel-workspace-root name))

(defun treebundel--workspace-projects (&optional workspace)
  "Return a list of absolute paths to projects in WORKSPACE."
  (thread-last (directory-files (treebundel-workspace-path (or workspace (treebundel-current-workspace))) t "\\`[^\\.]")
               (seq-filter #'file-directory-p)
               (seq-map (lambda (path) (file-name-nondirectory path)))))

(defun treebundel--workspaces ()
  "Return a list of all existing workspace names."
  (seq-map #'file-name-nondirectory
           (seq-filter #'file-directory-p
                       (directory-files treebundel-workspace-root t "\\`[^.].*"))))

(defun treebundel--workspace-of (file-path)
  "Return the name of the current workspace.
If FILE-PATH is non-nil, use the current buffer instead."
  (let* ((file-path (expand-file-name file-path))
         (workspace nil))
    ;; Traverse up parent directories until the workspace root is all that remains
    (while (string-prefix-p treebundel-workspace-root (directory-file-name file-path))
      (setq workspace (file-name-nondirectory (directory-file-name file-path)))
      (setq file-path (file-name-directory (directory-file-name file-path))))
    workspace))

(defun treebundel-current-workspace (&optional file-path)
  "Return the name of the current workspace.
If FILE-PATH is non-nil, use the current buffer instead."
  (when-let* ((file-path (or file-path buffer-file-name default-directory)))
    (treebundel--workspace-of file-path)))

;;;; User Interface

;; This section provides the stable user interface.

;;;;; Scope

(defclass treebundel-scope ()
  ((workspace :initarg :workspace
              :initform nil
              :type (or string null))
   (project :initarg :project
            :initform nil
            :type (or string null)))
  "Data in the scope of treebundel transients.
This class is the only class used for the scopes of these transients

There are 3 valid states an instance of this class should only be in.

1. Workspace scope
   :workspace is a string with a length more than 0
   :project   is nil

2. Project scope
   :workspace is a string with a length more than 0
   :project   is a string with a length more than 0

3. Bare scope
   :workspace is `treebundel-bare-dir'
   :project   is a string that ends in `.git'

An instance of `treebundel-scope' with `:workspace' set to `treebundel-bare-dir'
means it represents a bare directory rather than a project directory.")

(cl-defmethod treebundel-scope-bare-p ((scope treebundel-scope))
  "Return t if the SCOPE represents a bare directory."
  (and (string= treebundel-bare-dir (oref scope workspace))
       (string-suffix-p ".git" (oref scope project))))

;; (treebundel-scope-workspace-p (treebundel-scope))
(cl-defmethod treebundel-scope-workspace-p ((scope treebundel-scope))
  "Return t if the SCOPE represents a workspace directory."
  (and (oref scope workspace)
       (not (string= treebundel-bare-dir (oref scope workspace)))
       (not (string-prefix-p "." (oref scope workspace)))))

(cl-defmethod treebundel-scope-project-p ((scope treebundel-scope))
  "Return t if the SCOPE represents a workspace and project directory."
  (and (treebundel-scope-workspace-p scope)
       (length> (oref scope project) 0)))

(cl-defmethod treebundel-scope-bare ((scope treebundel-scope))
  "The bare name if the `treebundel-scope' SCOPE represents or has bare directory."
  (cond ((treebundel-scope-bare-p scope)
         (oref scope project))
        ((treebundel-scope-project-p scope)
         (treebundel--repo-bare (treebundel-project-path (oref scope workspace) (oref scope project))))
        (t (treebundel--error "This is not treebundel-managed project"))))

(cl-defmethod treebundel-scope-fmt ((scope treebundel-scope) &key workspace-state project-state &allow-other-keys)
  "Format the text of the `treebundel-scope' SCOPE."
  (cond ((treebundel-scope-project-p scope)
         (treebundel--fmt-workspace-project (oref scope workspace) (oref scope project)
                                            :workspace-state workspace-state
                                            :project-state project-state))
        ((treebundel-scope-workspace-p scope)
         (treebundel--fmt-workspace-project (oref scope workspace) (oref scope project)
                                            :workspace-state workspace-state
                                            :project-state project-state))
        ((treebundel-scope-bare-p scope)
         (treebundel--fmt-bare (oref scope project)
                               project-state))))

(cl-defmethod treebundel-scope-exists-p ((scope treebundel-scope))
  "Returns t directory at `treebundel-scope' SCOPE exists."
  (cond ((treebundel-scope-bare-p scope)
         (file-directory-p (treebundel--bare-path (oref scope project))))
        ((treebundel-scope-workspace-p scope)
         (file-directory-p (treebundel-workspace-path (oref scope workspace))))))

(cl-defmethod treebundel-scope-valid-p ((scope treebundel-scope))
  "Returns t if SCOPE is in a valid configuration.
Read `treebundel-scope' docstring for more information."
  (or (treebundel-scope-workspace-p scope)
      (treebundel-scope-bare-p scope)
      (treebundel-scope-project-p scope)))

;;;;; not-implemented
(defun treebundel--not-implemented ()
  "A placeholder command for unimplemented transient commands."
  (interactive)
  (treebundel--message "This command is not yet implemented"))

;;;;; Entrypoint
;;;###autoload(autoload 'treebundel "treebundel" nil t)
(transient-define-prefix treebundel (&optional workspace project)
  ""
  ["Quick"
   ("w" "Open in workspace" treebundel-open-workspace)
   ("p" "Open other project" treebundel-open-project :if (lambda () (treebundel-scope-workspace-p (transient-scope))))
   ("a" "Add project" treebundel-add-project :if (lambda () (treebundel-scope-workspace-p (transient-scope)))
    :description (lambda ()
                   (format "Add project to %s" (treebundel--fmt-workspace-project (oref (transient-scope) workspace) nil))))]

  ["Configure"
   ("W" "Workspace" treebundel-workspace
    :description (lambda () (treebundel--fmt-workspace-project (oref (transient-scope) workspace)
                                                               nil
                                                               :project-state 'inactive)))

   ("P" "Project" treebundel-project :if (lambda () (treebundel-scope-project-p (transient-scope)))
    :description (lambda () (treebundel-scope-fmt (transient-scope))))

   ("B" "Bare" treebundel-bare :if (lambda () (or (treebundel-scope-bare-p (transient-scope))
                                                  (treebundel-scope-project-p (transient-scope))))
    :description
    (lambda ()
      (format "Bare %s"
              (treebundel--fmt-bare (treebundel--repo-bare (treebundel--project-path
                                                            (oref (transient-scope) workspace)
                                                            (oref (transient-scope) project)))))))]

  ["Debug" :level 6
   ("l" "Log" treebundel--debug-gitlog)
   ("c" "Clear scope" treebundel--debug-clear-scope)]

  (interactive (let* ((scope (or (transient-scope) (treebundel-scope :workspace (treebundel-current-workspace) :project (treebundel-current-project)))))
                 (list (oref scope workspace) (oref scope project))))
  (transient-setup 'treebundel nil nil :scope (treebundel-scope :workspace workspace :project project)))

;;;;; Bare
(transient-define-prefix treebundel-bare (bare)
  "Prefix for working with bare repositories."
  [("c" "Clone new" treebundel-clone-bare)
   ("B" "Switch to other bare" treebundel-switch-bare)]

  [:description
   (lambda () (treebundel--fmt-bare (oref (transient-scope) project) 'active))

   ("P" (lambda ()
          (let ((use-count (length (cdr (treebundel--worktree-list (treebundel--bare-path (oref (transient-scope) project)))))))
            (format "Projects (%s)" (propertize (format "%d" use-count) 'face 'treebundel-project))))
    treebundel-open-bare-projects)

   ;; Open a file in this bare's directory
   ("v" "Visit" treebundel-visit-bare)

   ("k" "Delete" treebundel-delete-bare
    :description (lambda ()
                   (if-let* ((use-count (length (cdr (treebundel--worktree-list (treebundel--bare-path (oref (transient-scope) project))))))
                             ((> use-count 0)))
                       (propertize "Delete" 'face 'treebundel-disabled)
                     "Delete")))

   ;; TODO Git-fetch to update bare
   ("f" "Fetch" treebundel--not-implemented
    :description (lambda () (propertize "Fetch" 'face 'treebundel-disabled)))]

  (interactive (list (cond ((treebundel-scope-bare-p (transient-scope))
                            (transient-scope))
                           ((treebundel-scope-project-p (transient-scope))
                            (treebundel--repo-bare (treebundel--project-path (oref (transient-scope) workspace)
                                                                             (oref (transient-scope) project))))
                           ((treebundel-read-bare)))))
  (transient-setup 'treebundel-bare nil nil :scope (treebundel-scope
                                                    :workspace treebundel-bare-dir
                                                    :project bare)))

(transient-define-suffix treebundel-switch-bare (bare)
  "Start configuring BARE."
  :transient 'transient--do-exit
  (interactive (list (treebundel-read-bare)))
  (transient-setup transient-current-command nil nil :scope (treebundel-scope
                                                             :workspace treebundel-bare-dir
                                                             :project (file-name-nondirectory (treebundel--bare-path bare)))))

(transient-define-suffix treebundel-clone-bare (url)
  "Clone URL to the collection of bare repos.
Once a repository is in the bare repos collection, you can add it to a project
with `treebundel-add-project'"
  (interactive
   (list (read-string "URL: " (or (treebundel--git-url-like-p (gui-get-selection 'CLIPBOARD 'STRING))
                                  (treebundel--git-url-like-p (gui-get-selection 'PRIMARY 'STRING))))))
  (treebundel--message "Cloning %s..." url)
  (let ((bare (string-remove-suffix ".git"
                                    (file-name-nondirectory
                                     (directory-file-name
                                      (treebundel--bare-clone url))))))
    (treebundel--message "Finished cloning %s." bare)
    (transient-setup transient-current-command nil nil :scope (treebundel-scope
                                                               :workspace treebundel-bare-dir
                                                               :project (file-name-nondirectory (treebundel--bare-path bare))))))
(defalias 'treebundel-clone #'treebundel-clone-bare)

(transient-define-suffix treebundel-delete-bare (bare)
  "Delete a bare repository BARE.
Existing worktrees or uncommitted changes will prevent you from deleting."
  (interactive (list (when (and (treebundel-scope-exists-p (transient-scope))
                                (treebundel-scope-bare-p (transient-scope)))
                       (oref (transient-scope) project))))
  (when-let* ((bare-path (treebundel--bare-path bare)))
    (cond ((treebundel--has-worktrees-p bare-path)
           (treebundel--error "This bare has projects attached to it"))

          ((treebundel--bare-unpushed-commits-p bare)
           (treebundel--error "This bare has unpushed commits"))

          (t (treebundel--bare-delete bare)))))

(transient-define-suffix treebundel-fetch-bare ()
  "Perform a git-fetch on bare repo.
BARE is the name of the bare repo to fetch.

This command is normally not useful unless `treebundel-fetch-on-add' is
disabled.  Use this command to manually control when git-fetch operations are
performed."
  (interactive)
  (when-let* ((bare (treebundel--repo-bare (treebundel-project-path)))
              (bare-path (treebundel--bare-path bare))
              ((file-exists-p bare-path)))
    (treebundel--message "Fetching...")
    (treebundel--git-with-repo bare "fetch")
    (treebundel--message "%s updated" bare)))

(transient-define-suffix treebundel-visit-bare (bare)
  "Find a file in the bare repository at BARE-CONS.
BARE-CONS is `(treebundel-bare-dir . bare-name)'. This is because it follows a similar
pattern to the project cons that are `(workspace . project)'."
  (interactive (list (cond ((treebundel-scope-bare-p (transient-scope))
                            (oref (transient-scope) project))
                           ((and (treebundel-scope-project-p (transient-scope)))
                            (treebundel--repo-bare (treebundel--project-path (oref (transient-scope) workspace)
                                                                             (oref (transient-scope) project))))
                           ((treebundel-read-bare)))))
  (find-file (treebundel--bare-path bare)))

(transient-define-suffix treebundel-open-bare-projects (bare)
  ""
  :transient 'transient--do-exit
  (interactive (list (cond ((treebundel-scope-bare-p (transient-scope))
                            (oref (transient-scope) project))
                           ((treebundel-scope-project-p (transient-scope))
                            (treebundel--repo-bare (treebundel--project-path (oref (transient-scope) workspace)
                                                                             (oref (transient-scope) project))))
                           ((treebundel-read-bare)))))
  (let* ((bare-path (treebundel--bare-path bare))
         (worktrees (cdr (treebundel--worktree-list bare-path)))
         (worktree-paths (mapcar (lambda (worktree) (cadr (split-string (car worktree) " ")))
                                 worktrees))
         (candidates (mapcar (lambda (project-path)
                               (when-let* ((workspace (treebundel--workspace-of project-path))
                                           (project (treebundel--project-of project-path)))
                                 (cons (treebundel--fmt-workspace-project workspace project)
                                       (treebundel-scope :workspace workspace :project project))))
                             worktree-paths))
         (selection (completing-read (format "Open project of %s" (treebundel--fmt-bare bare))
                                     candidates
                                     nil
                                     t))
         (selected-scope (cdr (assoc selection
                                     candidates))))
    (transient-setup 'treebundel-project nil nil :scope selected-scope)))

(defvar treebundel--bare-history nil
  "The `completing-read' history `treebundel--bare-read'.")

(defun treebundel-read-bare (&optional prompt)
  "Interactively find the path of a bare.
PROMPT is the text prompt presented to the user in the minibuffer.

HISTORY"
  (interactive)
  (treebundel--bare-read (or prompt "Select bare: ") nil 'treebundel--bare-history))

;;;;; Projects
(transient-define-prefix treebundel-project (workspace project)
  "Working with a PROJECT."
  [("P" "Switch to other project" treebundel-switch-project)]

  [:description
   (lambda () (treebundel-scope-fmt (transient-scope) :project-state 'active))
   ("W" "Configure workspace" treebundel-workspace)
   ("B" "Configure bare" treebundel-bare
    :description
    (lambda ()
      (format "Bare %s"
              (treebundel--fmt-bare (treebundel--repo-bare (treebundel--project-path
                                                            (oref (transient-scope) workspace)
                                                            (oref (transient-scope) project)))))))]

  [("RET" "Open project" treebundel-open-project)
   ("f" "Open project file" (lambda () (interactive)
                              (when-let* ((project-current-directory-override (treebundel--project-path (oref (transient-scope) workspace)
                                                                                                        (oref (transient-scope) project))))
                                (project-find-file))))
   ("v" "Visit" treebundel-visit-project)
   ("k" treebundel-remove-project)
   ("m" "Move" treebundel-move-project)
   ("r" "Rename" treebundel-rename-project)]
  (interactive (let* ((workspace (or (oref (transient-scope) workspace)
                                     (treebundel-read-workspace nil :require-matchd)))
                      (project (or (oref (transient-scope) project)
                                   (treebundel-read-project workspace nil nil :require-match))))
                 (list workspace project)))
  (transient-setup 'treebundel-project nil nil :scope (treebundel-scope :workspace workspace :project project)))

(transient-define-suffix treebundel-switch-project (workspace project)
  "Switch to PROJECT in WORKSPACE."
  :transient 'transient--do-exit
  (interactive (if (treebundel-scope-workspace-p (transient-scope))
                   (let* ((workspace (oref (transient-scope) workspace)))
                     (list workspace (treebundel-read-project workspace nil nil :require-match)))
                 (list "test" "invalid")))
  (transient-setup transient-current-command nil nil :scope (treebundel-scope :workspace workspace :project project)))

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
  :transient 'transient--do-stack
  (interactive
   (when-let* ((workspace (oref (transient-scope) workspace))
               (bare (treebundel-read-bare))
               (project-branch (treebundel-read-branch (treebundel--bare-path bare)))
               (project (treebundel-read-project workspace "Project name: " bare)))
     (list workspace bare project project-branch)))
  (treebundel--project-add workspace
                           bare
                           project-branch
                           project))

(transient-define-suffix treebundel-remove-project (workspace project)
  "Remove PROJECT from workspace WORKSPACE.
There must be no changes in the project to remove it."
  :description (lambda ()
                 (if-let* ((workspace (oref (transient-scope) workspace))
                           (project (oref (transient-scope) project))
                           (project-path (treebundel--project-path workspace project)))
                     (if (treebundel--project-clean-p project-path)
                         (format "Remove %s"
                                 (propertize "(Clean)" 'face 'treebundel-success))
                       (format "%s %s"
                               (propertize "Remove" 'face 'treebundel-disabled)
                               (propertize "(Dirty)" 'face 'treebundel-error)))))
  (interactive (list (oref (transient-scope) workspace)
                     (oref (transient-scope) project)))
  (let* ((project-path (treebundel--project-path workspace project)))
    (if (and (treebundel--project-clean-p project-path)
             (treebundel--worktree-remove project-path))
        (treebundel--message "Removed %s" (treebundel--fmt-workspace-project workspace project))
      (treebundel--message "Cannot remove %s because the project is dirty"
                           (treebundel--fmt-workspace-project workspace project)))))

(transient-define-suffix treebundel-move-project (workspace project new-workspace)
  "Move a project from one workspace to another.
WORKSPACE is the name of the workspace that contains the project to be
moved.

PROJECT is name of the project to move to a new workspace.

NEW-WORKSPACE is the name of the workspace the project will be moved
into."
  (interactive
   (when-let* ((workspace (oref (transient-scope) workspace))
               (project (oref (transient-scope) project))
               (new-workspace (treebundel-read-workspace
                               (format "Move %s to: " (treebundel--fmt-workspace-project workspace project))
                               :require-match)))
     (list workspace project new-workspace)))
  (treebundel--project-move (treebundel--project-path workspace project)
                            (file-name-concat (treebundel-workspace-path new-workspace) project))
  (treebundel--message "Moved project %s -> %s"
                       (treebundel--fmt-workspace-project workspace project)
                       (treebundel--fmt-workspace-project new-workspace project)))

(transient-define-suffix treebundel-rename-project (project new-name)
  "Rename a project.
WORKSPACE is the name of the workspace that contains the project to be
renamed.

PROJECT is name of the project in WORKSPACE to be renamed.

NEW-NAME is the new name PROJECT will be renamed to."
  (interactive
   (when-let* ((workspace (oref (transient-scope) workspace))
               (project (oref (transient-scope) project))
               ((treebundel-scope-project-p workspace project)))
     (list project (read-string "New name: " project))))
  (when-let* ((workspace (treebundel-current-workspace)))
    (treebundel--project-move (treebundel--project-path workspace project)
                              (treebundel--project-path workspace new-name))
    (treebundel--message "Renamed project from %s to %s"
                         (treebundel--fmt-workspace-project workspace project)
                         (treebundel--fmt-workspace-project workspace new-name))))

(transient-define-suffix treebundel-open-project (workspace project)
  "Switch to and focus a PROJECT by opening a file."
  (interactive (let* ((workspace (or (oref (transient-scope) workspace) (treebundel-current-workspace))))
                 (list workspace (treebundel-read-project workspace nil nil :require-match))))
  (transient-setup 'treebundel-open-project nil nil :scope (treebundel-scope :workspace workspace :project project))
  (treebundel--project-open workspace project))

(transient-define-suffix treebundel-visit-project (workspace project)
  "Open dired to PROJECT within the current workspace."
  (interactive (if-let* ((scope (transient-scope))
                         ((treebundel-scope-project-p scope))
                         (workspace (oref scope workspace))
                         (project (oref scope project)))
                   (list workspace project)
                 (list (treebundel-current-workspace) (treebundel-current-project))))
  (funcall-interactively #'find-file (treebundel--project-path workspace project)))

(defvar treebundel--project-history nil
  "The `completing-read' history `treebundel-read-project'.")

(defun treebundel-read-project (workspace &optional prompt initial require-match)
  "Interactively find the path of a project.
WORKSPACE is the workspace to look for projects in.

PROMPT is the prompt to be presented to the user in the
minibuffer.

INITIAL is the default value for the name of the project that is
automatically inserted when the minibuffer prompt is shown.

REQUIRE-MATCH forces a valid workspace to be selected.  This removes
the ability to create a workspace with a new entry."
  (let* ((candidates (treebundel--workspace-projects workspace)))
    (completing-read (or prompt (format "Project in %s" (treebundel--fmt-workspace workspace)))
                     candidates
                     nil
                     require-match
                     initial
                     treebundel--project-history)))

(defun treebundel-read-branch (repo-path &optional prompt initial)
  "Interactively selected a branch for a repo.
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
                                (treebundel--repo-bare repo-path)))))

;;;;; Workspaces
(transient-define-prefix treebundel-workspace (workspace)
  "Working with a workspace."
  [("W" "Switch to other workspace" treebundel-switch-workspace)]

  [:description
   (lambda () (treebundel-scope-fmt (transient-scope) :workspace-state 'active))
   ("P" "Configure project" treebundel-project :transient transient--do-exit)]

  [("a" "Add project" treebundel-add-project)
   ("k" "Delete" treebundel-delete-workspace)
   ("m" "Rename" treebundel--not-implemented
    :description  (lambda () (propertize "Rename (not implemented)" 'face 'treebundel-disabled)))]

  (interactive (list (or (when (treebundel-scope-workspace-p (transient-scope))
                           (oref (transient-scope) workspace))
                         (treebundel-read-workspace nil :require-match))))
  (transient-setup 'treebundel-workspace nil nil :scope (treebundel-scope :workspace workspace :project nil)))

(transient-define-suffix treebundel-switch-workspace (workspace)
  "Switch to WORKSPACE."
  :transient 'transient--do-exit
  (interactive (list (treebundel-read-workspace nil :require-match)))
  (transient-setup transient-current-command nil nil :scope (treebundel-scope :workspace workspace :project nil)))

(transient-define-suffix treebundel-open-workspace (workspace project)
  "Switch to a workspace and open a project within it.
This will always prompt for a workspace.  If you want to prefer your
current workspace, use `treebundel-open-project'.

WORKSPACE is the name of the workspace to open.

PROJECT is the name of the project within the workspace to open."
  (interactive (let* ((workspace (treebundel-read-workspace nil :require-match))
                      (project (treebundel-read-project workspace nil nil :require-match)))
                 (list workspace project)))
  (treebundel--project-open workspace project))
(defalias 'treebundel-open #'treebundel-open-workspace)

(transient-define-suffix treebundel-delete-workspace (workspace)
  "Delete workspace at WORKSPACE.
This will check if all projects within the workspace are clean and if so, remove
everything in the workspace. Anything committed is still saved in the respective
projects' bare repository located at `treebundel-bare-dir' within
`treebundel-workspace-root'."
  (interactive (list (oref (transient-scope) workspace)))
  (when-let* ((workspace (or workspace (treebundel-read-workspace "Delete workspace: " :require-match)))
              (workspace-path (treebundel-workspace-path workspace))
              (project-paths (directory-files workspace-path t "\\`[^.].*")))
    (let* ((ignore-errors (transient-arg-value "--force" workspace)))
      (if (and (seq-every-p (lambda (project-path)
                              (treebundel--project-clean-p project-path))
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
            (treebundel--message "Deleted workspace %s" (treebundel--fmt-workspace workspace)))
        (user-error "There must not be any unsaved changes to delete a workspace")))))

(defvar treebundel--workspace-history nil
  "The `completing-read' history `treebundel-read-workspace'.")

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
  (let* ((candidates (treebundel--workspaces))
         (prompt (or prompt "Workspace: ")))
    (completing-read prompt candidates nil require-match nil treebundel--workspace-history)))

;;;;; Debug
(transient-define-suffix treebundel--debug-gitlog ()
  ""
  (interactive)
  (display-buffer (treebundel--gitlog-buffer)))

(transient-define-suffix treebundel--debug-clear-scope ()
  (interactive)
  (transient-setup transient-current-command nil nil :scope (treebundel-scope :workspace nil :project nil)))

(provide 'treebundel)
;;; treebundel.el ends here
