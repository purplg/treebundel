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
  (format "%s" (propertize (or bare "⸺")
                           'face 'treebundel-bare)))

(defun treebundel--fmt-workspace (&optional workspace)
  "Format the text of a WORKSPACE name."
  (concat (propertize (or workspace "⸺") 'face 'treebundel-workspace)
          "/"))

(defun treebundel--fmt-project (&optional project)
  "Format the text of a PROJECT name."
  (propertize (or project "⸺") 'face 'treebundel-project))

(defun treebundel--fmt-workspace-project (&optional workspace project)
  "Format the text of a WORKSPACE and PROJECT pair."
  (cond ((eq (type-of workspace) 'cons)
         (concat (treebundel--fmt-workspace (car workspace)) (treebundel--fmt-project (cdr workspace))))
        (t
         (concat (treebundel--fmt-workspace workspace) (treebundel--fmt-project project)))))

;;;; Workspace management

;; These functions provide useful functions for and the rules to enforce the
;; definitions of the terminology at the top of this package.

;;;;; Repos
(defun treebundel--repo-bare (repo-path)
  "Return the name of the bare repo related to REPO-PATH."
  (let ((bare-name (thread-first (treebundel--git-with-repo repo-path
                                   "rev-parse" "--path-format=absolute" "--git-common-dir")
                                 (directory-file-name)
                                 (file-name-base))))
    bare-name))

;;;;; Bares
(defun treebundel-bare-path (bare)
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
  (treebundel--project-current
   (treebundel--worktree-add bare
                             (treebundel--project-path workspace (or project bare))
                             (or branch-name (treebundel--branch-name workspace)))))

(defun treebundel--project-current (&optional file-path)
  "Return the project name of FILE-PATH.
If FILE-PATH is non-nil, use the current buffer."
  (when-let* ((file-path (or file-path buffer-file-name default-directory))
              (workspace (treebundel-current-workspace file-path))
              (workspace-path (treebundel-workspace-path workspace))
              (relative-path (when (string-prefix-p workspace-path file-path)
                               (string-remove-prefix workspace-path file-path)))
              (parts (split-string relative-path "/" :omit-empty)))
    (car parts)))

(defun treebundel--project-move (src-path dst-path)
  "Move a repo from SRC-PATH to DST-PATH."
  (treebundel--git-with-repo (treebundel-bare-path (treebundel--repo-bare src-path))
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
            (project (or project (treebundel--project-current)))
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

(defun treebundel-current-workspace (&optional file-path)
  "Return the name of the current workspace.
If FILE-PATH is non-nil, use the current buffer instead."
  (let* ((file-path (or (when file-path (expand-file-name file-path))
                        default-directory
                        buffer-file-name))
         (workspace nil))
    ;; Traverse up parent directories until the workspace root is all that remains
    (while (string-prefix-p treebundel-workspace-root (directory-file-name file-path))
      (setq workspace (file-name-nondirectory (directory-file-name file-path)))
      (setq file-path (file-name-directory (directory-file-name file-path))))
    workspace))

;;;; User Interface

;; This section provides the stable user interface.

;;;;; not-implemented
(defun treebundel--not-implemented ()
  "A placeholder command for unimplemented transient commands."
  (interactive)
  (treebundel--message "This command is not yet implemented"))

;;;;; Entrypoint
;;;###autoload(autoload 'treebundel "treebundel" nil t)
(transient-define-prefix treebundel (&optional workspace project)
  ""
  [:description "Quick"
   ("w" "Open in workspace" treebundel-open-in-workspace)
   ("p" "Open other project" (lambda ()
                               (interactive)
                               (when-let* ((workspace (car (transient-scope)))
                                           (project (treebundel-read-project workspace)))
                                 (treebundel-open-project workspace project))))
   ("a" "Add project" treebundel-add-project :if (lambda () (car (transient-scope)))
    :description (lambda ()
                   (format "Add project to %s" (treebundel--fmt-workspace (or (car (transient-scope))
                                                                              (treebundel-current-workspace))))))]

  ["Configure"
   ("W" "Workspace" treebundel-workspace
    :description (lambda () (treebundel--fmt-workspace-project (or (car (transient-scope)) (treebundel-current-workspace)))))

   ("P" "Project" treebundel-project :if (lambda () (cdr (transient-scope)))
    :description (lambda () (treebundel--fmt-workspace-project (transient-scope))))

   ("B" "Bare" treebundel-bare :if (lambda () (cdr (transient-scope)))
    :description
    (lambda ()
      (format "Bare %s"
              (treebundel--fmt-bare (treebundel--repo-bare (treebundel--project-path
                                                            (car (transient-scope))
                                                            (cdr (transient-scope))))))))]

  ["Debug" :level 6
   ("l" "Log" treebundel-open-gitlog)]
  (interactive (if-let* ((scope (transient-scope)))
                   (list (car scope) (cdr scope))
                 (list (treebundel-current-workspace) (treebundel--project-current))))
  (transient-setup 'treebundel nil nil :scope (cons workspace project)))

;;;;; Bare
(transient-define-prefix treebundel-bare (bare)
  "Prefix for working with bare repositories."
  [("c" "Clone new" treebundel-clone-bare)
   ("b" "Switch to other bare" treebundel-switch-bare)]

  [:description
   (lambda () (format "Configuring %s" (treebundel--fmt-bare (cdr (transient-scope)))))
   ("k" "Delete" treebundel-delete-bare
    :description (lambda ()
                   (if-let* ((use-count (length (cdr (treebundel--worktree-list (treebundel-bare-path (cdr (transient-scope)))))))
                             ((> use-count 0)))
                       (format "%s (in use by %s projects)"
                               (propertize "Delete" 'face 'treebundel-disabled)
                               (propertize (format "%d" use-count) 'face 'transient-argument))
                     "Delete")))

   ("p" "Projects" treebundel-open-bare-projects)

   ;; Open a file in this bare's directory
   ("v" "Visit" treebundel-visit-bare)

   ;; TODO Git-fetch to update bare
   ("f" "Fetch" treebundel--not-implemented
    :description (lambda () (propertize "Fetch" 'face 'treebundel-disabled)))]
  (interactive
   (list (cond ((string= treebundel-bare-dir (car (transient-scope)))
                (cdr (transient-scope)))
               ((and (car (transient-scope)) (cdr (transient-scope)))
                (treebundel--repo-bare (treebundel--project-path (car (transient-scope)) (cdr (transient-scope)))))
               ((treebundel-read-bare)))))
  (transient-setup 'treebundel-bare nil nil :scope (cons treebundel-bare-dir bare)))

(transient-define-suffix treebundel-switch-bare (bare)
  "Start configuring BARE."
  :transient 'transient--do-exit
  (interactive (list (treebundel-read-bare)))
  (transient-setup transient-current-command nil nil :scope (cons treebundel-bare-dir bare)))

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
    (treebundel--message "Finished cloning %s." bare-name)
    (transient-setup transient-current-command nil nil :scope (cons treebundel-bare-dir bare-name))))
(defalias 'treebundel-clone #'treebundel-clone-bare)

(transient-define-suffix treebundel-delete-bare (bare)
  "Delete a bare repository BARE.
Existing worktrees or uncommitted changes will prevent you from deleting."
  (interactive (list (and (string= treebundel-bare-dir (car (transient-scope)))
                          (cdr (transient-scope)))))
  (when-let* ((bare-path (treebundel-bare-path bare)))
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
  (let ((bare (treebundel-bare-path)))
   (treebundel--message "Fetching...")
   (treebundel--git-with-repo bare "fetch")
   (treebundel--message "%s updated" bare)))

(transient-define-suffix treebundel-visit-bare (bare)
  "Find a file in the bare repository at BARE-CONS.
BARE-CONS is `(treebundel-bare-dir . bare-name)'. This is because it follows a similar
pattern to the project cons that are `(workspace . project)'."

  (interactive (list (cond
                      ;; if the workspace is `treebundel-bare-dir' (e.g.
                      ;; ".bare"), then it's already a bare repo
                      ((string= (car (transient-scope)) treebundel-bare-dir)
                       (cdr (transient-scope)))

                      ;; otherwise, it's a workspace directory so look up the
                      ;; bare
                      ((car (transient-scope))
                       (treebundel--repo-bare (treebundel--project-path (car (transient-scope))
                                                                        (cdr (transient-scope)))))

                      ;; No idea at this point. Just prompt user to select the bare.
                      (t (treebundel-read-bare)))))
  (find-file (treebundel-bare-path bare)))

(transient-define-suffix treebundel-open-bare-projects (bare)
  ""
  :transient 'transient--do-exit
  (interactive (list (cond ((string= (car (transient-scope)) treebundel-bare-dir)
                            (cdr (transient-scope)))
                           ((car (transient-scope))
                            (treebundel--repo-bare (treebundel--project-path (car (transient-scope)) (cdr (transient-scope)))))
                           ((treebundel-read-bare)))))
  (when bare
    (let* ((candidates (thread-last bare
                                    (treebundel-bare-path)
                                    (treebundel--worktree-list)
                                    (cdr)
                                    (mapcar (lambda (worktree) (cadr (split-string (car worktree) " "))))
                                    (mapcar (lambda (project-path)
                                              (cons (treebundel--fmt-workspace-project
                                                     (treebundel-current-workspace project-path)
                                                     (treebundel--project-current project-path))
                                                    project-path)))))
           (selected-path (cdr (assoc (completing-read (format "Open project of %s" (treebundel--fmt-bare bare))
                                                      candidates
                                                      nil
                                                      t)
                                     candidates))))
      (transient-setup 'treebundel-project nil nil :scope (cons (treebundel-current-workspace selected-path) (treebundel--project-current selected-path))))))

(defun treebundel-read-bare (&optional prompt)
  "Interactively find the path of a bare.
PROMPT is the text prompt presented to the user in the minibuffer."
  (interactive)
  (treebundel--bare-read (or prompt "Select bare: ") nil nil))

;;;;; Projects
(transient-define-prefix treebundel-project (workspace project)
  "Working with a PROJECT."
  [:description
   (lambda () (format "Configuring %s" (treebundel--fmt-workspace-project (transient-scope))))
   ("p" "Switch to other project" treebundel-switch-project)]

  [("RET" "Open project" treebundel-open-project)
   ("f" "Open project file" (lambda () (interactive)
                              (when-let* ((project-current-directory-override (treebundel--project-path (car (transient-scope))
                                                                                                        (cdr (transient-scope)))))
                                (project-find-file))))
   ("v" "Visit" treebundel-visit-project)
   ("k" treebundel-remove-project)
   ("m" "Move" treebundel-move-project)
   ("r" "Rename" treebundel-rename-project)]
  (interactive (when-let* ((workspace (or (car (transient-scope)) (treebundel-current-workspace)))
                           (project (or (cdr (transient-scope)) (treebundel-read-project workspace))))
                 (list workspace project)))
  (transient-setup 'treebundel-project nil nil :scope (cons workspace project)))

(transient-define-suffix treebundel-switch-project (workspace project)
  "Start configuring PROJECT in WORKSPACE."
  :transient 'transient--do-exit
  (interactive (when-let* ((workspace (or (car (transient-scope))))
                           (project (treebundel-read-project workspace nil nil t)))
                 (list workspace project)))
  (transient-setup transient-current-command nil nil :scope (cons workspace project)))

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
   (when-let* ((workspace (car (transient-scope)))
               (bare (treebundel-read-bare))
               (project-branch (treebundel-read-branch (treebundel-bare-path bare)))
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
                 (if-let* ((workspace (car (transient-scope)))
                           (project (cdr (transient-scope)))
                           (project-path (treebundel--project-path workspace project)))
                     (if (treebundel--project-clean-p project-path)
                         (format "Remove %s"
                                 (propertize "(Clean)" 'face 'treebundel-success))
                       (format "%s %s"
                               (propertize "Remove" 'face 'treebundel-disabled)
                               (propertize "(Dirty)" 'face 'treebundel-error)))))
  (interactive (list (car (transient-scope))
                     (cdr (transient-scope))))
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
   (when-let* ((workspace (car (transient-scope)))
               (project (cdr (transient-scope)))
               (new-workspace (treebundel-read-workspace (format "Move %s to: " (treebundel--fmt-workspace-project workspace project)) t)))
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
   (when-let* ((project (or (cdr (transient-scope)) (treebundel--project-current))))
     (list project (read-string "New name: " project))))
  (when-let* ((workspace (treebundel-current-workspace)))
    (treebundel--project-move (treebundel--project-path workspace project)
                              (treebundel--project-path workspace new-name))
    (treebundel--message "Renamed project from %s to %s"
                         (treebundel--fmt-workspace-project workspace project)
                         (treebundel--fmt-workspace-project workspace new-name))))

(transient-define-suffix treebundel-open-project (workspace project)
  "Switch to and focus a PROJECT by opening a file."
  :transient 'transient--do-exit
  (interactive (list (car (transient-scope))
                     (cdr (transient-scope))))
  (treebundel--project-open workspace project))

(transient-define-suffix treebundel-visit-project (workspace project)
  "Open dired to PROJECT within the current workspace."
  (interactive (if (transient-scope)
                   (list (car (transient-scope)) (cdr (transient-scope)))
                 (list (treebundel-current-workspace) (treebundel--project-current))))
  (funcall-interactively #'find-file (treebundel--project-path workspace project)))

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
                     initial)))

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

;;;;; Log
(transient-define-suffix treebundel-open-gitlog ()
  ""
  (interactive)
  (display-buffer (treebundel--gitlog-buffer)))

;;;;; Workspaces
(transient-define-prefix treebundel-workspace (workspace)
  "Working with a workspace."
  [:description
   (lambda () (format "Configuring %s" (treebundel--fmt-workspace-project (car (transient-scope)))))
   ("w" "Switch to other workspace" treebundel-switch-workspace)]

  [("p" "Configure project" treebundel-project)
   ("a" "Add project" treebundel-add-project)
   ("k" "Delete" treebundel-delete-workspace)
   ("m" "Rename" treebundel--not-implemented
    :description  (lambda () (propertize "Rename (not implemented)" 'face 'treebundel-disabled)))]

  (interactive (list (or (car (transient-scope))
                         (treebundel-current-workspace)
                         (treebundel-read-workspace nil t))))
  (transient-setup 'treebundel-workspace nil nil :scope (cons workspace nil)))

(transient-define-suffix treebundel-switch-workspace (workspace)
  "Switch to WORKSPACE."
  :transient 'transient--do-exit
  (interactive (list (treebundel-read-workspace nil t)))
  (transient-setup transient-current-command nil nil :scope (cons workspace nil)))

(transient-define-suffix treebundel-open-in-workspace ()
  "Switch to a workspace and open a project within it.
This will always prompt for a workspace.  If you want to prefer your
current workspace, use `treebundel-switch-project'.

WORKSPACE is the name of the workspace to open.

PROJECT is the name of the project within the workspace to open."
  (interactive)
  (when-let* ((workspace (treebundel-read-workspace nil t))
              (project (treebundel-read-project workspace nil nil t)))
    (treebundel-open-project workspace project)))
(defalias 'treebundel-open #'treebundel-open-in-workspace)

(transient-define-suffix treebundel-delete-workspace (workspace)
  "Delete workspace at WORKSPACE.
This will check if all projects within the workspace are clean and if so, remove
everything in the workspace. Anything committed is still saved in the respective
projects' bare repository located at `treebundel-bare-dir' within
`treebundel-workspace-root'."
  (interactive (list (car (transient-scope))))
  (when-let* ((workspace (or workspace (treebundel-read-workspace "Delete workspace: " t)))
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
