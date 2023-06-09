;;; treebund.el --- Bundle related git-worktrees together -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1.0
;; Author: Ben Whitley
;; Homepage: https://github.com/purplg/treebund.el
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This package is used for bundling related git-worktrees from multiple
;; repositories together. This helps switch quickly between repositories and
;; ensure you're on the correct branch. When you're done with your changes, you
;; can use the repositories in the workspace and know which ones were modified
;; to simplify the process of getting the changes merged in together.
;;
;; Additionally, git metadata (the =.git= directory) is shared between all
;; projects. You can stash, pop, and pull changes in from the same repository in
;; other workspaces thanks to the power of git-worktrees.


;;;; Terminology:

;; Workspace
;;   A collection of 'PROJECT's created from 'BARE's.
;;
;; Project
;;   A git-worktree checked out from a 'BARE' stored in a 'WORKSPACE'.
;;
;; Bare
;;   A bare repository used as a source to create a 'PROJECT's git-worktree.


;;;; Structure:

;; The workspaces directory is structured as such:
;;
;; `treebund-workspace-root' (default: "~/workspaces/")
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
;; open a worktree for a new branch called 'feature/<workspace-name>'.
;;
;; 1. Create a new workspace using `treebund-new-workspace'.
;; 2. Interactively call `treebund-add-project'.
;; 3. Select the newly created workspace.
;; 4. Select '[ clone ]'.
;; 5. Enter the remote URL for the repository to be added to the workspace.


;;;; Configuration:

;; `treebund-prefix'
;;  Default: 'feature/'
;;
;; This is probably the most subjective variable you'd want to customize. With
;; its default value, when you add a project to a workspace named, for example,
;; 'new-protocol', the new project will be checked out to a new branch called
;; 'feature/new-protocol'. Eventually, I'd like to make the prefix
;; workspace-specific, then this would variable just provide the default.

;; `treebund-workspace-root'
;;  Default: '~/workspaces/'
;;
;; This one is also very subjective. It's where all of your workspaces will
;; exist on your file-system.

;; `treebund-project-open-function'
;;  Default: `project-switch-project'
;;
;; This is the function called when a project is opened. You could also just
;; make this `find-file' to just open the file instantly or any other function
;; that takes a file path.

;; `treebund-before-project-open-functions'
;; `treebund-after-project-open-hook'
;; `treebund-before-workspace-open-functions'
;; `treebund-after-workspace-open-hook'
;;
;; These hooks are called before or after a project or workspace is opened. The
;; `-functions'-suffixed hooks take a single argument, which is the path to the
;; project directory or workspace directory to be opened.


;;;; Usage:

;; | Command                     | Description                                 |
;; |-----------------------------+---------------------------------------------|
;; | ~treebund-open~             | Open a project in a workspace               |
;; | ~treebund-open-project~     | Open other project within current workspace |
;; | ~treebund-add-project~      | Add a project to a workspace                |
;; | ~treebund-remove-project~   | Remove a project from a workspace           |
;; | ~treebund-delete-workspace~ | Delete a workspace                          |

;;; Code:
(eval-when-compile (require 'subr-x))
(require 'vc-git)


;;;; Customization

(defgroup treebund nil
  "Exploit git-worktrees to create inter-related project workspaces."
  :group 'convenience
  :prefix "treebund-")

(defcustom treebund-prefix "feature/"
  "The string prefix before every new project branch."
  :group 'treebund
  :type 'string)

(defcustom treebund-workspace-root "~/workspaces/"
  "The path where all workspaces are stored."
  :group 'treebund
  :type 'string
  :set (lambda (option value)
         (set option (file-name-as-directory (expand-file-name
                                              value)))))

(defcustom treebund-bare-dir (concat (file-name-as-directory treebund-workspace-root) ".bare")
  "The path where bare repositories are stored."
  :group 'treebund
  :type 'string
  :set (lambda (option value)
         (set option (file-name-as-directory (expand-file-name
                                              value)))))

(defcustom treebund-project-open-function #'project-switch-project
  "Function called to switch to a new project."
  :group 'treebund
  :type 'function)

;; Hooks
(defcustom treebund-before-project-open-functions nil
  "Hook which is run before a project is opened.
A single argument is passed which is the path to the project to
be opened."
  :group 'treebund
  :type '(list function))

(defcustom treebund-after-project-open-hook nil
  "Hook which is run after a project is opened."
  :group 'treebund
  :type 'hook)

(defcustom treebund-before-workspace-open-functions nil
  "Hook which is run before a workspace is opened.
A single argument is passed which is the path to the workspace to
be opened."
  :group 'treebund
  :type '(list function))

(defcustom treebund-after-workspace-open-hook nil
  "Hook which is run after a workspace is opened."
  :group 'treebund
  :type 'hook)


;;;; Logging
(defface treebund--gitlog-heading
  '((t (:inherit mode-line :extend t)))
  "Face for widget group labels in treebund's dashboard."
  :group 'treebund)

(defvar treebund--gitlog-buffer "*treebund-git*")

(defun treebund--gitlog-buffer ()
  "Return the git history log buffer for treebund."
  (or (get-buffer treebund--gitlog-buffer)
      (let ((buf (get-buffer-create treebund--gitlog-buffer)))
        (with-current-buffer buf
          (read-only-mode 1)
          (set (make-local-variable 'window-point-insertion-type) t))
        buf)))

(defun treebund--gitlog (type &rest msg)
  "Insert a message in the treebund git log buffer.
TYPE is the type of log message.  Can be either 'command or 'output.

MSG is the text to be inserted into the log."
  (with-current-buffer (treebund--gitlog-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (cond ((eq 'command type)
             (insert (propertize (string-join (append '("git") msg '("\n")) " ") 'face 'treebund--gitlog-heading)))
            ((eq 'output type)
             (let ((msg (apply #'format msg)))
               (when (= (length msg) 0)
                 (setq msg " "))
               (insert msg))
             (newline 2))))))


;;;; Git operations

;; Fundamental functions to performing fundamental git operations. The two macros
;; should be used only by the rest of the functions in this section. The rest
;; functions parse the git output into something more useful for this package.

(defmacro treebund--git (&rest args)
  "Base macro for all treebund git commands.
ARGS are the arguements passed to git."
  (declare (indent defun))
  `(with-temp-buffer
     (treebund--gitlog 'command (string-join (list ,@args) " "))
     (let ((result (vc-git--call t ,@args))
           (output (string-trim-right (buffer-string))))
       (treebund--gitlog 'output output)
       (when (> result 0)
         (user-error "Git command failed with error %s.  See %s" result treebund--gitlog-buffer))
       output)))

(defmacro treebund--git-with-repo (repo-path &rest args)
  "Run a command on a specific git repositorys.
REPO-PATH is the repository to pass to git with the '-C' switch.

ARGS are the arguments passed to git."
  (declare (indent defun))
  `(treebund--git "-C" ,repo-path ,@args))

(defun treebund--branches (repo-path &optional omit-main)
  "Return a list of branches for repository at REPO-PATH.
When OMIT-MAIN is non-nil, exclude the default branch."
  (let ((branches (split-string (treebund--git-with-repo repo-path
                                  "branch" "--format=%(refname:short)")
                                "\n")))
    (seq-remove #'string-empty-p
                (if omit-main
                    (let ((main-branch (treebund--branch-default repo-path)))
                      (seq-remove (lambda (branch) (equal main-branch branch)) branches))
                  branches))))

(defun treebund--worktree-bare (project-path)
  "Return the bare related to PROJECT-PATH."
  (treebund--git-with-repo project-path
    "rev-parse" "--path-format=absolute" "--git-common-dir"))

(defun treebund--worktree-remove (repo-path)
  "Remove the worktree at REPO-PATH."
  (let ((bare-path (treebund--worktree-bare repo-path)))
    (treebund--git-with-repo bare-path
      "worktree" "remove" repo-path)))

(defun treebund--worktree-add (bare-path worktree-path branch-name)
  "Create a worktree.
BARE-PATH is the main repository the worktree is being created from.

WORKTREE-PATH is the path where the new worktree will be created.

BRANCH-NAME is the name of branch to be created and checked out at PROJECT-PATH.

Returns the path to the newly created worktree."
  (if (member branch-name (treebund--branches bare-path))
      (treebund--git-with-repo bare-path
        "worktree" "add" worktree-path branch-name)
    (treebund--git-with-repo bare-path
      "worktree" "add" worktree-path "-b" branch-name))
  worktree-path)

(defun treebund--worktree-list (repo-path)
  "Return a list of worktrees for REPO-PATH."
  (seq-map
   (lambda (worktree)
     (split-string worktree "\0" t))
   (split-string (treebund--git-with-repo repo-path
                   "worktree" "list" "-z" "--porcelain")
                 "\0\0"
                 t)))

(defun treebund--branch (repo-path)
  "Return the branch checked out REPO-PATH."
  (treebund--git-with-repo repo-path
    "branch" "--show-current"))

(defun treebund--branch-delete (bare-path branch-name)
  "Delete a branch in a git repository.
BARE-PATH is the bare git repository to be acted on.

BRANCH-NAME is the branch to be deleted within this repository."
  (treebund--git-with-repo bare-path
    "branch" "-D" branch-name))

(defun treebund--clone (url)
  "Clone a repository from URL to DEST."
  (let* ((dest (concat (file-name-as-directory treebund-bare-dir)
                       (car (last (split-string url "/"))))))
    (when (file-exists-p dest)
      (user-error "Repostitory with this name is already cloned"))
    (treebund--git "clone" url "--bare" dest)
    (treebund--git-with-repo dest "config" "remote.origin.fetch" "+refs/heads/*:refs/remotes/origin/*")
    (treebund--git-with-repo dest "fetch")
    dest))

(defun treebund--rev-count (repo-path commit-a &optional commit-b)
  "Return the number of commits between COMMIT-A and COMMIT-B at REPO-PATH."
  (unless commit-b
    (setq commit-b commit-a)
    (setq commit-a (treebund--branch-default repo-path)))
  (string-to-number
   (treebund--git-with-repo repo-path
     "rev-list" (concat commit-a ".." commit-b) "--count")))


;;;; Internal

(define-error 'treebund-error "treebund error")

(defun treebund--error (format &rest args)
  (signal 'treebund-error (list (apply #'format-message format args))))

;; These functions provide useful functions for and the rules to enforce the
;; definitions of the terminology at the top of this package.

;; Repos
(defmacro treebund--with-repo (repo-path &rest body)
  "Run BODY in the context of a buffer in REPOSITORY-PATH.
BODY is evaluated with the context of a buffer in the REPO-PATH repository"
  (declare (indent defun))
  `(let* ((buffer (find-file-noselect ,repo-path))
          (result nil))
     (with-current-buffer buffer
       (setq result ,@body))
     (kill-buffer buffer)
     result))

(defun treebund--repo-worktree-count (repo-path)
  "Return the number of worktrees that exist for REPO-PATH."
  (seq-count
   (lambda (str) (not (member "bare" str)))
   (treebund--worktree-list repo-path)))

(defun treebund--has-worktrees-p (repo-path)
  "Return t if REPO-PATH has any worktrees."
  (> (treebund--repo-worktree-count repo-path) 0))

(defun treebund--unpushed-commits-p (repo-path &optional branches)
  "Return if there are any unpushed commits to remote.
REPO-PATH is the repository to be acted on.

If BRANCH is nil, check all local BRANCHES.

If BRANCH is a string or list of strings, only check these local branches."
  (when (eq 'string (type-of branches))
    (setq branches (list branches)))
  (setq repo-path (treebund--project-bare repo-path))
  (> (length (treebund--git-with-repo repo-path "log" "--branches" "--not" "--remotes")) 0))

;; Bares
(defun treebund--bare-name (bare-path)
  "Return the name of bare repository at BARE-PATH."
  (when (string-prefix-p treebund-bare-dir bare-path)
    (file-name-base (directory-file-name bare-path))))

(defun treebund--bare-delete (bare-path)
  "Delete the bare repository at BARE-PATH.
This will check to see if BARE-PATH exists within
`treebund-workspace-root' before deleting."
  (setq bare-path (file-name-as-directory bare-path))
  (unless (string-prefix-p treebund-workspace-root bare-path)
    (treebund--error "Bare not within workspace root"))
  (unless (string-suffix-p ".git/" bare-path)
    (treebund--error "Bare repository does not end in .git"))
  (delete-directory bare-path t))

(defun treebund--bare-list ()
  "Return a list of all existing bare repository directory names."
  (directory-files treebund-bare-dir nil "^[^.].*"))

;; Workspaces
(defun treebund--workspace-name (workspace-path)
  "Return the name of a workspace at WORKSPACE-PATH."
  (file-name-nondirectory (directory-file-name workspace-path)))

(defun treebund--workspace-projects (workspace-path)
  "Return a list of absoute paths to projects in WORKSPACE-PATH."
  (seq-filter #'file-directory-p
              (directory-files workspace-path t "^[^.].*")))

(defun treebund--workspaces ()
  "Return a list of all existing workspace names."
  (seq-map #'file-name-nondirectory
           (seq-filter #'file-directory-p
                       (directory-files treebund-workspace-root t "^[^.].*"))))

(defun treebund-current-workspace (&optional file-path)
  "Return the path to the current workspace.
If FILE-PATH is non-nil, use the current buffer instead."
  (when-let* ((file-path (or file-path buffer-file-name))
              (file-path (expand-file-name file-path))
              (workspace-root (file-name-as-directory treebund-workspace-root))
              ;; Ensure project is with the workspace path
              ((string-prefix-p workspace-root file-path))
              ;; Strip workspace-root from the path to make a relative path.
              (workspace-name (car (split-string (string-remove-prefix workspace-root
                                                                       file-path)
                                                 "/")))
              ;; Ensure workspace name is not empty
              ((not (string-empty-p workspace-name))))
    (file-name-as-directory (concat (file-name-as-directory workspace-root) workspace-name))))

;; Projects
(defun treebund--project-add (workspace-path bare-path &optional branch-name project-path)
  "Add a project to a workspace.
This function defines the way project worktrees are added and named in workspaces.

WORKSPACE-PATH is the directory to place the new worktree in.

BARE-PATH is the main repository the worktree is being created from.

BRANCH-NAME is the name of branch to be created and checked out in the workspace.

PROJECT-PATH is the name of the worktrees' directory in the workspace."
  (treebund--worktree-add bare-path
                          (or project-path
                              (concat (file-name-as-directory workspace-path)
                                      (treebund--bare-name bare-path)))
                          (or branch-name
                              (treebund--branch-name workspace-path))))

(defun treebund--project-bare (project-path)
  "Return the respective bare for project at PROJECT-PATH."
  (when-let ((worktrees (seq-find (lambda (worktree) (member "bare" worktree))
                                  (treebund--worktree-list project-path))))
    (cadr (split-string (car worktrees)))))

(defun treebund--project-current (&optional file-path)
  "Return the project path of FILE-PATH.
If FILE-PATH is non-nil, use the current buffer."
  (when-let* ((file-path (or file-path buffer-file-name))
              (file-path (expand-file-name file-path))
              (workspace-path (treebund-current-workspace file-path))
              (relative-path (string-remove-prefix workspace-path file-path))
              ((not (string-empty-p relative-path)))
              (project-parts (split-string relative-path "/")))
    (when-let ((project-name (pop project-parts)))
      (concat workspace-path project-name))))

(defun treebund--project-name (file-path)
  "Return the name of project at PROJECT-PATH."
  (when-let (((string-prefix-p treebund-workspace-root file-path))
             (project-path (treebund--project-current file-path)))
    (file-name-nondirectory (directory-file-name file-path))))

;; Branches
(defun treebund--branch-name (workspace-path)
  "Generate a branch name for WORKSPACE-PATH."
  (concat treebund-prefix (treebund--workspace-name workspace-path)))

(defun treebund--branch-default (repo-path)
  "Return the default branch at REPO-PATH."
  (treebund--with-repo repo-path
    (car (vc-git-branches))))


;;;; User functions

;; This section provides the stable user interface.

(defun treebund--read-bare (&optional prompt clone omit)
  "Interactively find the path of a bare.
PROMPT is the text prompt presented to the user in the minibuffer.

When CLONE is non-nil, add a completion candidate named this
'[ clone ]' which will prompt the user to clone a new repository
instead.

When OMIT is non-nil, it should be a list of a candidates to be
excluded from the candidates."
  (let* ((candidates (mapcar (lambda (bare)
                               (cons (replace-regexp-in-string "\.git$" "" bare)
                                     (concat (file-name-as-directory treebund-bare-dir) bare)))
                             (treebund--bare-list))))
    (when omit
      (setq candidates (seq-remove (lambda (bare) (member (car bare) omit))
                                   candidates)))
    (when clone
      (setq candidates (append candidates '(("[ clone ]" . clone)))))
    (if-let ((selection (cdr (assoc (completing-read (or prompt "Select project: ") candidates) candidates))))
        (if (equal selection 'clone)
            (call-interactively #'treebund-clone)
          selection))))

(defun treebund--read-project (workspace-path &optional prompt add initial)
  "Interactively find the path of a project.
WORKSPACE-PATH is the workspace to look for projects in.

PROMPT is the prompt to be presented to the user in the
minibuffer.

When ADD is non-nil, add an option for the user to add a project
that isn't in the workspace."
  (let* ((candidates (mapcar (lambda (project)
                               (cons (file-name-nondirectory project) project))
                             (treebund--workspace-projects workspace-path))))
    (when add (setq candidates (append candidates '(("[ add ]" . add)))))
    (let* ((selection (completing-read (or prompt "Project: ") candidates nil nil initial))
           (value (cdr (assoc selection candidates))))
      (if (equal value 'add)
          (let ((bare-path (treebund--read-bare prompt t)))
            (treebund--project-add workspace-path
                                   bare-path
                                   (treebund--branch-name workspace-path)))
        (expand-file-name selection workspace-path)))))

(defun treebund--read-workspace (&optional prompt require-match)
  "Interactively find the path of a workspace.
PROMPT is the prompt to be presented to the user in the
minibuffer."
  (let* ((candidates (mapcar (lambda (workspace)
                               (cons workspace
                                     (expand-file-name workspace treebund-workspace-root)))
                             (treebund--workspaces)))
         (default (when-let ((workspace-path (treebund-current-workspace)))
                    (treebund--workspace-name workspace-path)))
         (prompt (if default (format "%s [%s]: " (or prompt "Workspace") default) "Workspace: "))
         (selection (completing-read prompt candidates nil require-match nil nil default)))
    (if-let ((existing (cdr (assoc selection candidates))))
        existing
      (let ((workspace-path (concat (file-name-as-directory treebund-workspace-root) selection)))
        (make-directory workspace-path)
        workspace-path))))

(defun treebund--git-url-like-p (url)
  "Return non-nil if URL seems like a git-clonable URL.
The URL is returned for non-nil."
  (and (or (string-prefix-p "ssh://git@" url)
           (string-prefix-p "git@" url)
           (string-prefix-p "http://" url)
           (string-prefix-p "https://" url))
       (string-suffix-p ".git" url)
       url))

(defun treebund--open (project-path)
  "Open a project in some treebund workspace.
PROJECT-PATH is the project to be opened."
  (let* ((workspace-path (treebund-current-workspace project-path))
         (new-workspace-p (not (string= (treebund-current-workspace) workspace-path)))
         (new-project-p (not (string= (treebund--project-current) project-path))))
    (when new-workspace-p (run-hook-with-args 'treebund-before-workspace-open-functions workspace-path))
    (when new-project-p (run-hook-with-args 'treebund-before-project-open-functions project-path))

    (funcall treebund-project-open-function project-path)

    (when new-project-p (run-hooks 'treebund-after-project-open-hook))
    (when new-workspace-p (run-hooks 'treebund-after-workspace-open-hook))))

;;;###autoload
(defun treebund-open (workspace-path)
  "Open or create a workspace and a project within it.
This will always prompt for a workspace. If you want to prefer
your current workspace, use `treebund-open-project'."
  (interactive (list (treebund--read-workspace "Open workspace")))
  (treebund--open
   (treebund--read-project workspace-path
                           (format "Open project in %s: " (treebund--workspace-name workspace-path))
                           t)))

;;;###autoload
(defun treebund-open-project ()
  "Open a project in some treebund workspace.
This function will try to use your current workspace first if the
current buffer is in one."
  (interactive)
  (treebund-open
   (or (treebund-current-workspace)
       (treebund--read-workspace))))

;;;###autoload
(defun treebund-delete-workspace (workspace-path)
  "Delete workspace at WORKSPACE-PATH."
  (interactive
   (list (treebund--read-workspace "Delete workspace" t)))
  (if (and (file-exists-p workspace-path)
           (directory-empty-p workspace-path))
      (delete-directory workspace-path nil nil)
    (user-error "Workspace must be empty to delete")))

;;;###autoload
(defun treebund-add-project (workspace-path bare-path project-path project-branch)
  "Add a project to a workspace.
This will create a worktree in WORKSPACE-PATH with a branch named
after the workspace with `treebund-prefix' prefixed.

WORKSPACE-PATH is the path to the workspace in which the worktree
will be created.

BARE-PATH is the bare git repository where the worktree is
derived.

PROJECT-PATH is the path where the worktree will be created.  The
provided path should be in WORKSPACE-PATH directory.

PROJECT-BRANCH is the name of the branch to be checked out for
this project."
  (interactive
   (let* ((workspace-path (treebund--read-workspace "Add project to"))
          (bare-path (treebund--read-bare (format "Add project to %s: "
                                                  (treebund--workspace-name workspace-path))
                                          t
                                          (treebund--workspace-projects workspace-path)))
          (project-branch (completing-read "Branch: "
                                           (treebund--branches bare-path)
                                           nil
                                           nil
                                           (treebund--branch-name workspace-path)))
          (project-path (treebund--read-project workspace-path "Project name: "
                                                nil
                                                (treebund--bare-name bare-path))))
     (list workspace-path bare-path project-path project-branch)))
  (treebund--open (treebund--project-add workspace-path
                                         bare-path
                                         project-branch
                                         project-path)))

;;;###autoload
(defun treebund-remove-project (project-path)
  "Remove project at PROJECT-PATH from a workspace.
If there are not commits to the branch, the branch will automatically be deleted."
  (interactive
   (let ((workspace-path (or (and (not current-prefix-arg)
                                  (treebund-current-workspace))
                             (treebund--read-workspace "Remove project from" t))))
     (list (treebund--read-project workspace-path
                                   (format "Remove project from %s: "
                                           (treebund--workspace-name workspace-path))))))
  (let ((bare-path (treebund--project-bare project-path))
        (branch-name (treebund--branch project-path)))
    (treebund--worktree-remove project-path)
    (unless (treebund--unpushed-commits-p bare-path `(,branch-name))
      (treebund--branch-delete bare-path branch-name))))

;;;###autoload
(defun treebund-clone (url)
  "Clone URL to the collection of bare repos."
  (interactive
   (list (read-string "URL: " (or (treebund--git-url-like-p (gui-get-selection 'CLIPBOARD 'STRING))
                                  (treebund--git-url-like-p (gui-get-selection 'PRIMARY 'STRING))))))
  (message "Cloning %s..." url)
  (let ((bare-path (treebund--clone url)))
    (message "Finished cloning %s." (treebund--bare-name bare-path))
    bare-path))

;;;###autoload
(defun treebund-delete-bare (bare-path &optional interactive)
  "Delete a bare repository at BARE-PATH.
Existing worktrees or uncommitted changes will be checked before
deletion."
  (interactive
   (list (treebund--read-bare "Select repo to delete: ") t))
  (cond ((treebund--has-worktrees-p bare-path)
         (treebund--error "This repository has worktrees checked out"))
        ((and (treebund--unpushed-commits-p bare-path)
              (not (if interactive
                       (yes-or-no-p (format "%s has unpushed commits on some branches.  Delete anyway?" (treebund--bare-name bare-path)))
                     (treebund--error (format "%s has unpushed commits on some branches." (treebund--bare-name bare-path)))))))
        (t (treebund--bare-delete bare-path))))

(provide 'treebund)

;;; treebund.el ends here
