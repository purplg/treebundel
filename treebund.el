;;; treebund.el --- Bundle related git-worktrees together -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "26.1"))
;; Version: 0.1.0
;; Author: Ben Whitley
;; Homepage: https://github.com/purplg/treebund.el
;; SPDX-License-Identifier: MIT

;;; Commentary:

;  This package is used for automation the grouping of related git-worktrees
;  from multiple repositories together. This helps switch quickly between
;  repositories and ensure you're on the correct branch. When you're done with
;  the feature, you can look at the repositories in the workspace and know which
;  ones were modified to simplify the process of getting the changes merged
;  together.

;  Additionally, a single bare repository is shared between multiple
;  workspaces. You can stash, pop, and pull changes in from the same repository
;  in other workspaces thanks to the power of git-worktrees.

;;
;; TERMINOLOGY
;;

;  WORKSPACE: A collection of 'PROJECT's created from 'BARE's.
;  PROJECT:   A git-worktree checked out from a 'BARE' stored in a 'WORKSPACE'.
;  BARE:      A bare repository used as a source to create 'PROJECT' git-worktree.
;  PREFIX:    The string added before the name of the branch checked out for the 'WORKSPACE'.

;;
;; STRUCTURE
;;

;  Workspaces are structured as such:

;  `treebund-workspace-root' (default: "~/workspaces/")
;     |
;     L workspace1
;     |    L project-one   (branch: "feature/workspace1")
;     |    L project-two   (branch: "feature/workspace1")
;     |    L project-three (branch: "feature/workspace1")
;     |
;     L workspace2
;          L project-one   (branch: "feature/workspace2")
;          L project-two   (branch: "feature/workspace2")
;          L project-three (branch: "feature/workspace2")

;;
;; QUICK START
;;

;  Assuming default configuration, the following will create a bare clone of the
;  provided repo URL to '~/workspaces/.bare/<repo-name>.git', then create and
;  open a worktree for a new branch called 'feature/<workspace-name>'.

;  1. Create a new workspace using `treebund-workspace-new'.
;  2. Interactively call `treebund-project-add'.
;  3. Select the newly created workspace.
;  4. Select '[ clone ]'.
;  5. Enter the remote URL for the repository to be added to the workspace.

;;
;; USAGE
;;

;  | Command                     | Description                       |
;  |-----------------------------+-----------------------------------|
;  | `treebund-open'             | Open a project in a workspace     |
;  | `treebund-project-add'      | Add a project to a workspace      |
;  | `treebund-project-remove'   | Remove a project from a workspace |
;  | `treebund-workspace-new'    | Create a new workspace            |
;  | `treebund-workspace-delete' | Delete a new workspace            |

;;; Code:
(require 'subr-x)
(require 'vc-git)
(require 'project)


;; Customization

(defgroup treebund nil
  "Exploit git-worktrees to create inter-related project workspaces."
  :group 'convenience
  :prefix "treebund-")

(defcustom treebund-prefix "feature/"
  "The string prefix before every new project branch."
  :group 'treebund
  :type 'string)

(defcustom treebund-workspace-root "~/workspaces/"
  "The path the root of all workspaces are stored."
  :group 'treebund
  :type 'string)

(defcustom treebund-bare-dir (expand-file-name ".bare" treebund-workspace-root)
  "The path where bare repositories are stored."
  :group 'treebund
  :type 'string)

(defcustom treebund-before-project-open-hook nil
  "Hook which is run before a project is opened."
  :group 'treebund
  :type 'hook)

(defcustom treebund-after-project-open-hook nil
  "Hook which is run before a project is opened."
  :group 'treebund
  :type 'hook)

(defcustom treebund-before-workspace-open-hook nil
  "Hook which is run before a workspace is opened."
  :group 'treebund
  :type 'hook)

(defcustom treebund-after-workspace-open-hook nil
  "Hook which is run before a workspace is opened."
  :group 'treebund
  :type 'hook)


;; Git commands

(defmacro treebund--git (&rest args)
  `(with-temp-buffer
     (treebund--gitlog 'command (string-join (list ,@args) " "))
     (let ((result (vc-git--call t ,@args))
           (output (string-trim-right (buffer-string))))
       (treebund--gitlog 'output output)
       (if (= 0 result)
           (if (string-empty-p output) t output)
         (user-error "Git command failed. See *treebund-log*.")))))

(defmacro treebund--git-with-repo (repo-path &rest args)
  `(treebund--git "-C" (expand-file-name ,repo-path) ,@args))

(defun treebund--branches (repo-path &optional omit-main)
  (let ((branches (string-lines (treebund--git-with-repo repo-path
                                  "branch" "--format=%(refname:short)"))))
    (if omit-main
        (let ((main-branch (treebund--branch-main repo-path)))
          (seq-remove (lambda (branch) (equal main-branch branch)) branches))
      branches)))

(defun treebund--worktree-bare (project-path)
  "Return the bare related to PROJECT-PATH."
  (treebund--git-with-repo project-path
    "rev-parse" "--path-format=absolute" "--git-common-dir"))

(defun treebund--worktree-remove (repo-path)
  "Remove the worktree at PROJECT-PATH."
  (let ((bare-path (treebund--worktree-bare repo-path)))
    (treebund--git-with-repo bare-path
      "worktree" "remove" (expand-file-name repo-path))))

(defun treebund--worktree-add (workspace-path bare-path &optional project-path branch-name)
  "Create a worktree.
WORKSPACE-PATH is the directory to place the new worktree in.

BARE-PATH is the main repository the worktree is being created from.

Returns the path to the newly created worktree."
  (let* ((bare-name (treebund--bare-name bare-path))
         (branch-name (or branch-name (treebund--branch-name workspace-path)))
         (project-path (or project-path (expand-file-name bare-name workspace-path))))
    (if (member branch-name (treebund--branches bare-path))
        (treebund--git-with-repo bare-path
          "worktree" "add" project-path branch-name)
      (treebund--git-with-repo bare-path
        "worktree" "add" project-path "-b" branch-name))
    project-path))

(defun treebund--branch (repo-path)
  "Return the branch checked out REPO-PATH."
  (treebund--git-with-repo repo-path
    "branch" "--show-current"))

(defun treebund--branch-delete (bare-path branch-name)
  (treebund--git-with-repo bare-path
                           "branch" "-D" branch-name))

(defun treebund--clone (url dest)
  "Clone a repository from URL to DEST."
  (message "Cloning %s..." url)
  (treebund--git
    "clone" url "--bare" dest)
  (treebund--git-with-repo dest
    "config" "remote.origin.fetch" "+refs/heads/*:refs/remotes/origin/*")
  (treebund--git-with-repo dest
    "fetch")
  (message "Finished cloning %s." (treebund--bare-name dest))
  t)

(defun treebund--list-worktrees (repo-path)
  "Returns a list of worktrees for REPO-PATH."
  (seq-map
   (lambda (worktree)
             (split-string worktree "\0" t))
           (split-string (treebund--git-with-repo repo-path
                           "worktree" "list" "-z" "--porcelain")
                         "\0\0"
                         t)))

(defun treebund--rev-count (repo-path commit-a &optional commit-b)
  "Return the number of commits between COMMIT-A and COMMIT-B at REPO-PATH."
  (unless commit-b
    (setq commit-b commit-a)
    (setq commit-a (treebund--branch-main repo-path)))
  (string-to-number
   (treebund--git-with-repo repo-path
     "rev-list" (concat commit-a ".." commit-b) "--count")))

; Logging
(defface treebund--gitlog-heading
  '((t (:inherit mode-line :extend t)))
  "Face for widget group labels in treebund's dashboard."
  :group 'treebund)

(defvar treebund--gitlog-buffer "*treebund-git*")

(defun treebund--gitlog-buffer ()
  "Return the debug buffer for treebund."
  (or (get-buffer treebund--gitlog-buffer)
      (let ((buf (get-buffer-create treebund--gitlog-buffer)))
        (with-current-buffer buf
          (read-only-mode 1)
          (set (make-local-variable 'window-point-insertion-type) t))
        buf)))

(defun treebund--gitlog (type &rest msg)
  "Insert a message in the treebund git log buffer.
COMMAND is the command that was executed.

OUTPUT is the output of the executed COMMAND."
  (with-current-buffer (treebund--gitlog-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (cond ((eq 'command type)
             (insert (propertize (string-join (append '("git") msg '("\n")) " ") 'face 'treebund--gitlog-heading)))
            ((eq 'output type)
             (let ((msg (apply #'format msg)))
               (when (length= msg 0)
                 (setq msg " "))
               (insert msg))
             (newline 2))))))


;; Internal

; Repos
(defmacro treebund--with-repo (repo-path &rest body)
  "Run BODY in the context of a buffer in REPOSITORY-PATH.
BODY is evaluated with the context of a buffer in the repo-path repository"
  `(let* ((buffer (find-file-noselect ,repo-path))
          (result nil))
     (with-current-buffer buffer
       (setq result ,@body))
     (kill-buffer buffer)
     result))

(defun treebund--repo-worktree-count (repo-path)
  "Returns the number of worktrees that exist for REPO-PATH."
  (seq-count
   (lambda (str) (not (member "bare" str)))
   (treebund--list-worktrees repo-path)))

(defun treebund--has-worktrees-p (repo-path)
  (> (treebund--repo-worktree-count repo-path) 1))

(defun treebund--unpushed-commits-p (repo-path &optional branches)
  "Returns if there are any unpushed commits to remote.

If BRANCH is nil, check all local branches.

If BRANCH is a string or list of strings, only check these local branches."
  (when (eq 'string (type-of branches))
    (setq branches (list branches)))
  (setq repo-path (treebund--project-bare repo-path))
  (seq-some (lambda (branch) (> (treebund--rev-count repo-path branch) 0))
            (or branches (treebund--branches repo-path))))

; Bares
(defun treebund--bare-name (bare-path)
  (file-name-base (directory-file-name bare-path)))

(defun treebund--bare-delete (bare-path)
  (setq bare-path (expand-file-name bare-path))
  (when (and (string-prefix-p (expand-file-name treebund-workspace-root) bare-path)
             (string-suffix-p ".git/" bare-path))
    (delete-directory bare-path t)))

(defun treebund--bare-list ()
  (directory-files treebund-bare-dir nil "^[^.].*"))

; Workspaces
(defun treebund--workspace-name (workspace-path)
  "Return the name of a workspace at WORKSPACE-PATH"
  (file-name-base (directory-file-name workspace-path)))

(defun treebund--workspace-projects (workspace-path)
  (directory-files workspace-path nil "^[^.].*"))

(defun treebund--workspaces ()
  (directory-files treebund-workspace-root nil "^[^.].*"))

(defun treebund--workspace-current (&optional file-path)
  "Return the path to the current workspace"
  (when-let ((project-path (or (treebund--project-current file-path)
                               file-path)))
    (treebund--project-workspace project-path)))

; Projects
(defun treebund--project-workspace (file-path)
  "Return the workspace path of a given PROJECT-PATH."
  (when-let* ((file-path (expand-file-name (directory-file-name file-path)))
              ((string-prefix-p (expand-file-name treebund-workspace-root) file-path))
              (parts (split-string file-path "/"))
              (workspace-name (nth (- (length parts) 2) parts)))
    (expand-file-name workspace-name treebund-workspace-root)))

(defun treebund--project-bare (project-path)
  "Return the respective bare for project at PROJECT-PATH."
  (cadr
   (split-string
    (car
     (seq-find (lambda (worktree) (member "bare" worktree))
               (treebund--list-worktrees project-path))))))

(defun treebund--project-current (project-path)
  "Return the project path."
  (when-let* ((project-path (or project-path buffer-file-name))
              (project-path (file-name-directory project-path))
              (file-exists-p project-path)
              (project (project-current nil project-path)))
    (expand-file-name (project-root project))))

; Branches
(defun treebund--branch-name (workspace-path)
  (concat treebund-prefix (treebund--workspace-name workspace-path)))

(defun treebund--branch-main (repo-path)
  (treebund--with-repo repo-path
    (car (vc-git-branches))))


;; Interactive read

(defun treebund--read-bare (&optional prompt clone omit)
  "Interactively find the path of a bare."
  (let* ((candidates (mapcar (lambda (bare)
                               (cons (replace-regexp-in-string "\.git$" "" bare)
                                     (file-name-as-directory (expand-file-name bare treebund-bare-dir))))
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

(defun treebund--read-project (workspace-path &optional prompt clone)
  "Interactively find the path of a project.
WORKSPACE-PATH is the workspace to look for projects in.

PROMPT is the prompt to be presented to the user in the
minibuffer.

When ADD is non-nil, add an option for the user to add a project
that isn't in the workspace."
  (let* ((candidates (mapcar (lambda (project)
                               (cons project (file-name-as-directory (expand-file-name project workspace-path))))
                             (treebund--workspace-projects workspace-path))))
    (when clone
      (setq candidates (append candidates '(("[ add ]" . add)))))
    (if-let ((selection (cdr (assoc (completing-read (or prompt "Project: ") candidates) candidates))))
        (if (equal selection 'add)
            (treebund-project-add workspace-path
                                  (treebund--read-bare prompt))
          selection))))

(defun treebund--read-workspace (&optional prompt)
  "Interactively find the path of a workspace."
  (let ((candidates (mapcar (lambda (workspace)
                              (cons workspace (file-name-as-directory (expand-file-name workspace treebund-workspace-root))))
                            (treebund--workspaces))))
    (cdr (assoc (completing-read (or prompt "Workspace: ") candidates) candidates))))


;; User functions

(defun treebund--git-url-like-p (url)
  "Return non-nil if URL looks kinda like a git-clonable URL.
When non-nil, the returned value URL."
  (and (or (string-prefix-p "ssh://git@" url)
           (string-prefix-p "git@" url)
           (string-prefix-p "http://" url)
           (string-prefix-p "https://" url))
       (string-suffix-p ".git" url)
       url))

;;;###autoload
(defun treebund-open (project-path)
  (interactive
   (let* ((workspace-path (or (and (not current-prefix-arg)
                                   (treebund--workspace-current))
                              (treebund--read-workspace "Open project in workspace: ")))
          (project-path (treebund--read-project
                         workspace-path
                         (format "Open project in %s: "
                                 (treebund--workspace-name workspace-path))
                         t)))
     (list project-path)))
  (let ((new-workspace-p (not (string= (treebund--workspace-current)
                                       (treebund--workspace-current project-path)))))
    (when new-workspace-p (run-hooks 'treebund-before-workspace-open-hook))
    (run-hooks 'treebund-before-project-open-hook)

    (project-switch-project project-path)

    (run-hooks 'treebund-after-project-open-hook)
    (when new-workspace-p (run-hooks 'treebund-after-workspace-open-hook))))

;;;###autoload
(defun treebund-workspace-new (workspace-path)
  (interactive
   (list (read-directory-name "Create a new workspace: " (expand-file-name treebund-workspace-root))))
  (unless (file-exists-p workspace-path)
    (make-directory workspace-path)))

;;;###autoload
(defun treebund-workspace-delete (workspace-path)
  (interactive
   (list (treebund--read-workspace "Delete a workspace: ")))
  (if (and (file-exists-p workspace-path)
           (directory-empty-p workspace-path))
      (delete-directory workspace-path nil nil)
    (user-error "Workspace must be empty to delete.")))

;;;###autoload
(defun treebund-project-add (workspace-path bare-path)
  "Add a project to a workspace.
This will create a worktree in WORKSPACE-PATH with a branch named
after the workspace with `treebund-prefix' prefixed.

WORKSPACE-PATH is the path to the workspace in which the worktree
will be created.

BARE-PATH is the bare git repository where the worktree is
derived."
  (interactive
   (let ((workspace-path (or (and (not current-prefix-arg)
                                  (treebund--workspace-current))
                             (treebund--read-workspace "Add project to workspace: "))))
     (list workspace-path
           (treebund--read-bare (format "Add project to %s: " (treebund--workspace-name workspace-path))
                                t
                                (treebund--workspace-projects workspace-path)))))
  (treebund-open (treebund--worktree-add workspace-path bare-path)))

;;;###autoload
(defun treebund-project-add-detailed (workspace-path bare-path project-path project-branch)
  "Like `treebund-project-add' but also specify a project path and branch.
This will create a worktree in WORKSPACE-PATH with a branch named
after the workspace with `treebund-prefix' prefixed.

WORKSPACE-PATH is the path to the workspace in which the worktree
will be created.

BARE-PATH is the bare git repository where the worktree is
derived.

PROJECT-PATH is the path where the worktree will be created. The
provided path should be in WORKSPACE-PATH directory.

PROJECT-BRANCH is the name of the branch to be checked out for
this project."
  (interactive
   (let* ((workspace-path (or (and (not current-prefix-arg)
                                   (treebund--workspace-current))
                              (treebund--read-workspace "Add project to workspace: ")))
          (bare-path (treebund--read-bare (format "Add project to %s: "
                                                  (treebund--workspace-name workspace-path))
                                          t
                                          (treebund--workspace-projects workspace-path)))
          (project-path (expand-file-name (treebund--read-project workspace-path "Project path: ") workspace-path))
          (project-branch (read-string "Branch: " (file-name-base (directory-file-name project-path)))))
     (list workspace-path bare-path project-path project-branch)))
  (treebund-open (treebund--worktree-add workspace-path
                                         bare-path
                                         project-path
                                         project-branch)))

;;;###autoload
(defun treebund-project-remove (project-path)
  "Remove project at PROJECT-PATH from a workspace.
If there are not commits to the branch, the branch will automatically be deleted."
  (interactive
   (let ((workspace-path (or (and (not current-prefix-arg)
                                  (treebund--workspace-current))
                             (treebund--read-workspace "Remove project from workspace: "))))
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
  (let* ((dir-name (car (last (split-string url "/"))))
         (dest (expand-file-name dir-name treebund-bare-dir)))
    (when (file-exists-p dest)
      (user-error "Repostitory with this name is already cloned."))
    (treebund--clone url dest)))

;;;###autoload
(defun treebund-bare-delete (bare-path)
  (interactive
   (list (treebund--read-bare "Select repo to delete: ")))
  (cond ((treebund--has-worktrees-p bare-path)
         (user-error "This repository has worktrees checked out."))
        ((and (treebund--unpushed-commits-p bare-path)
              (not (yes-or-no-p (format "%s has unpushed commits on some branches. Delete anyway?" (treebund--bare-name bare-path))))))
        (t (treebund--bare-delete bare-path))))

(define-minor-mode treebund-mode
  "Exploit git-worktrees to create inter-related project workspaces."
  :group 'treebund
  :global t
  :interactive t)

(provide 'treebund)
