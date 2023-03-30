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


;; Git commands

(defmacro treebund--git (&rest args)
  `(with-temp-buffer
     (treebund--log "COMMAND: git " (string-join (list ,@args) " "))
     (let ((result (vc-git--call t ,@args))
           (output (string-trim-right (buffer-string))))
       (treebund--log output ?\n)
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

(defun treebund--worktree-add (workspace-path bare-path)
  "Create a worktree.
WORKSPACE-PATH is the directory to place the new worktree in.

BARE-PATH is the main repository the worktree is being created from.

Returns the path to the newly created worktree."
  (let* ((bare-name (treebund--bare-name bare-path))
         (branch-name (treebund--branch-name workspace-path))
         (project-path (expand-file-name bare-name workspace-path)))
    (if (member branch-name (treebund--branches bare-path))
        (treebund--git-with-repo bare-path
          "worktree" "add" project-path branch-name)
      (treebund--git-with-repo bare-path
        "worktree" "add" project-path "-b" branch-name))
    project-path))

(defun treebund--clone (bare-url)
  "Clone a repository to the bare directory.
Returns the path to the newly cloned repo."
  (let* ((bare-name (car (last (split-string bare-url "/"))))
         (bare-path (expand-file-name bare-name treebund-bare-dir)))
    (message "Cloning %s..." bare-url)
    (treebund--git "clone" bare-url "--bare" bare-path)
    (treebund--git-with-repo bare-path "config" "remote.origin.fetch" "+refs/heads/*:refs/remotes/origin/*")
    (treebund--git-with-repo bare-path "fetch")
    (message "Finished cloning %s." (treebund--bare-name bare-path))
    bare-path))

(defun treebund--list-worktrees (repo-path)
  (treebund--git-with-repo repo-path "worktree" "list" "-z" "--porcelain"))

(defun treebund--rev-count (repo-path commit-a &optional commit-b)
  "Return the number of commits between COMMIT-A and COMMIT-B at REPO-PATH."
  (unless commit-b
    (setq commit-b commit-a)
    (setq commit-a (treebund--branch-main repo-path)))
  (string-to-number (treebund--git-with-repo repo-path "rev-list" (concat commit-a ".." commit-b) "--count")))

; Logging
(defvar treebund--git-buffer "*treebund-git*")

(defun treebund--log (&rest msg)
  (with-current-buffer (get-buffer-create treebund--git-buffer)
    (goto-char (point-max))
    (apply #'insert msg)
    (insert ?\n)))


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
  (seq-count
   (lambda (str) (equal "worktree" str))
   (split-string (treebund--list-worktrees repo-path) "\\(\0\\| \\)" t)))

(defun treebund--has-worktrees-p (repo-path)
  (> (treebund--repo-worktree-count repo-path) 1))

(defun treebund--unpushed-commits-p (repo-path)
  (seq-some (lambda (branch) (> (treebund--rev-count repo-path branch) 0))
            (treebund--branches repo-path)))

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

(defun treebund--workspace-current ()
  "Return the path to the current workspace"
  (when-let ((project (project-current)))
    (treebund--project-workspace (project-root project))))

; Projects
(defun treebund--project-workspace (project-path)
  "Return the workspace path of a given PROJECT-PATH."
  (let ((path (directory-file-name project-path)))
    (when (string-prefix-p treebund-workspace-root path)
      (let* ((parts (split-string path "/"))
             (workspace-name (nth (- (length parts) 2) parts)))
        (expand-file-name workspace-name treebund-workspace-root)))))

(defun treebund--project-current ()
  "Return the project path."
  (when-let ((path (directory-file-name (project-root (project-current)))))
    (when (treebund--project-workspace path)
      path)))

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

(defun treebund--read-project (workspace-path &optional prompt add)
  "Interactively find the path of a project."
  (let* ((candidates (mapcar (lambda (project)
                               (cons project (file-name-as-directory (expand-file-name project workspace-path))))
                             (treebund--workspace-projects workspace-path))))
    (when add
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

;;;###autoload
(defun treebund-open (project-path)
  (interactive
   (let ((workspace-path (treebund--read-workspace "Open project in workspace: ")))
     (list (treebund--read-project
            workspace-path
            (format "Open project in %s: "
                    (treebund--workspace-name workspace-path))
            t))))
  (project-switch-project project-path))

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
  (interactive
   (let ((workspace-path (treebund--read-workspace)))
     (list workspace-path
           (treebund--read-bare (format "Add project to %s: " (treebund--workspace-name workspace-path))
                                t
                                (treebund--workspace-projects workspace-path)))))
  (project-switch-project (treebund--worktree-add workspace-path bare-path)))

;;;###autoload
(defun treebund-project-remove (project-path)
  (interactive
   (let ((workspace-path (or (treebund--workspace-current)
                             (treebund--read-workspace))))
     (list (treebund--read-project workspace-path
                                   (format "Remove project from %s: "
                                           (treebund--workspace-name workspace-path))))))
  (treebund--worktree-remove project-path))

;;;###autoload
(defun treebund-clone (url)
  (interactive
   (list (read-string "URL: " (let ((clipboard (gui-get-selection 'CLIPBOARD 'STRING)))
                                ;; Does clipboard kinda look like a git url?
                                (and (or (string-prefix-p "git@" clipboard)
                                         (string-prefix-p "http" clipboard)
                                         (string-prefix-p "https" clipboard))
                                     (string-suffix-p ".git" clipboard)
                                     clipboard)))))
  (treebund--clone url))

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
