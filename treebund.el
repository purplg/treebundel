;;; treebund.el --- Group related git-worktrees together -*- lexical-binding: t; -*-

;; This package is used to automate my personal workflow of creating directories
;; of related worktrees from multiple repositories together.

;; TERMINOLOGY

; WORKSPACE: A collection of 'PROJECT's created from 'BARE's.
; PROJECT:   A git-worktree checked out from a 'BARE' stored in a 'WORKSPACE'.
; BARE:      A bare repository used as a source to create 'PROJECT' git-worktree.
; PREFIX:    The string added before the name of the branch checked out for the 'WORKSPACE'.

;; STRUCTURE

; Workspaces are structured as such:

; `treebund-workspace-root' (default: "~/workspaces/")
;    |
;    L workspace1
;    |    L project-one (branch: "feature/workspace1")
;    |    L project-two (branch: "feature/workspace1")
;    |    L project-three (branch: "feature/workspace1")
;    |
;    L workspace2
;         L project-one (branch: "feature/workspace2")
;         L project-two (branch: "feature/workspace2")
;         L project-three (branch: "feature/workspace2")

;; GETTING STARTED

; 1. Create a new workspace using `treebund-workspace-new'.
; 2. Interactively call `treebund-project-add'.
; 3. Select the newly created workspace.
; 4. Select '[ new ]'.
; 5. Enter the remote URL for the repository to be added to the workspace.
;      After this step, steps 4-5 can be skipped by just selecting the now-existing repository directly.
; 6. Edit

; USAGE

(require 'subr-x)
(require 'vc-git)

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


;; Logging
(defvar treebund--git-buffer "*treebund-git*")

(defun treebund--log (&rest msg)
  (with-current-buffer (get-buffer-create treebund--git-buffer)
    (goto-char (point-max))
    (apply #'insert msg)
    (insert ?\n)))


;; Git commands

(defmacro treebund--git (&rest args)
  `(with-temp-buffer
     (treebund--log "COMMAND: git " (string-join (list ,@args) " "))
     (let ((result (vc-git--call t ,@args))
           (output (string-trim-right (buffer-string))))
       (treebund--log output ?\n)
       (if (= 0 result)
           (if (string-empty-p output) t output)
         (user-error "Git command failed.")))))

(defmacro treebund--git-with-repo (repo-path &rest args)
  `(treebund--git "-C" (expand-file-name ,repo-path) ,@args))

(defun treebund--branches (repo-path)
  (string-lines
   (treebund--git-with-repo repo-path
     "branch" "--format=%(refname:short)")))

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
  (let ((bare-name (treebund--bare-name bare-path))
        (branch-name (treebund--branch-name workspace-path)))
    (if (member branch-name (treebund--branches bare-path))
        (treebund--git-with-repo bare-path
          "worktree" "add" (expand-file-name bare-name workspace-path) branch-name)
      (treebund--git-with-repo bare-path
        "worktree" "add" (expand-file-name bare-name workspace-path) "-b" branch-name))))

(defun treebund--clone (repo-url)
  (let ((repo-name (car (last (split-string repo-url "/")))))
    (treebund--git "clone" repo-url "--bare" (expand-file-name repo-name treebund-bare-dir))))


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

; Bares
(defun treebund--bare-name (bare-path)
  (file-name-base (directory-file-name bare-path)))

; Workspaces
(defun treebund--workspace-name (workspace-path)
  "Return the name of a workspace at WORKSPACE-PATH"
  (file-name-base (directory-file-name workspace-path)))

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

(defun treebund--branch-main ()
  (when-let ((project-path (treebund--project-current)))
    (treebund--with-repo (treebund--worktree-bare project-path)
      (car (vc-git-branches)))))


;; Interactive read

(defun treebund--read-bare (&optional prompt)
  "Interactively find the path of a bare."
  (let ((candidates (mapcar (lambda (bare)
                              (cons (replace-regexp-in-string "\.git$" "" bare)
                                    (file-name-as-directory (expand-file-name bare treebund-bare-dir))))
                            (directory-files treebund-bare-dir nil "^[^.].*"))))
    (cdr (assoc (completing-read (or prompt "Select project: ") candidates) candidates))))

(defun treebund--read-project (workspace-path &optional prompt)
  "Interactively find the path of a project."
  (let* ((candidates (mapcar (lambda (project)
                               (cons project (file-name-as-directory (expand-file-name project workspace-path))))
                             (directory-files workspace-path nil "^[^.].*")))
         (candidates (append candidates '(("[ new ]" . new)))))
    (if-let ((selection (cdr (assoc (completing-read (or prompt "Project: ") candidates) candidates))))
        (if (equal selection 'new)
            (treebund-project-add workspace-path
                                (treebund--read-bare prompt))
          selection))))

(defun treebund--read-workspace (&optional prompt)
  "Interactively find the path of a workspace."
  (let ((candidates (mapcar (lambda (workspace)
                              (cons workspace (file-name-as-directory (expand-file-name workspace treebund-workspace-root))))
                            (directory-files treebund-workspace-root nil "^[^.].*"))))
    (cdr (assoc (completing-read (or prompt "Workspace: ") candidates) candidates))))


;; User functions

(defun treebund-open (project-path)
  (interactive
   (let ((workspace-path (treebund--read-workspace "Open project in workspace: ")))
     (list (treebund--read-project
            workspace-path
            (format "Open project in %s: "
                    (treebund--workspace-name workspace-path))))))
  (project-switch-project project-path))

(defun treebund-workspace-new (workspace-path)
  (interactive
   (list (read-directory-name "Create a new workspace: " (expand-file-name treebund-workspace-root))))
  (unless (file-exists-p workspace-path)
    (make-directory workspace-path)))

(defun treebund-workspace-delete (workspace-path)
  (interactive
   (list (treebund--read-workspace "Delete a workspace: ")))
  (if (and (file-exists-p workspace-path)
             (directory-empty-p workspace-path))
      (delete-directory workspace-path nil nil)
    (user-error "Workspace must be empty to delete.")))

(defun treebund-project-add (workspace-path bare-path)
  (interactive
   (let ((workspace-path (treebund--read-workspace)))
     (list workspace-path
           (treebund--read-bare (format "Add project to %s: " (treebund--workspace-name workspace-path))))))
  (treebund--worktree-add workspace-path bare-path))

(defun treebund-project-remove (project-path)
  (interactive
   (let ((workspace-path (or (treebund--workspace-current)
                             (treebund--read-workspace))))
     (list (treebund--read-project workspace-path
                                    (format "Remove project from %s: "
                                            (treebund--workspace-name workspace-path))))))
  (treebund--worktree-remove project-path))

(defun treebund-clone (url)
  (interactive
   (list (read-string "URL: " (gui-selection-value))))
  (treebund--clone url))

(define-minor-mode treebund-mode
  "Exploit git-worktrees to create inter-related project workspaces."
  :group 'treebund
  :global t
  :interactive t)

(provide 'treebund)
