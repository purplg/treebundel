;;; treebund-tests.el --- Bundle related git-worktrees together -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)

(require 'treebund)


;;; Logging:
(setq ert-quiet nil)

(setq-default treebund-test-logging nil)

(when treebund-test-logging
  (defun treebund--gitlog (type &rest msg)
    "Override the logging method to write to a file instead of buffer."
    (with-temp-buffer
      (goto-char (point-max))
      (cond ((eq 'command type)
             (insert "==> git " (string-join msg " ") ?\n))
            ((eq 'output type)
             (let ((msg (apply #'format msg)))
               (when (= (length msg) 0)
                 (setq msg " "))
               (insert msg))
             (newline 2)))
      (append-to-file nil nil (expand-file-name "treebund-test-git.log" temporary-file-directory)))))


;;; Environment:
(setq treebund-test--dir (concat temporary-file-directory "treebund-tests/"))
(setq treebund-remote--dir (concat treebund-test--dir "simulated-remote/"))

(defun treebund-test--setup-branch (name origin-path &optional num-commits)
  (let ((worktree-path (expand-file-name name treebund-remote--dir)))
    (treebund--git "clone" origin-path worktree-path)
    (treebund--git-with-repo worktree-path
      "checkout" "-b" name)
    (dotimes (i (or num-commits 3))
      (let ((test-file (expand-file-name "some-file" worktree-path)))
        (with-temp-buffer
          (insert i)
          (write-file test-file))
        (treebund--git-with-repo worktree-path
          "add" test-file)
        (treebund--git-with-repo worktree-path
          "commit" "-m" (concat "commit-" (int-to-string i)))))
    (treebund--git-with-repo worktree-path "push" "--set-upstream" "origin" name)
    (delete-directory worktree-path t)))

(defun treebund-test--setup-remote (name branches)
  "Setup simulated remote.
NAME is the name of the remote repository.

BRANCHES is a list of branch names to be created in this
remote. Each branch will have 2 commits added."
  (let ((origin-path (expand-file-name (concat name ".git") treebund-remote--dir)))
    (treebund--git "init" "--bare" origin-path)
    (dolist (branch branches)
      (treebund-test--setup-branch branch origin-path 3))
    (make-directory treebund-bare-dir t)))

(defun treebund-test--setup-project (worktree-path remote-name branch)
  "Setup simulated remote.
NAME is the name of the remote repository.

BRANCHES is a list of branch names to be created in this
remote. Each branch will have 2 commits added."
  ;; Clone the repo if it hasn't been cloned from simulated-remote yet
  (let ((bare-path (concat (file-name-as-directory treebund-bare-dir)
                           (concat remote-name ".git")))
        (worktree-path (concat treebund-workspace-root worktree-path)))
    (make-directory worktree-path t)
    (unless (file-exists-p bare-path)
      (treebund--clone (concat (file-name-as-directory treebund-remote--dir)
                               (concat remote-name ".git"))))
    (treebund--worktree-add bare-path worktree-path branch)))

(defun treebund-test--setup (remotes projects)
  "Setup testing environment.
REMOTES is a list of cons cells of remote names to a list of
branch names. For example:

\\=(:remotes ((\"origin-one\" . (\"branch-one\" \"branch-two\"))
           (\"origin-two\" . (\"branch-one\")))
 :projects ((\"workspace/project-one-path\" \"origin-one\" \"branch-one\"))
            (\"workspace/project-two-path\" \"origin-one\" \"branch-two\")))

Each created branch will have 2 commits."
  ;; Create temp directory.
  (when (file-directory-p treebund-test--dir)
    (delete-directory treebund-test--dir t))
  (make-directory treebund-test--dir)
  (make-directory treebund-workspace-root)
  (make-directory treebund-remote--dir)

  (let ((remote (pop remotes)))
    (while remote
      (treebund-test--setup-remote (car remote)
                                   (cdr remote))
      (setq remote (pop remotes))))

  (let ((project (pop projects)))
    (while project
      (let* ((worktree-path (pop project))
             (remote-branch (split-string (pop project) "/")))
        (treebund-test--setup-project worktree-path (pop remote-branch) (pop remote-branch)))
      (setq project (pop projects)))))

(defmacro treebund-deftest (name test-args &rest body)
  "Wrapper around `ert-deftest' to ensure correct tmp directories
are used for all tests."
  (declare (indent defun)
           (doc-string 3))
  `(ert-deftest ,name ()
     ,(when (stringp (car body))
        (pop body))
     (let* ((inhibit-message nil)
            (treebund-workspace-root (concat treebund-test--dir "workspaces/"))
            (treebund-bare-dir (concat (file-name-as-directory treebund-workspace-root) ".bare/"))
            (treebund-project-open-function (lambda (&rest _)))
            (treebund-prefix "test/"))
       (treebund-test--setup
        (plist-get ',test-args ':remotes)
        (plist-get ',test-args ':projects))
       ,@body)))


;;; Tests:
(treebund-deftest setup
  ( :remotes (("remote-one" . ())
              ("remote-two" . ("branch-one"))
              ("remote-three" . ("branch-one" "branch-two")))
    :projects (("some-workspace/project-two-one" "remote-two/branch-one")
               ("some-workspace/project-three-one" "remote-three/branch-one")
               ("some-workspace/project-three-two" "remote-three/branch-two")))
  "The basic testing environment used for all treebund tests."
  (let ((origin (expand-file-name "remote-one.git" treebund-remote--dir)))
    (should (file-directory-p origin))
    (should-not (treebund--branches origin)))

  (let ((origin (expand-file-name "remote-two.git" treebund-remote--dir)))
    (should (file-directory-p origin))
    (should (= (length (treebund--branches origin)) 1))
    (should (member "branch-one" (treebund--branches origin))))

  (let ((origin (expand-file-name "remote-three.git" treebund-remote--dir)))
    (should (file-directory-p origin))
    (should (= (length (treebund--branches origin)) 2))
    (should (member "branch-one" (treebund--branches origin)))
    (should (member "branch-two" (treebund--branches origin))))

  (let* ((workspace (concat treebund-workspace-root "some-workspace"))
         (project-two-one (concat (file-name-as-directory workspace) "project-two-one"))
         (project-three-one (concat (file-name-as-directory workspace) "project-three-one"))
         (project-three-two (concat (file-name-as-directory workspace) "project-three-two")))
    (should (file-directory-p workspace))
    (should (file-directory-p project-two-one))
    (should (file-directory-p project-three-one))
    (should (file-directory-p project-three-two))))

(treebund-deftest branches
  (:remotes (("remote" . ("test/branches" "test/branches-two"))
             ("empty-remote" . ())))
  (let* ((workspace-path (concat treebund-workspace-root "branches"))
         (remote (concat treebund-remote--dir "remote.git"))
         (bare-path (treebund--clone remote)))

    ; Check bare repository branches
    (should (= (length (treebund--branches bare-path)) 2))
    (should (member "test/branches" (treebund--branches remote)))
    (should (member "test/branches-two" (treebund--branches remote)))

    ; Check worktree branches
    (let ((project-path (treebund--project-add workspace-path bare-path)))
      (should (= (length (treebund--branches project-path)) 2))
      (should (member "test/branches" (treebund--branches project-path)))
      (should (member "test/branches-two" (treebund--branches project-path)))))

  ; Ensure nil is returned when no branches exist.
  (let* ((remote (expand-file-name "empty-remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote)))
    (should (= (length (treebund--branches bare-path)) 0))))

(treebund-deftest worktree-bare
  (:remotes (("remote" . ("test/worktree-bare"))))
  (let* ((workspace-path (concat treebund-workspace-root "worktree-bare"))
         (remote (expand-file-name "remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote))
         (project-path (treebund--project-add workspace-path bare-path)))
    (should (string= bare-path (treebund--worktree-bare project-path)))))

(treebund-deftest worktree-remove
  (:remotes (("remote" . ("test/worktree-remove"))))
  (let* ((workspace-path (concat treebund-workspace-root "worktree-remove/"))
         (remote (concat treebund-remote--dir "remote.git"))
         (bare-path (treebund--clone remote))
         (project-path (treebund--project-add workspace-path bare-path)))
    (treebund--worktree-remove project-path)
    (should-not (file-exists-p project-path))))

(treebund-deftest worktree-add
  (:remotes (("remote" . ("test/worktree-add"))))
  (let* ((workspace-path (expand-file-name "worktree-add" treebund-workspace-root))
         (remote (expand-file-name "remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote))
         (project-path (treebund--project-add workspace-path bare-path)))
    (should (file-directory-p project-path))))

(treebund-deftest branch
  (:remotes (("remote" . ("test/branch"))))
  (let* ((workspace-path (expand-file-name "branch" treebund-workspace-root))
         (remote (expand-file-name "remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote))
         (project-path (treebund--project-add workspace-path bare-path)))
    (should (string= "test/branch" (treebund--branch project-path)))))

(treebund-deftest branch-delete
  (:remotes (("remote" . ("master" "branch-one" "branch-two"))))
  (let* ((workspace-path (expand-file-name "branch-delete" treebund-workspace-root))
         (remote (expand-file-name "remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote))
         (project-path (treebund--project-add workspace-path bare-path)))
    (should (= (length (treebund--branches bare-path)) 4))
    (treebund--branch-delete bare-path "branch-one")
    (should (= (length (treebund--branches bare-path)) 3))
    (treebund--branch-delete project-path "branch-two")
    (should (= (length (treebund--branches bare-path)) 2))))

(treebund-deftest clone
  (:remotes (("remote" . ("master" "other-branch"))))
  (let* ((workspace-path (expand-file-name "branch-delete" treebund-workspace-root))
         (remote (expand-file-name "remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote)))
    (should (file-directory-p bare-path))
    (should (= (length (treebund--branches bare-path)) 2))
    (should (member "master" (treebund--branches bare-path)))
    (should (member "other-branch" (treebund--branches bare-path)))))

(treebund-deftest worktree-list
  (:remotes (("remote" . ("master"))))
  (let* ((remote (expand-file-name "remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote)))
    (should (= (length (treebund--worktree-list bare-path)) 1))
    (should (member "bare" (car (treebund--worktree-list bare-path))))

    (let ((worktree (treebund--project-add
                         (expand-file-name "worktree-list-one" treebund-workspace-root)
                         bare-path)))
      (should (= (length (treebund--worktree-list worktree)) 2))
      (should-not (member "bare" (cadr (treebund--worktree-list bare-path)))))

    (let ((worktree (treebund--project-add
                         (expand-file-name "worktree-list-two" treebund-workspace-root)
                         bare-path)))
      (should (= (length (treebund--worktree-list worktree)) 3)))))

(treebund-deftest rev-count
  (:remotes (("remote" . ("master" "other-branch"))))
  (let* ((remote (expand-file-name "remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote)))
    (should (= (treebund--rev-count bare-path "other-branch") 3))))

(treebund-deftest repo-worktree-count
  (:remotes (("remote" . ("master"))))
  (let* ((remote (expand-file-name "remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote)))
    (should (= (treebund--repo-worktree-count bare-path) 0))
    (let ((worktree (treebund--project-add (expand-file-name "repo-worktree-count-one"
                                                             treebund-workspace-root)
                                           bare-path)))
      (should (= (treebund--repo-worktree-count bare-path) 1)))

    (let ((worktree (treebund--project-add (expand-file-name "repo-worktree-count-two"
                                                             treebund-workspace-root)
                                           bare-path)))
      (should (= (treebund--repo-worktree-count bare-path) 2)))))

(treebund-deftest has-worktrees-p
  (:remotes (("remote" . ("master"))))
  (let* ((remote (expand-file-name "remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote)))
    (should-not (treebund--has-worktrees-p bare-path))
    (treebund--project-add (expand-file-name "repo-worktree-count-one"
                                             treebund-workspace-root)
                           bare-path)
    (should (treebund--has-worktrees-p bare-path))))

(treebund-deftest unpushed-commits-p
  ( :remotes (("origin" . ("master" "other-branch")))
    :projects (("some-feature/master" . ("origin/master"))
               ("some-feature/other" . ("origin/other"))))
  (let ((master-path (concat (file-name-as-directory treebund-workspace-root) "some-feature/master"))
        (other-path (concat (file-name-as-directory treebund-workspace-root) "some-feature/other")))
    ;; A freshly clone repository should not be dirty.
    (should-not (treebund--unpushed-commits-p master-path))

    ;; Create and commit an empty file
    (let ((test-file (concat (file-name-as-directory master-path) "unpushed")))
      (with-temp-buffer (write-file test-file))
      (treebund--git-with-repo master-path "add" test-file)
      (treebund--git-with-repo master-path "commit" "-m" "unpushed-commit"))

    ;; Since we have an unpushed commit, this should pass now.
    (should (treebund--unpushed-commits-p master-path))

    ;; Other project should also reflect unpushed commits
    (should (treebund--unpushed-commits-p other-path))

    ;; Push and it should now pass.
    (treebund--git-with-repo master-path "push" "--set-upstream" "origin" "master")
    (should-not (treebund--unpushed-commits-p master-path))))

(treebund-deftest bare-name
  ( :remotes (("origin" . ("master")))
    :projects (("some-feature/feature" "origin/master")))
  (let* ((remote-path (concat (file-name-as-directory treebund-remote--dir) "origin.git"))
         (bare-path (concat (file-name-as-directory treebund-bare-dir) "origin.git"))
         (project-path (concat (file-name-as-directory treebund-workspace-root) "some-feature/feature")))
    (should (string= "origin" (treebund--bare-name bare-path)))))

(treebund-deftest do-not-delete-with-worktrees
  ( :remotes (("remote" . ("master")))
    :projects (("some-feature/some-branch" "remote/master")))
  (should (string= "This repository has worktrees checked out"
                   (cadr (should-error
                          (treebund-delete-bare (concat (file-name-as-directory treebund-bare-dir) "remote.git"))
                          :type 'treebund-error)))))

(treebund-deftest do-not-delete-with-unpushed-changes
  ( :remotes (("remote" . ("master")))
    :projects (("some-feature/some-branch" "remote/master")))
  (let* ((bare-path (concat (file-name-as-directory treebund-bare-dir) "remote.git"))
         (project-path (concat (file-name-as-directory treebund-workspace-root) "some-feature/some-branch")))
    ;; Create and commit an empty file
    (let ((test-file (concat (file-name-as-directory project-path) "unpushed")))
      (with-temp-buffer (write-file test-file))
      (treebund--git-with-repo project-path "add" test-file)
      (treebund--git-with-repo project-path "commit" "-m" "unpushed-commit"))
    (treebund-remove-project project-path)
    (should (string= (format "%s has unpushed commits on some branches." (treebund--bare-name bare-path))
                     (cadr (should-error
                            (treebund-delete-bare bare-path)
                            :type 'treebund-error))))))

(treebund-deftest do-not-delete-outside-workspace-root ()
  (let ((project-path (concat (file-name-as-directory treebund-test--dir) "test-project.git")))
    (make-directory project-path t)
    (treebund--git-with-repo project-path "init" "--bare")
    (should (string= "Bare not within workspace root"
                     (cadr (should-error
                            (treebund-delete-bare project-path)
                            :type 'treebund-error))))))

(treebund-deftest current-workspace
  ( :remotes (("remote" . ("master")))
    :projects (("some-workspace/some-project" "remote/master")))
  (make-directory (concat (file-name-as-directory treebund-workspace-root) "test-project"))
  (with-temp-buffer
    (let ((buffer-file-name (expand-file-name "../" treebund-workspace-root)))
      (should-not (treebund-current-workspace)))

    (let ((buffer-file-name treebund-workspace-root))
      (should-not (treebund-current-workspace)))

    (let ((buffer-file-name (concat (file-name-as-directory treebund-workspace-root) "some-workspace")))
      (should (string= (concat (file-name-as-directory treebund-test--dir) "workspaces/some-workspace/")
                       (treebund-current-workspace))))
    (let ((buffer-file-name (concat (file-name-as-directory treebund-workspace-root) "some-workspace/some-project")))
      (should (string= (concat (file-name-as-directory treebund-test--dir) "workspaces/some-workspace/")
                       (treebund-current-workspace))))))

(provide 'treebund-tests)

;;; treebund-tests.el ends here
