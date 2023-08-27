;;; treebundel-tests.el --- Bundle related git-worktrees together -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)

(require 'compat)
(require 'treebundel)


;;; Logging:
(setq ert-quiet nil)

(setq-default treebundel-test-logging nil)

(when treebundel-test-logging
  (defun treebundel--gitlog (type &rest msg)
    "Overridden logging method to write to a file instead of buffer."
    (with-temp-buffer
      (goto-char (point-max))
      (cond ((eq 'command type)
             (insert "==> git " (string-join msg " ") ?\n))
            ((eq 'output type)
             (let ((msg (apply #'format msg)))
               (when (length= msg 0)
                 (setq msg " "))
               (insert msg))
             (newline 2)))
      (append-to-file nil nil (file-name-concat temporary-file-directory "treebundel-test-git.log")))))


;;; Environment:
(setq treebundel-test--dir (file-name-concat temporary-file-directory "treebundel-tests/"))
(setq treebundel-remote--dir (file-name-concat treebundel-test--dir "simulated-remote/"))

(defun treebundel-test--setup-branch (name origin-path &optional num-commits)
  (let ((worktree-path (expand-file-name name treebundel-remote--dir)))
    (treebundel--git "clone" origin-path worktree-path)
    (treebundel--git-with-repo worktree-path
      "checkout" "-b" name)
    (dotimes (i (or num-commits 3))
      (let ((test-file (expand-file-name "some-file" worktree-path)))
        (with-temp-buffer
          (insert i)
          (write-file test-file))
        (treebundel--git-with-repo worktree-path
          "add" test-file)
        (treebundel--git-with-repo worktree-path
          "commit" "-m" (concat "commit-" (int-to-string i)))))
    (treebundel--git-with-repo worktree-path "push" "--set-upstream" "origin" name)
    (delete-directory worktree-path t)))

(defun treebundel-test--setup-remote (name branches)
  "Setup simulated remote.
NAME is the name of the remote repository.

BRANCHES is a list of branch names to be created in this
remote. Each branch will have 2 commits added."
  (let ((origin-path (expand-file-name (file-name-with-extension name ".git") treebundel-remote--dir)))
    (treebundel--git "init" "--bare" origin-path)
    (dolist (branch branches)
      (treebundel-test--setup-branch branch origin-path 3))
    (make-directory treebundel-bare-dir t)))

(defun treebundel-test--setup-project (worktree-path remote-name branch)
  "Setup simulated remote.
NAME is the name of the remote repository.

BRANCHES is a list of branch names to be created in this
remote. Each branch will have 2 commits added."
  ;; Clone the repo if it hasn't been cloned from simulated-remote yet
  (let ((bare-path (file-name-concat treebundel-bare-dir
                                     (file-name-with-extension remote-name ".git")))
        (worktree-path (file-name-concat treebundel-workspace-root worktree-path)))
    (make-directory worktree-path t)
    (unless (file-exists-p bare-path)
      (treebundel--clone (file-name-concat treebundel-remote--dir
                                         (file-name-with-extension remote-name ".git"))))
    (treebundel--worktree-add bare-path worktree-path branch)))

(defun treebundel-test--setup (remotes projects)
  "Setup testing environment.
REMOTES is a list of cons cells of remote names to a list of
branch names. For example:

\\=(:remotes ((\"origin-one\" . (\"branch-one\" \"branch-two\"))
           (\"origin-two\" . (\"branch-one\")))
 :projects ((\"workspace/project-one-path\" \"origin-one\" \"branch-one\"))
            (\"workspace/project-two-path\" \"origin-one\" \"branch-two\")))

Each created branch will have 2 commits."
  ;; Create temp directory.
  (when (file-directory-p treebundel-test--dir)
    (delete-directory treebundel-test--dir t))
  (make-directory treebundel-test--dir)
  (make-directory treebundel-workspace-root)
  (make-directory treebundel-remote--dir)

  (let ((remote (pop remotes)))
    (while remote
      (treebundel-test--setup-remote (car remote)
                                   (cdr remote))
      (setq remote (pop remotes))))

  (let ((project (pop projects)))
    (while project
      (let* ((worktree-path (pop project))
             (remote-branch (split-string (pop project) "/")))
        (treebundel-test--setup-project worktree-path (pop remote-branch) (pop remote-branch)))
      (setq project (pop projects)))))

(defmacro treebundel-deftest (name test-args &rest body)
  "Wrapper around `ert-deftest' to ensure correct tmp directories
are used for all tests."
  (declare (indent defun)
           (doc-string 3))
  `(ert-deftest ,name ()
     ,(when (stringp (car body))
        (pop body))
     (let* ((inhibit-message nil)
            (treebundel-workspace-root (concat treebundel-test--dir "workspaces/"))
            (treebundel-bare-dir (file-name-concat treebundel-workspace-root ".bare/"))
            (treebundel-project-open-function (lambda (&rest _)))
            (treebundel-prefix "test/"))
       (treebundel-test--setup
        (plist-get ',test-args ':remotes)
        (plist-get ',test-args ':projects))
       ,@body)))


;;; Tests:
(treebundel-deftest setup
  ( :remotes (("remote-one" . ())
              ("remote-two" . ("branch-one"))
              ("remote-three" . ("branch-one" "branch-two")))
    :projects (("some-workspace/project-two-one" "remote-two/branch-one")
               ("some-workspace/project-three-one" "remote-three/branch-one")
               ("some-workspace/project-three-two" "remote-three/branch-two")))
  "The basic testing environment used for all treebundel tests."
  (let ((origin (expand-file-name "remote-one.git" treebundel-remote--dir)))
    (should (file-directory-p origin))
    (should-not (treebundel--branches origin)))

  (let ((origin (expand-file-name "remote-two.git" treebundel-remote--dir)))
    (should (file-directory-p origin))
    (should (length= (treebundel--branches origin) 1))
    (should (member "branch-one" (treebundel--branches origin))))

  (let ((origin (expand-file-name "remote-three.git" treebundel-remote--dir)))
    (should (file-directory-p origin))
    (should (length= (treebundel--branches origin) 2))
    (should (member "branch-one" (treebundel--branches origin)))
    (should (member "branch-two" (treebundel--branches origin))))

  (let* ((workspace (concat treebundel-workspace-root "some-workspace"))
         (project-two-one (file-name-concat workspace "project-two-one"))
         (project-three-one (file-name-concat workspace "project-three-one"))
         (project-three-two (file-name-concat workspace "project-three-two")))
    (should (file-directory-p workspace))
    (should (file-directory-p project-two-one))
    (should (file-directory-p project-three-one))
    (should (file-directory-p project-three-two))))

(treebundel-deftest branches
  (:remotes (("remote" . ("test/branches" "test/branches-two"))
             ("empty-remote" . ())))
  (let* ((workspace-path (concat treebundel-workspace-root "branches"))
         (remote (concat treebundel-remote--dir "remote.git"))
         (bare-path (treebundel--clone remote))
         (project-path (treebundel--project-add workspace-path bare-path)))
    ; Check bare repository branches
    (should (length= (treebundel--branches bare-path) 2))
    (should (member "test/branches" (treebundel--branches remote)))
    (should (member "test/branches-two" (treebundel--branches remote)))
    ; Check worktree branches
    (should (length= (treebundel--branches project-path) 2))
    (should (member "test/branches" (treebundel--branches project-path)))
    (should (member "test/branches-two" (treebundel--branches project-path))))

  ; Ensure nil is returned when no branches exist.
  (should (length= 0 (treebundel--branches
                      (treebundel--clone
                       (file-name-concat treebundel-remote--dir "empty-remote.git"))))))

(treebundel-deftest worktree-bare
  (:remotes (("remote" . ("test/worktree-bare"))))
  (let* ((workspace-path (concat treebundel-workspace-root "worktree-bare"))
         (remote (expand-file-name "remote.git" treebundel-remote--dir))
         (bare-path (treebundel--clone remote))
         (project-path (treebundel--project-add workspace-path bare-path)))
    (should (equal bare-path (treebundel--worktree-bare project-path)))))

(treebundel-deftest worktree-remove
  (:remotes (("remote" . ("test/worktree-remove"))))
  (let* ((workspace-path (concat treebundel-workspace-root "worktree-remove/"))
         (remote (concat treebundel-remote--dir "remote.git"))
         (bare-path (treebundel--clone remote))
         (project-path (treebundel--project-add workspace-path bare-path)))
    (treebundel--worktree-remove project-path)
    (should-not (file-exists-p project-path))))

(treebundel-deftest worktree-add
  (:remotes (("remote" . ("test/worktree-add"))))
  (let* ((workspace-path (expand-file-name "worktree-add" treebundel-workspace-root))
         (remote (expand-file-name "remote.git" treebundel-remote--dir))
         (bare-path (treebundel--clone remote))
         (project-path (treebundel--project-add workspace-path bare-path)))
    (should (file-directory-p project-path))))

(treebundel-deftest branch
  (:remotes (("remote" . ("test/branch"))))
  (let* ((workspace-path (expand-file-name "branch" treebundel-workspace-root))
         (remote (expand-file-name "remote.git" treebundel-remote--dir))
         (bare-path (treebundel--clone remote))
         (project-path (treebundel--project-add workspace-path bare-path)))
    (should (equal "test/branch" (treebundel--branch project-path)))))

(treebundel-deftest clone
  (:remotes (("remote" . ("master" "other-branch"))))
  (let* ((workspace-path (expand-file-name "branch-delete" treebundel-workspace-root))
         (remote (expand-file-name "remote.git" treebundel-remote--dir))
         (bare-path (treebundel--clone remote)))
    (should (file-directory-p bare-path))
    (should (length= (treebundel--branches bare-path) 2))
    (should (member "master" (treebundel--branches bare-path)))
    (should (member "other-branch" (treebundel--branches bare-path)))))

(treebundel-deftest worktree-list
  (:remotes (("remote" . ("master"))))
  (let* ((remote (expand-file-name "remote.git" treebundel-remote--dir))
         (bare-path (treebundel--clone remote)))
    (should (length= (treebundel--worktree-list bare-path) 1))
    (should (member "bare" (car (treebundel--worktree-list bare-path))))
    (should (length= (treebundel--worktree-list
                      (treebundel--project-add
                       (expand-file-name "worktree-list-one" treebundel-workspace-root)
                       bare-path)) 2))
    (should-not (member "bare" (cadr (treebundel--worktree-list bare-path))))
    (should (length= (treebundel--worktree-list
                      (treebundel--project-add
                       (expand-file-name "worktree-list-two" treebundel-workspace-root)
                       bare-path)) 3))))

(treebundel-deftest rev-count
  (:remotes (("remote" . ("master" "other-branch"))))
  (should (equal 3 (treebundel--rev-count
                    (treebundel--clone (expand-file-name "remote.git" treebundel-remote--dir))
                    "other-branch"))))

(treebundel-deftest repo-worktree-count
  (:remotes (("remote" . ("master"))))
  (let* ((remote (expand-file-name "remote.git" treebundel-remote--dir))
         (bare-path (treebundel--clone remote)))

    (should (equal 0 (treebundel--repo-worktree-count bare-path)))

    (treebundel--project-add
     (expand-file-name "repo-worktree-count-one" treebundel-workspace-root)
     bare-path)
    (should (equal 1 (treebundel--repo-worktree-count bare-path)))

    (treebundel--project-add
     (expand-file-name "repo-worktree-count-two" treebundel-workspace-root)
     bare-path)
    (should (equal 2 (treebundel--repo-worktree-count bare-path)))))

(treebundel-deftest has-worktrees-p
  (:remotes (("remote" . ("master"))))
  (let* ((remote (expand-file-name "remote.git" treebundel-remote--dir))
         (bare-path (treebundel--clone remote)))
    (should-not (treebundel--has-worktrees-p bare-path))
    (treebundel--project-add
     (expand-file-name "repo-worktree-count-one" treebundel-workspace-root)
     bare-path)
    (should (treebundel--has-worktrees-p bare-path))))

(treebundel-deftest unpushed-commits-p
  ( :remotes (("origin" . ("master" "other-branch")))
    :projects (("some-feature/master" . ("origin/master"))
               ("some-feature/other" . ("origin/other"))))
  (let ((master-path (file-name-concat treebundel-workspace-root "some-feature/master"))
        (other-path (file-name-concat treebundel-workspace-root "some-feature/other")))
    ;; A freshly clone repository should not be dirty.
    (should-not (treebundel--unpushed-commits-p master-path))

    ;; Create and commit an empty file
    (let ((test-file (file-name-concat master-path "unpushed")))
      (with-temp-buffer (write-file test-file))
      (treebundel--git-with-repo master-path "add" test-file)
      (treebundel--git-with-repo master-path "commit" "-m" "unpushed-commit"))

    ;; Since we have an unpushed commit, this should pass now.
    (should (treebundel--unpushed-commits-p master-path))

    ;; Other project should also reflect unpushed commits
    (should (treebundel--unpushed-commits-p other-path))

    ;; Push and it should now pass.
    (treebundel--git-with-repo master-path "push" "--set-upstream" "origin" "master")
    (should-not (treebundel--unpushed-commits-p master-path))))

(treebundel-deftest project-clean-p
  ( :remotes (("origin" . ("master" "other-branch")))
    :projects (("some-feature/master" . ("origin/master"))))
  (let* ((project-path (file-name-concat treebundel-workspace-root "some-feature/master"))
         (test-file (file-name-concat project-path "unpushed")))

    ;; A fresh project should be clean.
    (should (treebundel--project-clean-p project-path))

    ;; An untracked file should be dirty.
    (with-temp-buffer (write-file test-file))
    (should-not (treebundel--project-clean-p project-path))

    ;; Tracking the file should still be dirty.
    (treebundel--git-with-repo project-path "add" test-file)
    (should-not (treebundel--project-clean-p project-path))

    ;; Committing the file should make the project clean.
    (treebundel--git-with-repo project-path "commit" "-m" "some-commit")
    (should (treebundel--project-clean-p project-path))))

(treebundel-deftest bare-name
  ( :remotes (("origin" . ("master")))
    :projects (("some-feature/feature" "origin/master")))
  (should (equal "origin" (treebundel--bare-name (file-name-concat treebundel-bare-dir "origin.git"))))
  (should-not (treebundel--bare-name (file-name-concat treebundel-workspace-root "some-feature/feature"))))

(treebundel-deftest do-not-delete-with-worktrees
  ( :remotes (("remote" . ("master")))
    :projects (("some-feature/some-branch" "remote/master")))
  (should (equal "This repository has worktrees checked out"
                   (cadr (should-error
                          (treebundel-delete-bare (file-name-concat treebundel-bare-dir "remote.git"))
                          :type 'treebundel-error)))))

(treebundel-deftest do-not-delete-with-unpushed-changes
  ( :remotes (("remote" . ("master")))
    :projects (("some-feature/some-branch" "remote/master")))
  (let* ((bare-path (file-name-concat treebundel-bare-dir "remote.git"))
         (project-path (file-name-concat treebundel-workspace-root "some-feature/some-branch")))
    ;; Create and commit an empty file
    (let ((test-file (file-name-concat project-path "unpushed")))
      (with-temp-buffer (write-file test-file))
      (treebundel--git-with-repo project-path "add" test-file)
      (treebundel--git-with-repo project-path "commit" "-m" "unpushed-commit"))
    (treebundel-remove-project project-path)
    (should (equal (format "%s has unpushed commits on some branches" (treebundel--bare-name bare-path))
                     (cadr (should-error
                            (treebundel-delete-bare bare-path)
                            :type 'treebundel-error))))))

(treebundel-deftest do-not-delete-outside-workspace-root ()
  (let ((project-path (file-name-concat treebundel-test--dir "test-project.git")))
    (make-directory project-path t)
    (treebundel--git-with-repo project-path "init" "--bare")
    (should (equal "Bare not within workspace root"
                     (cadr (should-error
                            (treebundel-delete-bare project-path)
                            :type 'treebundel-error))))))

(treebundel-deftest current-workspace
  ( :remotes (("remote" . ("master")))
    :projects (("some-workspace/some-project" "remote/master")))
  (make-directory (file-name-concat treebundel-workspace-root "test-project"))
  (with-temp-buffer
    ;; Should handle nil
    (treebundel-current-workspace nil)

    (let ((buffer-file-name (expand-file-name "../" treebundel-workspace-root)))
      (should-not (treebundel-current-workspace)))

    (let ((buffer-file-name treebundel-workspace-root))
      (should-not (treebundel-current-workspace)))

    (let ((buffer-file-name (file-name-concat treebundel-workspace-root "some-workspace")))
      (should (equal (file-name-concat treebundel-test--dir "workspaces/some-workspace/")
                       (treebundel-current-workspace))))

    (let ((buffer-file-name (file-name-concat treebundel-workspace-root "some-workspace/some-project")))
      (should (equal (file-name-concat treebundel-test--dir "workspaces/some-workspace/")
                       (treebundel-current-workspace))))))

(provide 'treebundel-tests)

;;; treebundel-tests.el ends here
