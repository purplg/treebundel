;;; treebund-tests.el --- Bundle related git-worktrees together -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)

(require 'compat)
(require 'treebund)


;;; Logging:
(setq ert-quiet nil)

(setq-default treebund-test-logging nil)

(when treebund-test-logging
  (defun treebund--gitlog (type &rest msg)
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
      (append-to-file nil nil (file-name-concat temporary-file-directory "treebund-test-git.log")))))


;;; Environment:
(setq treebund-test--dir (file-name-concat temporary-file-directory "treebund-tests/"))
(setq treebund-remote--dir (file-name-concat treebund-test--dir "simulated-remote/"))

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
  (let ((origin-path (expand-file-name (file-name-with-extension name ".git") treebund-remote--dir)))
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
  (let ((bare-path (file-name-concat treebund-bare-dir
                                     (file-name-with-extension remote-name ".git")))
        (worktree-path (file-name-concat treebund-workspace-root worktree-path)))
    (make-directory worktree-path t)
    (unless (file-exists-p bare-path)
      (treebund--clone (file-name-concat treebund-remote--dir
                                         (file-name-with-extension remote-name ".git"))))
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
            (treebund-bare-dir (file-name-concat treebund-workspace-root ".bare/"))
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
    (should (length= (treebund--branches origin) 1))
    (should (member "branch-one" (treebund--branches origin))))

  (let ((origin (expand-file-name "remote-three.git" treebund-remote--dir)))
    (should (file-directory-p origin))
    (should (length= (treebund--branches origin) 2))
    (should (member "branch-one" (treebund--branches origin)))
    (should (member "branch-two" (treebund--branches origin))))

  (let* ((workspace (concat treebund-workspace-root "some-workspace"))
         (project-two-one (file-name-concat workspace "project-two-one"))
         (project-three-one (file-name-concat workspace "project-three-one"))
         (project-three-two (file-name-concat workspace "project-three-two")))
    (should (file-directory-p workspace))
    (should (file-directory-p project-two-one))
    (should (file-directory-p project-three-one))
    (should (file-directory-p project-three-two))))

(treebund-deftest branches
  (:remotes (("remote" . ("test/branches" "test/branches-two"))
             ("empty-remote" . ())))
  (let* ((workspace-path (concat treebund-workspace-root "branches"))
         (remote (concat treebund-remote--dir "remote.git"))
         (bare-path (treebund--clone remote))
         (project-path (treebund--project-add workspace-path bare-path)))
    ; Check bare repository branches
    (should (length= (treebund--branches bare-path) 2))
    (should (member "test/branches" (treebund--branches remote)))
    (should (member "test/branches-two" (treebund--branches remote)))
    ; Check worktree branches
    (should (length= (treebund--branches project-path) 2))
    (should (member "test/branches" (treebund--branches project-path)))
    (should (member "test/branches-two" (treebund--branches project-path))))

  ; Ensure nil is returned when no branches exist.
  (should (length= (treebund--branches
                    (treebund--clone
                     (file-name-concat treebund-remote--dir "empty-remote.git")))
                   0)))

(treebund-deftest worktree-bare
  (:remotes (("remote" . ("test/worktree-bare"))))
  (let* ((workspace-path (concat treebund-workspace-root "worktree-bare"))
         (remote (expand-file-name "remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote))
         (project-path (treebund--project-add workspace-path bare-path)))
    (should (equal bare-path (treebund--worktree-bare project-path)))))

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
    (should (equal "test/branch" (treebund--branch project-path)))))

(treebund-deftest clone
  (:remotes (("remote" . ("master" "other-branch"))))
  (let* ((workspace-path (expand-file-name "branch-delete" treebund-workspace-root))
         (remote (expand-file-name "remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote)))
    (should (file-directory-p bare-path))
    (should (length= (treebund--branches bare-path) 2))
    (should (member "master" (treebund--branches bare-path)))
    (should (member "other-branch" (treebund--branches bare-path)))))

(treebund-deftest worktree-list
  (:remotes (("remote" . ("master"))))
  (let* ((remote (expand-file-name "remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote)))
    (should (length= (treebund--worktree-list bare-path) 1))
    (should (member "bare" (car (treebund--worktree-list bare-path))))
    (should (length= (treebund--worktree-list
                      (treebund--project-add
                       (expand-file-name "worktree-list-one" treebund-workspace-root)
                       bare-path)) 2))
    (should-not (member "bare" (cadr (treebund--worktree-list bare-path))))
    (should (length= (treebund--worktree-list
                      (treebund--project-add
                       (expand-file-name "worktree-list-two" treebund-workspace-root)
                       bare-path)) 3))))

(treebund-deftest rev-count
  (:remotes (("remote" . ("master" "other-branch"))))
  (should (equal 3 (treebund--rev-count
                    (treebund--clone (expand-file-name "remote.git" treebund-remote--dir))
                    "other-branch"))))

(treebund-deftest repo-worktree-count
  (:remotes (("remote" . ("master"))))
  (let* ((remote (expand-file-name "remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote)))

    (should (equal 0 (treebund--repo-worktree-count bare-path)))

    (treebund--project-add
     (expand-file-name "repo-worktree-count-one" treebund-workspace-root)
     bare-path)
    (should (equal 1 (treebund--repo-worktree-count bare-path)))

    (treebund--project-add
     (expand-file-name "repo-worktree-count-two" treebund-workspace-root)
     bare-path)
    (should (equal 2 (treebund--repo-worktree-count bare-path)))))

(treebund-deftest has-worktrees-p
  (:remotes (("remote" . ("master"))))
  (let* ((remote (expand-file-name "remote.git" treebund-remote--dir))
         (bare-path (treebund--clone remote)))
    (should-not (treebund--has-worktrees-p bare-path))
    (treebund--project-add
     (expand-file-name "repo-worktree-count-one" treebund-workspace-root)
     bare-path)
    (should (treebund--has-worktrees-p bare-path))))

(treebund-deftest unpushed-commits-p
  ( :remotes (("origin" . ("master" "other-branch")))
    :projects (("some-feature/master" . ("origin/master"))
               ("some-feature/other" . ("origin/other"))))
  (let ((master-path (file-name-concat treebund-workspace-root "some-feature/master"))
        (other-path (file-name-concat treebund-workspace-root "some-feature/other")))
    ;; A freshly clone repository should not be dirty.
    (should-not (treebund--unpushed-commits-p master-path))

    ;; Create and commit an empty file
    (let ((test-file (file-name-concat master-path "unpushed")))
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

(treebund-deftest project-clean-p
  ( :remotes (("origin" . ("master" "other-branch")))
    :projects (("some-feature/master" . ("origin/master"))))
  (let* ((project-path (file-name-concat treebund-workspace-root "some-feature/master"))
         (test-file (file-name-concat project-path "unpushed")))

    ;; A fresh project should be clean.
    (should (treebund--project-clean-p project-path))

    ;; An untracked file should be dirty.
    (with-temp-buffer (write-file test-file))
    (should-not (treebund--project-clean-p project-path))

    ;; Tracking the file should still be dirty.
    (treebund--git-with-repo project-path "add" test-file)
    (should-not (treebund--project-clean-p project-path))

    ;; Committing the file should make the project clean.
    (treebund--git-with-repo project-path "commit" "-m" "some-commit")
    (should (treebund--project-clean-p project-path))))

(treebund-deftest bare-name
  ( :remotes (("origin" . ("master")))
    :projects (("some-feature/feature" "origin/master")))
  (should (equal "origin" (treebund--bare-name (file-name-concat treebund-bare-dir "origin.git"))))
  (should-not (treebund--bare-name (file-name-concat treebund-workspace-root "some-feature/feature"))))

(treebund-deftest do-not-delete-with-worktrees
  ( :remotes (("remote" . ("master")))
    :projects (("some-feature/some-branch" "remote/master")))
  (should (equal "This repository has worktrees checked out"
                   (cadr (should-error
                          (treebund-delete-bare (file-name-concat treebund-bare-dir "remote.git"))
                          :type 'treebund-error)))))

(treebund-deftest do-not-delete-with-unpushed-changes
  ( :remotes (("remote" . ("master")))
    :projects (("some-feature/some-branch" "remote/master")))
  (let* ((bare-path (file-name-concat treebund-bare-dir "remote.git"))
         (project-path (file-name-concat treebund-workspace-root "some-feature/some-branch")))
    ;; Create and commit an empty file
    (let ((test-file (file-name-concat project-path "unpushed")))
      (with-temp-buffer (write-file test-file))
      (treebund--git-with-repo project-path "add" test-file)
      (treebund--git-with-repo project-path "commit" "-m" "unpushed-commit"))
    (treebund-remove-project project-path)
    (should (equal (format "%s has unpushed commits on some branches" (treebund--bare-name bare-path))
                     (cadr (should-error
                            (treebund-delete-bare bare-path)
                            :type 'treebund-error))))))

(treebund-deftest do-not-delete-outside-workspace-root ()
  (let ((project-path (file-name-concat treebund-test--dir "test-project.git")))
    (make-directory project-path t)
    (treebund--git-with-repo project-path "init" "--bare")
    (should (equal "Bare not within workspace root"
                     (cadr (should-error
                            (treebund-delete-bare project-path)
                            :type 'treebund-error))))))

(treebund-deftest current-workspace
  ( :remotes (("remote" . ("master")))
    :projects (("some-workspace/some-project" "remote/master")))
  (make-directory (file-name-concat treebund-workspace-root "test-project"))
  (with-temp-buffer
    ;; Should handle nil
    (treebund-current-workspace nil)

    (let ((buffer-file-name (expand-file-name "../" treebund-workspace-root)))
      (should-not (treebund-current-workspace)))

    (let ((buffer-file-name treebund-workspace-root))
      (should-not (treebund-current-workspace)))

    (let ((buffer-file-name (file-name-concat treebund-workspace-root "some-workspace")))
      (should (equal (file-name-concat treebund-test--dir "workspaces/some-workspace/")
                       (treebund-current-workspace))))

    (let ((buffer-file-name (file-name-concat treebund-workspace-root "some-workspace/some-project")))
      (should (equal (file-name-concat treebund-test--dir "workspaces/some-workspace/")
                       (treebund-current-workspace))))))

(provide 'treebund-tests)

;;; treebund-tests.el ends here
