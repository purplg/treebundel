;;; treebundel-tests.el --- -*- lexical-binding: t; treebundel-workspace-root: "/tmp/treebundel-tests/workspaces"; treebundel-bare-dir: "/tmp/treebundel-tests/workspaces/.bare/" -*-
;;; Commentary:
;;; Code:
(require 'treebundel)
(require 'vc)

(defvar treebundel-test-debug nil
  "Set to t to log to a file for debugging tests failures.")

(setopt inhibit-message (not treebundel-test-debug))
(when treebundel-test-debug
  (setopt inhibit-message nil)
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

(defvar treebundel-tests-dir (file-name-concat temporary-file-directory "treebundel-tests"))
(defvar treebundel-tests-remotes (file-name-concat treebundel-tests-dir "remote/"))
(setopt treebundel-workspace-root (file-name-concat treebundel-tests-dir "workspaces/"))
(setopt treebundel-bare-dir (file-name-concat treebundel-workspace-root ".bare/"))

(defun treebundel-tests-setup (remote-name branches &optional num-commits)
  (unless (listp branches) (setq branches (list branches)))
  ;; Ensure remote exists.
  (let ((remote-path (file-name-concat
                      treebundel-tests-remotes
                      (file-name-with-extension remote-name ".git"))))
    (unless (file-exists-p remote-path)
      (treebundel--git
        "init" "--bare" remote-path))
    (make-directory (file-name-concat treebundel-tests-remotes "branches" remote-name) t)
    (dolist (branch-name branches)
      (unless (file-exists-p (file-name-concat remote-path
                                               "refs/heads/"
                                               branch-name))
        (let ((repo-path (file-name-concat treebundel-tests-remotes
                                           "branches"
                                           remote-name
                                           branch-name)))
          (treebundel--git "clone" remote-path repo-path)
          (treebundel--git-with-repo repo-path "checkout" "-b" branch-name)
          (dotimes (i (or num-commits 3))
            (let ((test-file (file-name-concat repo-path "some-file")))
              (with-temp-buffer (insert i) (write-file test-file))
              (treebundel--git-with-repo repo-path "add" test-file)
              (treebundel--git-with-repo repo-path
                "commit" "-m" (concat "commit-" (int-to-string i)))))
          (treebundel--git-with-repo repo-path
            "push" "--set-upstream" "origin" branch-name)))
      (delete-directory (file-name-concat treebundel-tests-remotes "branches" branch-name) t)))
  (delete-directory (file-name-concat treebundel-tests-remotes "branches") t))

(describe nil
  :var* ((workspace "workspace-one")
         (remote "remote-a")
         (remote-path (file-name-concat
                       treebundel-tests-remotes
                       (file-name-with-extension remote ".git")))
         (project remote)
         (project-path (treebundel-project-path workspace project)))
  (before-all (make-directory treebundel-tests-dir t)
              (make-directory treebundel-tests-remotes t)
              (treebundel-tests-setup remote "main")
              (treebundel-tests-setup remote "branch-1")
              (treebundel-tests-setup remote "branch-2"))
  (before-each (make-directory (treebundel-workspace-path workspace) t))
  (after-each (delete-directory treebundel-workspace-root t))
  (after-all (delete-directory treebundel-tests-dir t))

  (describe "safety"
    (before-each (treebundel--clone remote-path)
                 (treebundel--project-add workspace remote))

    (it "should detect clean repositories"
      (expect (treebundel--repo-clean-p project-path))
      (with-temp-buffer
        (write-file (file-name-concat
                     (treebundel-project-path workspace remote)
                     "test-file")))
      (expect :not (treebundel--repo-clean-p project-path)))
    
    (it "should prevent deletion of projects with uncommitted files"
      (with-temp-buffer
        (write-file (file-name-concat
                     (treebundel-project-path workspace remote)
                     "test-file")))
      (should-error
       (treebundel-remove-project workspace remote)
       :type 'treebundel-error))

    (it "should prevent deletion of bares with unpushed commits"
      (with-temp-buffer
        (write-file (file-name-concat
                     (treebundel-project-path workspace remote)
                     "test-file")))
      (treebundel--git-with-repo project-path
        "add" "-A")
      (treebundel--git-with-repo project-path
        "commit" "-m" "\"test message\"")
      (treebundel-remove-project workspace remote)
      (should-error
       (treebundel-delete-bare remote)
       :type 'treebundel-error)))
  
  (describe "projects"
    (before-each (treebundel--clone remote-path))
    (it "should be created in workspaces"
      (treebundel--project-add workspace remote)
      (expect (file-directory-p (treebundel-project-path workspace
                                                 remote))))
      
    (it "should generate branch name"
      (treebundel--project-add workspace remote)
      (expect (treebundel--git-with-repo (treebundel-project-path workspace
                                                          remote)
                "rev-parse" "--abbrev-ref" "HEAD")
              :to-equal (concat treebundel-branch-prefix workspace)))

    (it "should use the specified branch"
      (treebundel--project-add workspace remote "new-branch")
      (expect (treebundel--git-with-repo (treebundel-project-path workspace
                                                          remote)
                "rev-parse" "--abbrev-ref" "HEAD")
              :to-equal "new-branch"))

    (it "should use the specified project-name"
      (treebundel--project-add workspace
                               remote
                               nil
                               "test-project-name")
      (expect (file-directory-p (treebundel-project-path workspace
                                                 "test-project-name"))
              :to-be t)))
  
  (describe "cloning"
    (it "should create bare repo"
      (treebundel--clone remote-path)
      (expect (file-directory-p (treebundel-bare-path remote)) :to-be t)))

  (describe "current-workspace"
    (it "should work with nil"
      (expect (treebundel-current-workspace nil) :to-be nil))

    (it "should not work outside root"
      (let ((default-directory (expand-file-name "../" treebundel-workspace-root)))
        (expect (treebundel-current-workspace) :to-be nil)))

    (it "should work in a workspace"
      (let ((default-directory (file-name-concat treebundel-workspace-root workspace)))
        (expect "workspace-one" :to-equal (treebundel-current-workspace))))

    (it "should work in a project"
      (let ((default-directory (file-name-concat treebundel-workspace-root workspace "feature")))
        (expect "workspace-one" :to-equal (treebundel-current-workspace))))))

(provide 'treebundel-tests)
;;; treebundel-tests.el ends here.
