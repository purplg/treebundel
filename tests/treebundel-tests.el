;;; treebundel-tests.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'treebundel)
(require 'vc)

(setopt inhibit-message t) ;; Set to nil to print (message)'s
(when nil ;; Set to t to log to a file for debugging tests failures.
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

(defvar tt-dir (file-name-concat temporary-file-directory "treebundel-tests"))
(defvar tt-remotes (file-name-concat tt-dir "remote/"))
(setopt treebundel-workspace-root (file-name-concat tt-dir "workspaces/"))
(setopt treebundel-bare-dir (file-name-concat treebundel-workspace-root ".bare/"))

(defmacro tt-remote-path (name)
  `(file-name-concat
    tt-remotes
    (file-name-with-extension ,name ".git")))

(defmacro tt-bare-path (name)
  `(file-name-concat
    treebundel-bare-dir
    (file-name-with-extension ,name ".git")))

(defmacro tt-workspace-path (name)
  `(file-name-concat
    treebundel-workspace-root
    ,name))

(defmacro tt-project-path (workspace project)
  `(file-name-concat
    (tt-workspace-path ,workspace)
    ,project))

(defun tt-setup (remote-name branches &optional num-commits)
  (unless (listp branches) (setq branches (list branches)))
  ;; Ensure remote exists.
  (unless (file-exists-p (tt-remote-path remote-name))
    (treebundel--git
      "init" "--bare" (tt-remote-path remote-name)))
  (make-directory (file-name-concat tt-remotes "branches" remote-name) t)
  (dolist (branch-name branches)
    (let ((repo-path (file-name-concat tt-remotes
                                       "branches"
                                       remote-name
                                       branch-name))
          (remote-path (tt-remote-path remote-name)))
      (unless (file-exists-p (file-name-concat remote-path
                                               "refs/heads/"
                                               branch-name))
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
    (delete-directory (file-name-concat tt-remotes "branches" branch-name) t))
  (delete-directory (file-name-concat tt-remotes "branches") t))

(describe nil
  :var* ((workspace "workspace-one")
         (remote "remote-a"))
  (before-all (make-directory tt-dir t)
              (make-directory tt-remotes t)
              (tt-setup "remote-a" "main")
              (tt-setup "remote-a" "branch-1")
              (tt-setup "remote-a" "branch-2"))
  (before-each (make-directory (tt-workspace-path workspace) t))
  (after-each (delete-directory treebundel-workspace-root t))
  (after-all (delete-directory tt-dir t))

  (describe "safety"
    (before-each (treebundel-clone (tt-remote-path remote)))

    (it "should prevent deletion of projects with uncommitted files"
      (treebundel--project-add (tt-workspace-path workspace) (tt-bare-path remote))
      (with-temp-buffer
        (write-file (file-name-concat
                     (tt-project-path workspace remote)
                     "test-file")))
      (should-error
       (treebundel-remove-project (tt-project-path workspace remote))
       :type 'treebundel-error))

    (it "should prevent deletion of bares with unpushed commits"
      (treebundel--project-add (tt-workspace-path workspace) (tt-bare-path remote))
      (with-temp-buffer
        (write-file (file-name-concat
                     (tt-project-path workspace remote)
                     "test-file")))
      (treebundel--git-with-repo (tt-project-path workspace remote)
        "add" "-A")
      (treebundel--git-with-repo (tt-project-path workspace remote)
        "commit" "-m" "\"test message\"")
      (treebundel-remove-project (tt-project-path workspace remote))
      (should-error
       (treebundel-delete-bare (tt-bare-path remote))
       :type 'treebundel-error)))
  
  (describe "projects"
    (before-each (treebundel-clone (tt-remote-path remote)))
    (it "should be created in workspaces"
      (treebundel--project-add (tt-workspace-path workspace) (tt-bare-path remote))
      (expect (file-directory-p (tt-project-path workspace
                                                 remote))))
      
    (it "should generate branch name"
      (treebundel--project-add (tt-workspace-path workspace) (tt-bare-path remote))
      (expect (treebundel--git-with-repo (tt-project-path workspace
                                                          remote)
                "rev-parse" "--abbrev-ref" "HEAD")
              :to-equal "feature/workspace-one"))

    (it "should use the specified branch"
      (treebundel--project-add (tt-workspace-path workspace) (tt-bare-path remote) "new-branch")
      (expect (treebundel--git-with-repo (tt-project-path workspace
                                                          remote)
                "rev-parse" "--abbrev-ref" "HEAD")
              :to-equal "new-branch"))

    (it "should use the specified project-name"
      (treebundel--project-add (tt-workspace-path workspace)
                               (tt-bare-path remote)
                               nil
                               (tt-project-path workspace
                                                "test-project-name"))
      (expect (file-directory-p (tt-project-path workspace
                                                 "test-project-name"))
              :to-be t)))
  
  (describe "cloning"
    (it "should create bare repo"
      (treebundel-clone (tt-remote-path remote))
      (expect (file-directory-p (tt-bare-path remote)) :to-be t)))

  (describe "current-workspace"
    (it "should work with nil"
      (expect (treebundel-current-workspace nil) :to-be nil))

    (it "should not work outside root"
      (let ((default-directory (expand-file-name "../" treebundel-workspace-root)))
        (expect (treebundel-current-workspace) :to-be nil)))

    (it "should work in a workspace"
      (let ((default-directory (file-name-concat treebundel-workspace-root "workspace-one/")))
        (expect (treebundel-current-workspace)
                :to-equal
                (file-name-concat tt-dir "workspaces/workspace-one/"))))

    (it "should work in a project"
      (let ((default-directory (file-name-concat treebundel-workspace-root "workspace-one/feature")))
        (expect (treebundel-current-workspace)
                :to-equal
                (file-name-concat tt-dir "workspaces/workspace-one/"))))))

(provide 'treebundel-tests)
;;; treebundel-tests.el ends here.
;; Local Variables
;; read-symbol-shorthands: (("tt- . "treebundel-tests-"))
;;
;; End:
