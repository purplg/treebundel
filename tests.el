;;; treebundel-tests.el --- Description -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'ert)
(require 'treebundel)

(ert-deftest test-fmt-bare ()
  (should (string= (--fmt-bare nil) "⸺"))
  (should (string= (--fmt-bare "thebare") "thebare")))

(ert-deftest test-fmt-workspace ()
  (should (string= (--fmt-workspace nil) "⸺/"))
  (should (string= (--fmt-workspace "theworkspace") "theworkspace/")))

(ert-deftest test-fmt-project ()
  (should (string= (--fmt-project nil) "⸺"))
  (should (string= (--fmt-project "theproject") "theproject")))

(ert-deftest test-fmt-workspace-project ()
  (should (string= (--fmt-workspace-project nil nil) "⸺/⸺"))
  (should (string= (--fmt-workspace-project nil "theproject") "⸺/⸺"))
  (should (string= (--fmt-workspace-project "theworkspace" nil) "theworkspace/⸺"))
  (should (string= (--fmt-workspace-project "theworkspace" "theproject") "theworkspace/theproject")))

(ert-deftest test-workspace-of ()
  (let* ((-workspace-root "/tmp/treebundel-tests/"))
    (should-not (--workspace-of "/"))
    (should-not (--workspace-of "/home/treebundel-user/dir"))
    (should-not (--workspace-of "/tmp/treebundel-tests"))
    (should-not (--workspace-of "/tmp/treebundel-tests/"))
    (should (string= (--workspace-of "/tmp/treebundel-tests/test-ws") "test-ws"))
    (should (string= (--workspace-of "/tmp/treebundel-tests/test-ws/") "test-ws"))
    (should (string= (--workspace-of "/tmp/treebundel-tests/test-ws/two") "test-ws"))
    (should (string= (--workspace-of "/tmp/treebundel-tests/test-ws/two/three") "test-ws"))))

(ert-deftest test-project-of ()
  (let* ((-workspace-root "/tmp/treebundel-tests/"))
    (should-not (--project-of "/"))
    (should-not (--project-of "/home/treebundel-user/dir"))
    (should-not (--project-of "/tmp/treebundel-tests"))
    (should-not (--project-of "/tmp/treebundel-tests/"))
    (should (string= (--project-of "/tmp/treebundel-tests/test-ws/test-project") "test-project"))
    (should (string= (--project-of "/tmp/treebundel-tests/test-ws/test-project/three") "test-project"))))

(defvar test-matrix
  `((:scope ,(treebundel-scope :workspace ".bare" :project "repo.git")
     :workspace-p nil
     :project-p nil
     :bare-p t)

    (:scope ,(treebundel-scope :workspace ".bare" :project "repo")
     :workspace-p nil
     :project-p nil
     :bare-p nil)

    (:scope ,(treebundel-scope :workspace ".bare" :project nil)
     :workspace-p nil
     :project-p nil
     :bare-p nil)

    (:scope ,(treebundel-scope :workspace ".hidden" :project "repo")
     :workspace-p nil
     :project-p nil
     :bare-p nil)

    (:scope ,(treebundel-scope :workspace "bare" :project nil)
     :workspace-p t :project-p nil :bare-p nil)

    (:scope ,(treebundel-scope :workspace "bare" :project "repo.git")
     :workspace-p t
     :project-p t
     :bare-p nil)

    (:scope ,(treebundel-scope :workspace "somews" :project "somepj")
     :workspace-p t
     :project-p t
     :bare-p nil)))

(ert-deftest test-scope-predicates ()
  (dolist (current-test test-matrix)
    (let* ((scope (plist-get current-test :scope))
           (workspace-p (equal (plist-get current-test :workspace-p) (treebundel-scope-workspace-p scope)))
           (project-p (equal (plist-get current-test :project-p) (treebundel-scope-project-p scope)))
           (bare-p (equal (plist-get current-test :bare-p) (treebundel-scope-bare-p scope))))
      (unless workspace-p (ert-fail (format "workspace-p %s %s" (plist-get current-test :workspace-p) (plist-get current-test :scope))))
      (unless project-p (ert-fail (format "project-p %s %s" (plist-get current-test :project-p) (plist-get current-test :scope))))
      (unless bare-p (ert-fail (format "bare-p %s %s" (plist-get current-test :bare-p) (plist-get current-test :scope)))))))

(provide '-tests)
;;; tests.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("test-" . "treebundel-test--")
;;                          ("-" . "treebundel-"))
;; End:
