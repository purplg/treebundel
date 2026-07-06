;;; treebundel-tests.el --- Description -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'ert)
(require 'treebundel)

(ert-deftest test-fmt-bare ()
  (should (string= (--fmt-bare) "⸺"))
  (should (string= (--fmt-bare "thebare") "thebare")))

(ert-deftest test-fmt-workspace ()
  (should (string= (--fmt-workspace) "⸺/"))
  (should (string= (--fmt-workspace "theworkspace") "theworkspace/")))

(ert-deftest test-fmt-project ()
  (should (string= (--fmt-project) "⸺"))
  (should (string= (--fmt-project "theproject") "theproject")))

(ert-deftest test-fmt-workspace-project ()
  (should (string= (--fmt-workspace-project) "⸺/⸺"))
  (should (string= (--fmt-workspace-project nil "theproject") "⸺/theproject"))
  (should (string= (--fmt-workspace-project "theworkspace") "theworkspace/⸺"))
  (should (string= (--fmt-workspace-project "theworkspace" "theproject") "theworkspace/theproject")))

(ert-deftest test-current-workspace ()
  (let* ((-workspace-root "/tmp/treebundel-tests/"))
    (should-not (-current-workspace "/"))
    (should-not (-current-workspace "/home/user/dir"))
    (should-not (-current-workspace "/tmp/treebundel-tests"))
    (should-not (-current-workspace "/tmp/treebundel-tests/"))
    (should (string= (-current-workspace "/tmp/treebundel-tests/test-ws") "test-ws"))
    (should (string= (-current-workspace "/tmp/treebundel-tests/test-ws/") "test-ws"))
    (should (string= (-current-workspace "/tmp/treebundel-tests/test-ws/two") "test-ws"))
    (should (string= (-current-workspace "/tmp/treebundel-tests/test-ws/two/three") "test-ws"))))

(ert-deftest test-project-current ()
  (let* ((-workspace-root "/tmp/treebundel-tests/"))
    (should-not (--project-current "/"))
    (should-not (--project-current "/home/user/dir"))
    (should-not (--project-current "/tmp/treebundel-tests"))
    (should-not (--project-current "/tmp/treebundel-tests/"))
    (should (string= (--project-current "/tmp/treebundel-tests/test-ws/test-project") "test-project"))
    (should (string= (--project-current "/tmp/treebundel-tests/test-ws/test-project/three") "test-project"))))

(provide '-tests)
;;; tests.el ends here
;; Local Variables:
;; treebundel-workspace-root: "/tmp/workspace"
;; read-symbol-shorthands: (("test-" . "treebundel-test--")
;;                          ("-" . "treebundel-"))
;; End:
