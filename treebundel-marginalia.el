;;; treebundel-marginalia.el --- Marginalia annotations for treebundel completions -*- lexical-binding: t; -*-
;;
;; Package-Requires: ((emacs "30.1") (marginalia "2.11"))
;; Author: Ben Whitley
;; Version: 0.0.1
;; Keywords: convenience docs extensions help vc
;; Homepage: https://github.com/purplg/treebundel-marginalia
;; SPDX-License-Identifier: MIT
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'treebundel)
(require 'marginalia)

(defvar treebundel-marginalia-categories '((treebundel-bare . treebundel-bare)
                                           (treebundel-read-bare . treebundel-bare)
                                           (treebundel-switch-bare . treebundel-bare)
                                           (treebundel-delete-bare . treebundel-bare)
                                           (treebundel-project . treebundel-project)
                                           (treebundel-read-project . treebundel-project)
                                           (treebundel-switch-project . treebundel-project)
                                           (treebundel-switch-workspace . treebundel-workspace)))

(defun treebundel-marginalia--bare-annotator (bare)
  ""
  (let* ((projects (cdr (treebundel--worktree-list (treebundel--bare-path bare))))
         (workspaces (mapcar (lambda (project) (treebundel--workspace-of (cadr project)))
                             projects)))
    (delete-dups workspaces)
    (marginalia--fields
     ((concat
       (propertize (let ((workspace-count (length workspaces)))
                     (format "%s" (propertize (format "%d" workspace-count) 'face 'treebundel-workspace)))
                   :face 'treebundel-workspace)
       "/"
       (propertize (let ((project-count (length projects)))
                     (format "%s" (propertize (format "%d" project-count) 'face 'treebundel-project)))
                   :face 'treebundel-project))))))

(defun treebundel-marginalia--workspace-annotator-project-count (workspace)
  ""
  (when-let* ((projects (treebundel--workspace-projects workspace))
              ((length> projects 0)))
    (marginalia--fields
     ((treebundel--fmt-project (format "%d" (length projects)))))))

(defun treebundel-marginalia--workspace-annotator-project-list (workspace)
  ""
  (when-let* ((projects (treebundel--workspace-projects workspace)))
    (marginalia--fields
     ((string-join (mapcar (lambda (project) (treebundel--fmt-project project)) projects) ", ")))))

(defun treebundel-marginalia--workspace-annotator-bare-list (workspace)
  ""
  (when-let* ((projects (treebundel--workspace-projects workspace))
              (bares (seq-map (lambda (project) (treebundel--bare-of workspace project)) projects)))
    (delete-dups bares)
    (marginalia--fields
     ((treebundel-marginalia--workspace-annotator-project-count workspace))
     ((string-join (mapcar (lambda (bare) (treebundel--fmt-bare bare)) bares) ", ")))))

(defun treebundel-marginalia--project-annotator (project)
  ""
  (marginalia--fields
   ((when-let* ((workspace (and (transient-scope) (oref (transient-scope) workspace)))
                (bare (treebundel--bare-of workspace project)))
      (treebundel--fmt-bare bare)))))

(defun treebundel-marginalia-enable ()
  "Enable the treebundel marginalia annotators for treebundel."
  (interactive)
  (setq marginalia-command-categories (append marginalia-command-categories treebundel-marginalia-categories))
  (add-to-list 'marginalia-annotators '(treebundel-bare treebundel-marginalia--bare-annotator builtin none))
  (add-to-list 'marginalia-annotators '(treebundel-workspace
                                        treebundel-marginalia--workspace-annotator-bare-list
                                        treebundel-marginalia--workspace-annotator-project-list
                                        treebundel-marginalia--workspace-annotator-project-count
                                        none))
  (add-to-list 'marginalia-annotators '(treebundel-project treebundel-marginalia--project-annotator builtin none)))

(defun treebundel-marginalia-disable ()
  "Disable treebundel marginalia annotators."
  (interactive)
  (dolist (category treebundel-marginalia-categories)
    (setq marginalia-command-categories (delete category marginalia-command-categories)))
  (setq marginalia-annotators (delete '(treebundel-bare treebundel-marginalia--bare-annotator builtin none)
                                      marginalia-annotators))
  (setq marginalia-annotators (delete '(treebundel-workspace
                                        treebundel-marginalia--workspace-annotator-bare-list
                                        treebundel-marginalia--workspace-annotator-project-list
                                        treebundel-marginalia--workspace-annotator-project-count
                                        none)
                                      marginalia-annotators))
  (setq marginalia-annotators (delete '(treebundel-project treebundel-marginalia--project-annotator builtin none)
                                      marginalia-annotators)))

(provide 'treebundel-marginalia)
;;; treebundel-marginalia.el ends here
