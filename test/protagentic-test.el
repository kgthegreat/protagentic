;;; protagentic-test.el --- Tests for Protagentic -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Unit tests for Protagentic using ERT (Emacs Lisp Regression Testing).
;; Run tests with: M-x ert RET t RET

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load Protagentic modules
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(require 'protagentic)
(require 'protagentic-core)
(require 'protagentic-utils)
(require 'protagentic-templates)
(require 'protagentic-commands)

;; Test utilities

(defvar protagentic-test-temp-dir nil
  "Temporary directory for test files.")

(defun protagentic-test-setup ()
  "Set up test environment."
  (setq protagentic-test-temp-dir (make-temp-file "protagentic-test" t))
  ;; Mock project root detection
  (cl-letf (((symbol-function 'protagentic--detect-project-root)
             (lambda () protagentic-test-temp-dir)))
    protagentic-test-temp-dir))

(defun protagentic-test-teardown ()
  "Clean up test environment."
  (when (and protagentic-test-temp-dir 
             (file-directory-p protagentic-test-temp-dir))
    (delete-directory protagentic-test-temp-dir t))
  (setq protagentic-test-temp-dir nil))

(defmacro protagentic-test-with-temp-project (&rest body)
  "Execute BODY with a temporary project setup."
  `(let ((protagentic-test-temp-dir (make-temp-file "protagentic-test" t)))
     (unwind-protect
         (cl-letf (((symbol-function 'protagentic--detect-project-root)
                    (lambda () protagentic-test-temp-dir)))
           ,@body)
       (when (file-directory-p protagentic-test-temp-dir)
         (delete-directory protagentic-test-temp-dir t)))))

;; Core functionality tests

(ert-deftest protagentic-test-spec-creation ()
  "Test spec structure creation."
  (protagentic-test-with-temp-project
   (let ((spec (protagentic--create-spec-struct "test-feature")))
     (should (protagentic-spec-p spec))
     (should (string= (protagentic-spec-name spec) "test-feature"))
     (should (string-match-p "test-feature" (protagentic-spec-root-path spec)))
     (should (string-match-p "requirements\\.md" (protagentic-spec-requirements-file spec)))
     (should (string-match-p "design\\.md" (protagentic-spec-design-file spec)))
     (should (string-match-p "tasks\\.md" (protagentic-spec-tasks-file spec))))))

(ert-deftest protagentic-test-phase-detection ()
  "Test phase detection based on existing files."
  (protagentic-test-with-temp-project
   (let* ((spec-dir (expand-file-name ".protagentic/specs/test-spec" protagentic-test-temp-dir))
          (requirements-file (expand-file-name "requirements.md" spec-dir))
          (design-file (expand-file-name "design.md" spec-dir)))
     
     ;; No files exist
     (should (null (protagentic--detect-current-phase spec-dir)))
     
     ;; Create requirements file
     (make-directory spec-dir t)
     (with-temp-file requirements-file (insert "# Requirements"))
     (should (eq (protagentic--detect-current-phase spec-dir) 'requirements))
     
     ;; Create design file
     (with-temp-file design-file (insert "# Design"))
     (should (eq (protagentic--detect-current-phase spec-dir) 'design))
     
     ;; Create tasks file
     (with-temp-file (expand-file-name "tasks.md" spec-dir) (insert "# Tasks"))
     (should (eq (protagentic--detect-current-phase spec-dir) 'tasks)))))

(ert-deftest protagentic-test-phase-validation ()
  "Test phase prerequisite validation."
  (protagentic-test-with-temp-project
   (let* ((spec (protagentic--create-spec-struct "test-spec"))
          (spec-dir (protagentic-spec-root-path spec)))
     
     ;; Should fail for design phase without requirements
     (should-error (protagentic--validate-phase-prerequisites 'design spec))
     
     ;; Create requirements file
     (make-directory spec-dir t)
     (with-temp-file (protagentic-spec-requirements-file spec)
       (insert "# Requirements\n## Requirements\n### Requirement 1"))
     
     ;; Should pass for design phase with requirements
     (should (protagentic--validate-phase-prerequisites 'design spec))
     
     ;; Should fail for tasks phase without design
     (should-error (protagentic--validate-phase-prerequisites 'tasks spec))
     
     ;; Create design file
     (with-temp-file (protagentic-spec-design-file spec)
       (insert "# Design\n## Overview"))
     
     ;; Should pass for tasks phase with design
     (should (protagentic--validate-phase-prerequisites 'tasks spec)))))

(ert-deftest protagentic-test-spec-name-validation ()
  "Test spec name validation and sanitization."
  ;; Valid names
  (should (string= (protagentic--validate-spec-name "test-feature") "test-feature"))
  (should (string= (protagentic--validate-spec-name "Test Feature") "test-feature"))
  (should (string= (protagentic--validate-spec-name "test_feature") "test-feature"))
  
  ;; Invalid names
  (should-error (protagentic--validate-spec-name ""))
  (should-error (protagentic--validate-spec-name "   "))
  (should-error (protagentic--validate-spec-name "!@#$%"))
  
  ;; Long names
  (let ((long-name (make-string 60 ?a)))
    (should-error (protagentic--validate-spec-name long-name))))

(ert-deftest protagentic-test-spec-status ()
  "Test spec status detection and reporting."
  (protagentic-test-with-temp-project
   (let* ((spec (protagentic--create-spec-struct "test-spec"))
          (spec-dir (protagentic-spec-root-path spec)))
     
     (make-directory spec-dir t)
     
     ;; Initial status - no files
     (let ((status (protagentic--get-spec-status spec)))
       (should (not (plist-get status :requirements-complete)))
       (should (not (plist-get status :design-complete)))
       (should (not (plist-get status :tasks-complete)))
       (should (not (plist-get status :workflow-complete)))
       (should (null (plist-get status :current-phase))))
     
     ;; Add requirements
     (with-temp-file (protagentic-spec-requirements-file spec)
       (insert "# Requirements"))
     (let ((status (protagentic--get-spec-status spec)))
       (should (plist-get status :requirements-complete))
       (should (eq (plist-get status :current-phase) 'requirements))
       (should (eq (plist-get status :next-phase) 'design)))
     
     ;; Add design
     (with-temp-file (protagentic-spec-design-file spec)
       (insert "# Design"))
     (let ((status (protagentic--get-spec-status spec)))
       (should (plist-get status :design-complete))
       (should (eq (plist-get status :current-phase) 'design))
       (should (eq (plist-get status :next-phase) 'tasks)))
     
     ;; Add tasks
     (with-temp-file (protagentic-spec-tasks-file spec)
       (insert "# Tasks"))
     (let ((status (protagentic--get-spec-status spec)))
       (should (plist-get status :tasks-complete))
       (should (plist-get status :workflow-complete))
       (should (eq (plist-get status :current-phase) 'tasks))
       (should (null (plist-get status :next-phase)))))))

;; Template generation tests

(ert-deftest protagentic-test-requirements-template ()
  "Test requirements template generation."
  (let ((template (protagentic--generate-requirements-template "A user authentication system")))
    (should (string-match-p "# Requirements Document" template))
    (should (string-match-p "## Introduction" template))
    (should (string-match-p "## Requirements" template))
    (should (string-match-p "\\*\\*User Story:\\*\\*" template))
    (should (string-match-p "#### Acceptance Criteria" template))
    (should (string-match-p "WHEN.*THEN.*SHALL" template))))

(ert-deftest protagentic-test-design-template ()
  "Test design template generation."
  (let* ((requirements "# Requirements\n## Requirements\n### Requirement 1\n**User Story:** As a user, I want authentication")
         (template (protagentic--generate-design-template requirements)))
    (should (string-match-p "# Design Document" template))
    (should (string-match-p "## Overview" template))
    (should (string-match-p "## Architecture" template))
    (should (string-match-p "## Components and Interfaces" template))
    (should (string-match-p "## Data Models" template))
    (should (string-match-p "## Error Handling" template))
    (should (string-match-p "## Testing Strategy" template))))

(ert-deftest protagentic-test-tasks-template ()
  "Test tasks template generation."
  (let* ((design "# Design\n## Components\n#### User Component\n## Data Models\n#### User Model")
         (template (protagentic--generate-tasks-template design)))
    (should (string-match-p "# Implementation Plan" template))
    (should (string-match-p "- \\[ \\] [0-9]+\\." template))
    (should (string-match-p "_Requirements:" template))))

;; Content parsing tests

(ert-deftest protagentic-test-requirements-parsing ()
  "Test requirements content parsing."
  (let* ((content "# Requirements\n## Introduction\nGood intro\n## Requirements\n### Requirement 1\n**User Story:** As a user, I want login\n#### Acceptance Criteria\n1. WHEN user enters credentials THEN system SHALL validate")
         (parsed (protagentic--parse-requirements-content content)))
    (should (= (length (plist-get parsed :user-stories)) 1))
    (should (string-match-p "As a user, I want login" (car (plist-get parsed :user-stories))))
    (should (> (length (plist-get parsed :acceptance-criteria)) 0))
    (should (plist-get parsed :has-introduction))))

(ert-deftest protagentic-test-design-parsing ()
  "Test design content parsing."
  (let* ((content "# Design\n## Overview\n## Architecture\n#### User Component\n#### Auth Component\n#### User Model\n**Frontend:** React")
         (parsed (protagentic--parse-design-content content)))
    (should (member "Overview" (plist-get parsed :sections)))
    (should (member "Architecture" (plist-get parsed :sections)))
    (should (= (length (plist-get parsed :components)) 2))
    (should (member "User" (plist-get parsed :components)))
    (should (member "Auth" (plist-get parsed :components)))
    (should (= (length (plist-get parsed :data-models)) 1))
    (should (member "User" (plist-get parsed :data-models)))))

;; File operation tests

(ert-deftest protagentic-test-safe-file-operations ()
  "Test safe file operations."
  (protagentic-test-with-temp-project
   (let ((test-file (expand-file-name "test.txt" protagentic-test-temp-dir))
         (test-content "Hello, World!"))
     
     ;; Test safe write
     (should (protagentic--safe-write-file test-file test-content))
     (should (file-exists-p test-file))
     
     ;; Test safe read
     (should (string= (protagentic--safe-read-file test-file) test-content))
     
     ;; Test read non-existent file
     (should (null (protagentic--safe-read-file "/nonexistent/file.txt")))
     
     ;; Test safe delete
     (should (protagentic--safe-delete-file test-file))
     (should (not (file-exists-p test-file))))))

(ert-deftest protagentic-test-directory-operations ()
  "Test directory creation and management."
  (protagentic-test-with-temp-project
   (let ((test-dir (expand-file-name "test-dir" protagentic-test-temp-dir)))
     
     ;; Test directory creation
     (should (protagentic--safe-create-directory test-dir))
     (should (file-directory-p test-dir))
     
     ;; Test creating existing directory (should succeed)
     (should (protagentic--safe-create-directory test-dir))
     
     ;; Test directory deletion
     (should (protagentic--safe-delete-directory test-dir))
     (should (not (file-exists-p test-dir))))))

;; Utility function tests

(ert-deftest protagentic-test-spec-name-sanitization ()
  "Test spec name sanitization."
  (should (string= (protagentic--sanitize-spec-name "Test Feature") "test-feature"))
  (should (string= (protagentic--sanitize-spec-name "test_feature") "test-feature"))
  (should (string= (protagentic--sanitize-spec-name "Test  Multiple   Spaces") "test-multiple-spaces"))
  (should (string= (protagentic--sanitize-spec-name "test!@#$feature") "testfeature"))
  (should (string= (protagentic--sanitize-spec-name "---test---") "test")))

(ert-deftest protagentic-test-next-requirement-number ()
  "Test requirement number extraction."
  (let ((content "### Requirement 1\n### Requirement 3\n### Requirement 5"))
    (should (= (protagentic--get-next-requirement-number content) 6)))
  
  (let ((content "No requirements here"))
    (should (= (protagentic--get-next-requirement-number content) 1))))

;; Integration tests

(ert-deftest protagentic-test-workflow-guidance ()
  "Test workflow guidance generation."
  (protagentic-test-with-temp-project
   (let* ((spec (protagentic--create-spec-struct "test-spec"))
          (spec-dir (protagentic-spec-root-path spec)))
     
     (make-directory spec-dir t)
     
     ;; Test guidance for new spec
     (let ((guidance (protagentic--provide-workflow-guidance spec)))
       (should (string-match-p "Getting Started" guidance))
       (should (string-match-p "protagentic-create-spec" guidance)))
     
     ;; Add requirements and test guidance
     (with-temp-file (protagentic-spec-requirements-file spec)
       (insert "# Requirements"))
     (let ((guidance (protagentic--provide-workflow-guidance spec)))
       (should (string-match-p "Ready for Design" guidance))
       (should (string-match-p "protagentic-generate-design" guidance))))))

(ert-deftest protagentic-test-quality-analysis ()
  "Test document quality analysis."
  (let* ((good-requirements "# Requirements\n## Introduction\nGood intro\n## Requirements\n### Requirement 1\n**User Story:** As a user, I want login\n#### Acceptance Criteria\n1. WHEN user clicks login THEN system SHALL validate")
         (analysis (protagentic--analyze-requirements-quality good-requirements)))
    (should (> (plist-get analysis :quality-score) 50))
    (should (> (plist-get analysis :user-story-count) 0))
    (should (> (plist-get analysis :criteria-count) 0)))
  
  (let* ((poor-requirements "# Requirements\nNot much here")
         (analysis (protagentic--analyze-requirements-quality poor-requirements)))
    (should (< (plist-get analysis :quality-score) 50))
    (should (> (length (plist-get analysis :suggestions)) 0))))

;; Run all tests
(defun protagentic-run-tests ()
  "Run all Protagentic tests."
  (interactive)
  (ert "protagentic-test-"))

(provide 'protagentic-test)

;;; protagentic-test.el ends here