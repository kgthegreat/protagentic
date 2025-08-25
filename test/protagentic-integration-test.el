;;; protagentic-integration-test.el --- Integration tests for Protagentic -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Integration tests for Protagentic workflows.
;; Tests complete user workflows from spec creation to task generation.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load Protagentic modules
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(require 'protagentic)
(require 'protagentic-commands)
(require 'protagentic-navigation)

;; Integration test utilities

(defmacro protagentic-integration-test-with-project (&rest body)
  "Execute BODY with a complete project setup for integration testing."
  `(let ((protagentic-test-temp-dir (make-temp-file "protagentic-integration" t))
         (protagentic-auto-open-generated-files nil)  ; Don't open files during tests
         (protagentic-validate-prerequisites-interactively nil)  ; Don't show interactive dialogs
         (protagentic-test-current-spec nil))  ; Track current spec for tests
     (unwind-protect
         (progn
           ;; Create a mock project structure
           (let ((default-directory protagentic-test-temp-dir))
             ;; Create .git directory to make it a valid project
             (make-directory ".git")
             ;; Mock all interactive functions
             (cl-letf (((symbol-function 'protagentic--detect-project-root)
                        (lambda () protagentic-test-temp-dir))
                       ;; Mock generation mode prompt to always return template mode
                       ((symbol-function 'protagentic-config-prompt-for-generation-mode)
                        (lambda () 'template))
                       ;; Mock spec selection to return current test spec
                       ((symbol-function 'protagentic--prompt-for-spec)
                        (lambda (prompt) protagentic-test-current-spec))
                       ;; Mock find-current-spec to return current test spec
                       ((symbol-function 'protagentic--find-current-spec)
                        (lambda () protagentic-test-current-spec))
                       ;; Mock file opening to prevent actual file operations
                       ((symbol-function 'find-file)
                        (lambda (file) nil))
                       ;; Mock completing-read for any other prompts
                       ((symbol-function 'completing-read)
                        (lambda (prompt collection &rest args) 
                          (if collection (caar collection) "")))
                       ;; Mock read-string for user inputs
                       ((symbol-function 'read-string)
                        (lambda (prompt &optional initial-input history default-value inherit-input-method)
                          (cond
                           ((string-match-p "Describe the" prompt) "A secure user authentication system with login and registration")
                           ((string-match-p "additional context" prompt) "")
                           (t (or default-value ""))))))
               ,@body)))
       (when (file-directory-p protagentic-test-temp-dir)
         (delete-directory protagentic-test-temp-dir t)))))

(defun protagentic-test-create-mock-input (inputs)
  "Create mock input function that returns INPUTS in sequence."
  (let ((input-list (copy-sequence inputs))
        (call-count 0))
    (lambda (prompt &rest args)
      (let ((input (nth call-count input-list)))
        (setq call-count (1+ call-count))
        (or input "")))))

;; Complete workflow integration tests

(ert-deftest protagentic-integration-test-complete-workflow ()
  "Test complete workflow from spec creation to task generation."
  (protagentic-integration-test-with-project
   (let ((spec-name "user-authentication")
         (feature-description "A secure user authentication system with login and registration"))
     
     ;; Step 1: Create spec with requirements
     (let ((spec (protagentic-commands-create-spec spec-name)))
       ;; Set current spec for subsequent operations
       (setq protagentic-test-current-spec spec)
       
       (should (protagentic-spec-p spec))
       (should (string= (protagentic-spec-name spec) spec-name))
       
       ;; Verify requirements file was created
       (should (file-exists-p (protagentic-spec-requirements-file spec)))
       (let ((requirements-content (protagentic--read-file-content 
                                    (protagentic-spec-requirements-file spec))))
         (should (string-match-p "# Requirements Document" requirements-content)))
       
       ;; Verify spec status after requirements
       (let ((status (protagentic--get-spec-status spec)))
         (should (plist-get status :requirements-complete))
         (should (not (plist-get status :design-complete)))
         (should (not (plist-get status :tasks-complete)))
         (should (eq (plist-get status :current-phase) 'requirements))
         (should (eq (plist-get status :next-phase) 'design)))
       
       ;; Step 2: Generate design document
       (protagentic-commands-generate-design)
       
       ;; Verify design file was created
       (should (file-exists-p (protagentic-spec-design-file spec)))
       (let ((design-content (protagentic--read-file-content 
                              (protagentic-spec-design-file spec))))
         (should (string-match-p "# Design Document" design-content))
         (should (string-match-p "## Overview" design-content))
         (should (string-match-p "## Architecture" design-content)))
       
       ;; Verify spec status after design
       (let ((status (protagentic--get-spec-status spec)))
         (should (plist-get status :requirements-complete))
         (should (plist-get status :design-complete))
         (should (not (plist-get status :tasks-complete)))
         (should (eq (plist-get status :current-phase) 'design))
         (should (eq (plist-get status :next-phase) 'tasks)))
       
       ;; Step 3: Generate tasks document
       (protagentic-commands-generate-tasks)
       
       ;; Verify tasks file was created
       (should (file-exists-p (protagentic-spec-tasks-file spec)))
       (let ((tasks-content (protagentic--read-file-content 
                             (protagentic-spec-tasks-file spec))))
         (should (string-match-p "# Implementation Plan" tasks-content))
         (should (string-match-p "- \\[ \\]" tasks-content)))
       
       ;; Verify final spec status
       (let ((status (protagentic--get-spec-status spec)))
         (should (plist-get status :requirements-complete))
         (should (plist-get status :design-complete))
         (should (plist-get status :tasks-complete))
         (should (plist-get status :workflow-complete))
         (should (eq (plist-get status :current-phase) 'tasks))
         (should (null (plist-get status :next-phase))))))))

(ert-deftest protagentic-integration-test-navigation-workflow ()
  "Test navigation between spec documents."
  (protagentic-integration-test-with-project
   (let ((spec-name "test-navigation"))
     
     ;; Create spec and all phases
     (let ((spec (protagentic-commands-create-spec spec-name)))
       (setq protagentic-test-current-spec spec)
       (protagentic-commands-generate-design)
       (protagentic-commands-generate-tasks)
       
       ;; Test navigation commands
       ;; Mock find-file to track which files are opened
       (let ((opened-files '()))
         (cl-letf (((symbol-function 'find-file)
                    (lambda (file) (push file opened-files))))
           
           ;; Test opening requirements
           (protagentic-navigation-open-requirements)
           (should (cl-some (lambda (file) 
                              (string-match-p "requirements\\.md" file)) 
                            opened-files))
           
           ;; Test opening design
           (protagentic-navigation-open-design)
           (should (cl-some (lambda (file) 
                              (string-match-p "design\\.md" file)) 
                            opened-files))
           
           ;; Test opening tasks
           (protagentic-navigation-open-tasks)
           (should (cl-some (lambda (file) 
                              (string-match-p "tasks\\.md" file)) 
                            opened-files))))))))

(ert-deftest protagentic-integration-test-error-handling-workflow ()
  "Test error handling in workflow scenarios."
  (protagentic-integration-test-with-project
   
   ;; Test prerequisite validation
   (let ((spec-name "test-errors"))
     
     ;; Try to generate design without requirements (should fail)
     (setq protagentic-test-current-spec nil)  ; No current spec
     (should-error (protagentic-commands-generate-design))
     
     ;; Create requirements first
     (let ((spec (protagentic-commands-create-spec spec-name)))
       (setq protagentic-test-current-spec spec)
       
       ;; Now design generation should work (returns success message)
       (should (stringp (protagentic-commands-generate-design)))
       
       ;; Tasks generation should also work now (returns success message)
       (should (stringp (protagentic-commands-generate-tasks)))))))

(ert-deftest protagentic-integration-test-multiple-specs ()
  "Test managing multiple specs in the same project."
  (protagentic-integration-test-with-project
   
   ;; Create multiple specs
   (let ((spec-one (protagentic-commands-create-spec "feature-one"))
         (spec-two (protagentic-commands-create-spec "feature-two"))
         (spec-three (protagentic-commands-create-spec "feature-three")))
     
     ;; Test spec listing
     (let ((specs (protagentic--get-project-specs)))
       (should (= (length specs) 3))
       (should (member "feature-one" specs))
       (should (member "feature-two" specs))
       (should (member "feature-three" specs)))
     
     ;; Complete workflow for spec-one only
     (setq protagentic-test-current-spec spec-one)
     (protagentic-commands-generate-design)
     (protagentic-commands-generate-tasks)
     
     ;; Check that spec-one is complete but spec-two is not
     (let ((status-one (protagentic--get-spec-status spec-one))
           (status-two (protagentic--get-spec-status spec-two)))
       (should (plist-get status-one :workflow-complete))
       (should (not (plist-get status-two :workflow-complete)))))))

(ert-deftest protagentic-integration-test-refinement-workflow ()
  "Test interactive refinement workflows."
  (protagentic-integration-test-with-project
   (let ((spec-name "test-refinement"))
     
     ;; Create initial spec
     (let ((spec (protagentic-commands-create-spec spec-name)))
       (setq protagentic-test-current-spec spec)
       
       ;; Test requirements analysis
       (let* ((requirements-content (protagentic--read-file-content 
                                     (protagentic-spec-requirements-file spec)))
              (analysis (protagentic--analyze-requirements-quality requirements-content)))
         
         ;; Should have some quality score and suggestions
         (should (numberp (plist-get analysis :quality-score)))
         (should (>= (plist-get analysis :quality-score) 0))
         (should (<= (plist-get analysis :quality-score) 100)))
       
       ;; Generate design and test design analysis
       (protagentic-commands-generate-design)
       (let* ((design-content (protagentic--read-file-content 
                               (protagentic-spec-design-file spec)))
              (analysis (protagentic--analyze-design-quality design-content)))
         
         ;; Should have quality metrics
         (should (numberp (plist-get analysis :quality-score)))
         (should (listp (plist-get analysis :suggestions))))))))

(ert-deftest protagentic-integration-test-file-operations-workflow ()
  "Test file operations throughout the workflow."
  (protagentic-integration-test-with-project
   (let ((spec-name "file-ops-test"))
     
     ;; Create spec
     (let ((spec (protagentic-commands-create-spec spec-name)))
       (setq protagentic-test-current-spec spec)
       
       ;; Verify directory structure was created safely
       (should (file-directory-p (protagentic-spec-root-path spec)))
       (should (file-exists-p (protagentic-spec-requirements-file spec)))
       
       ;; Test file content integrity
       (let ((content (protagentic--safe-read-file 
                       (protagentic-spec-requirements-file spec))))
         (should (stringp content))
         (should (> (length content) 0)))
       
       ;; Generate design and verify file operations
       (protagentic-commands-generate-design)
       (should (file-exists-p (protagentic-spec-design-file spec)))
       
       ;; Generate tasks and verify file operations
       (protagentic-commands-generate-tasks)
       (should (file-exists-p (protagentic-spec-tasks-file spec)))
       
       ;; Test that all files have valid content
       (dolist (file (list (protagentic-spec-requirements-file spec)
                           (protagentic-spec-design-file spec)
                           (protagentic-spec-tasks-file spec)))
         (let ((content (protagentic--safe-read-file file)))
           (should (stringp content))
           (should (> (length content) 10))  ; Should have substantial content
           (should (string-match-p "^#" content)))))))  ; Should start with markdown header

(ert-deftest protagentic-integration-test-status-and-guidance ()
  "Test status reporting and workflow guidance throughout the process."
  (protagentic-integration-test-with-project
   (let ((spec-name "status-test"))
     
     ;; Test guidance for new project (no specs)
     (let ((specs (protagentic--get-project-specs)))
       (should (null specs)))
     
     ;; Create spec and test guidance at each phase

       
       ;; Phase 1: Requirements created
       (let ((spec (protagentic-commands-create-spec spec-name)))
         (setq protagentic-test-current-spec spec)
         (let ((guidance (protagentic--provide-workflow-guidance spec)))
           (should (string-match-p "Ready for Design" guidance))
           (should (string-match-p "protagentic-generate-design" guidance)))
         
         ;; Phase 2: Design created
         (protagentic-commands-generate-design)
         (let ((guidance (protagentic--provide-workflow-guidance spec)))
           (should (string-match-p "Ready for Implementation" guidance))
           (should (string-match-p "protagentic-generate-tasks" guidance)))
         
         ;; Phase 3: Tasks created (workflow complete)
         (protagentic-commands-generate-tasks)
         (let ((guidance (protagentic--provide-workflow-guidance spec)))
           (should (string-match-p "Congratulations" guidance))
           (should (string-match-p "complete" guidance))))))))

(ert-deftest protagentic-integration-test-spec-deletion ()
  "Test spec deletion workflow."
  (protagentic-integration-test-with-project
   (let ((spec-name "deletion-test"))
     
     ;; Create a complete spec
     (let ((spec (protagentic-commands-create-spec spec-name)))
       (setq protagentic-test-current-spec spec)
       (protagentic-commands-generate-design)
       (protagentic-commands-generate-tasks))
     
     ;; Verify spec exists
     (let ((specs (protagentic--get-project-specs)))
       (should (member spec-name specs)))
     
     ;; Mock user confirmation for deletion
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) t)))
       (protagentic-commands-delete-spec spec-name))
     
     ;; Verify spec was deleted
     (let ((specs (protagentic--get-project-specs)))
       (should (not (member spec-name specs))))
     
     ;; Verify files were deleted
     (let ((spec (protagentic--create-spec-struct spec-name)))
       (should (not (file-exists-p (protagentic-spec-root-path spec))))))))

;; Performance and stress tests

(ert-deftest protagentic-integration-test-large-content ()
  "Test handling of large document content."
  (protagentic-integration-test-with-project
   (let ((spec-name "large-content-test")
         (large-description (make-string 5000 ?x)))  ; 5KB description
     
     ;; Create spec with large content
     (cl-letf (((symbol-function 'read-string)
                (protagentic-test-create-mock-input 
                 (list large-description ""))))
       
       (let ((spec (protagentic-commands-create-spec spec-name)))
         (setq protagentic-test-current-spec spec)
         
         ;; Verify large content was handled correctly
         (let ((content (protagentic--safe-read-file 
                         (protagentic-spec-requirements-file spec))))
           (should (> (length content) 100))  ; Should contain substantial content
           (should (string-match-p "Requirements Document" content)))
         
         ;; Test that subsequent operations work with large content
         (should (stringp (protagentic-commands-generate-design)))
         
         (should (stringp (protagentic-commands-generate-tasks))))))))

;; Run all integration tests
(defun protagentic-run-integration-tests ()
  "Run all Protagentic integration tests."
  (interactive)
  (ert "protagentic-integration-test-"))

(defun protagentic-run-all-tests ()
  "Run all Protagentic tests (unit and integration)."
  (interactive)
  (ert "protagentic-.*-test-"))

(provide 'protagentic-integration-test)

;;; protagentic-integration-test.el ends here