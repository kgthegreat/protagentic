;;; protagentic-prompts-test.el --- Tests for protagentic-prompts -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Unit tests for protagentic-prompts module.

;;; Code:

(require 'ert)
(require 'protagentic-prompts)
(require 'protagentic-generator) ; For generation context structure

;; Test prompt template structure

(ert-deftest protagentic-prompts-test-template-structure ()
  "Test prompt template structure creation and access."
  (let ((template (make-protagentic-prompt-template
                   :name "test"
                   :system-prompt "System prompt"
                   :user-template "User template with {PLACEHOLDER}"
                   :context-builders '(test-builder)
                   :validation-rules '(test-validator)
                   :examples '("example1" "example2"))))
    (should (equal (protagentic-prompt-template-name template) "test"))
    (should (equal (protagentic-prompt-template-system-prompt template) "System prompt"))
    (should (equal (protagentic-prompt-template-user-template template) "User template with {PLACEHOLDER}"))
    (should (equal (protagentic-prompt-template-context-builders template) '(test-builder)))
    (should (equal (protagentic-prompt-template-validation-rules template) '(test-validator)))
    (should (equal (protagentic-prompt-template-examples template) '("example1" "example2")))))

;; Test built-in templates

(ert-deftest protagentic-prompts-test-builtin-templates-exist ()
  "Test that built-in templates are properly defined."
  (should (protagentic-prompt-template-p protagentic-prompts--requirements-template))
  (should (protagentic-prompt-template-p protagentic-prompts--design-template))
  (should (protagentic-prompt-template-p protagentic-prompts--tasks-template))
  
  ;; Check that templates have required components
  (should (stringp (protagentic-prompt-template-system-prompt protagentic-prompts--requirements-template)))
  (should (stringp (protagentic-prompt-template-user-template protagentic-prompts--requirements-template)))
  (should (> (length (protagentic-prompt-template-system-prompt protagentic-prompts--requirements-template)) 100))
  (should (> (length (protagentic-prompt-template-user-template protagentic-prompts--requirements-template)) 100)))

;; Test context builders

(ert-deftest protagentic-prompts-test-requirements-context-builder ()
  "Test requirements context builder."
  (let ((context (make-protagentic-generation-context
                  :feature-description "Test feature"
                  :spec-name "test-spec"
                  :project-files '("test.el" "package.json"))))
    
    (let ((result (protagentic-prompts--build-requirements-context context)))
      (should (stringp result))
      (should (string-match-p "test-spec" result))
      (should (string-match-p "Emacs Lisp" result)))))

(ert-deftest protagentic-prompts-test-design-context-builder ()
  "Test design context builder."
  (let ((context (make-protagentic-generation-context
                  :requirements-content "Short requirements"
                  :project-files '("test.py" "requirements.txt"))))
    
    (let ((result (protagentic-prompts--build-design-context context)))
      (should (stringp result))
      (should (string-match-p "Python" result))
      (should (string-match-p "simple\\|Simple" result)))))

(ert-deftest protagentic-prompts-test-tasks-context-builder ()
  "Test tasks context builder."
  (let ((context (make-protagentic-generation-context
                  :design-content "Medium length design content for testing complexity assessment"
                  :requirements-content "Medium length requirements content"
                  :project-files '("test1.js" "test2.js" "package.json"))))
    
    (let ((result (protagentic-prompts--build-tasks-context context)))
      (should (stringp result))
      (should (string-match-p "Task Sizing" result)))))

;; Test project analysis functions

(ert-deftest protagentic-prompts-test-project-type-inference ()
  "Test project type inference from file lists."
  (should (equal (protagentic-prompts--infer-project-type '("test.el" "package.el")) "Emacs Lisp"))
  (should (equal (protagentic-prompts--infer-project-type '("app.js" "package.json")) "JavaScript/TypeScript"))
  (should (equal (protagentic-prompts--infer-project-type '("main.py" "setup.py")) "Python"))
  (should (equal (protagentic-prompts--infer-project-type '("main.java" "pom.xml")) "Java"))
  (should (equal (protagentic-prompts--infer-project-type '("main.rs" "Cargo.toml")) "Rust"))
  (should (equal (protagentic-prompts--infer-project-type '("main.go" "go.mod")) "Go"))
  (should (equal (protagentic-prompts--infer-project-type '("readme.txt")) "Multi-language or Unknown")))

(ert-deftest protagentic-prompts-test-technology-stack-inference ()
  "Test technology stack inference from file lists."
  (let ((node-stack (protagentic-prompts--infer-technology-stack '("package.json" "app.js"))))
    (should (member "Node.js/npm" node-stack)))
  
  (let ((python-stack (protagentic-prompts--infer-technology-stack '("requirements.txt" "app.py"))))
    (should (member "Python" python-stack)))
  
  (let ((rust-stack (protagentic-prompts--infer-technology-stack '("Cargo.toml" "main.rs"))))
    (should (member "Rust/Cargo" rust-stack)))
  
  (let ((empty-stack (protagentic-prompts--infer-technology-stack '("readme.txt"))))
    (should (null empty-stack))))

(ert-deftest protagentic-prompts-test-complexity-assessment ()
  "Test project complexity assessment."
  ;; Simple project
  (let ((simple-context (make-protagentic-generation-context
                         :requirements-content "Short req"
                         :design-content "Short design"
                         :project-files '("one.el"))))
    (should (eq (protagentic-prompts--assess-project-complexity simple-context) 'simple)))
  
  ;; Complex project
  (let ((complex-context (make-protagentic-generation-context
                          :requirements-content (make-string 6000 ?x)
                          :design-content (make-string 9000 ?x)
                          :project-files (make-list 20 "file.el"))))
    (should (eq (protagentic-prompts--assess-project-complexity complex-context) 'complex))))

;; Test placeholder substitution

(ert-deftest protagentic-prompts-test-placeholder-substitution ()
  "Test placeholder substitution in templates."
  (let ((template "Feature: {FEATURE_DESCRIPTION}\nReqs: {REQUIREMENTS_CONTENT}\nContext: {CONTEXT_SECTION}")
        (context (make-protagentic-generation-context
                  :feature-description "Test Feature"
                  :requirements-content "Test Requirements"))
        (context-content "Additional Context"))
    
    (let ((result (protagentic-prompts--substitute-placeholders template context context-content)))
      (should (string-match-p "Feature: Test Feature" result))
      (should (string-match-p "Reqs: Test Requirements" result))
      (should (string-match-p "Context: Additional Context" result)))))

;; Test prompt building

(ert-deftest protagentic-prompts-test-requirements-prompt-generation ()
  "Test complete requirements prompt generation."
  (let ((context (make-protagentic-generation-context
                  :feature-description "User authentication system"
                  :spec-name "auth-system")))
    
    (let ((prompt (protagentic-prompts-generate-requirements-prompt context)))
      (should (stringp prompt))
      (should (> (length prompt) 500))
      (should (string-match-p "User authentication system" prompt))
      (should (string-match-p "Requirements Document" prompt))
      (should (string-match-p "software requirements analyst" prompt)))))

(ert-deftest protagentic-prompts-test-design-prompt-generation ()
  "Test complete design prompt generation."
  (let ((context (make-protagentic-generation-context
                  :requirements-content "Test requirements for design"
                  :project-files '("app.py"))))
    
    (let ((prompt (protagentic-prompts-generate-design-prompt context)))
      (should (stringp prompt))
      (should (> (length prompt) 500))
      (should (string-match-p "Test requirements for design" prompt))
      (should (string-match-p "Design Document" prompt))
      (should (string-match-p "software architect" prompt)))))

(ert-deftest protagentic-prompts-test-tasks-prompt-generation ()
  "Test complete tasks prompt generation."
  (let ((context (make-protagentic-generation-context
                  :requirements-content "Test requirements"
                  :design-content "Test design content")))
    
    (let ((prompt (protagentic-prompts-generate-tasks-prompt context)))
      (should (stringp prompt))
      (should (> (length prompt) 500))
      (should (string-match-p "Test requirements" prompt))
      (should (string-match-p "Test design content" prompt))
      (should (string-match-p "Implementation Plan" prompt))
      (should (string-match-p "technical lead" prompt)))))

;; Test validation functions

(ert-deftest protagentic-prompts-test-requirements-validation ()
  "Test requirements output validation."
  (let ((valid-content "# Requirements Document\n\n## Introduction\n\n## Requirements\n\n**User Story:** As a user\n\n#### Acceptance Criteria\n")
        (invalid-content "This is not a proper requirements document"))
    
    (should (eq (protagentic-prompts--validate-requirements-output valid-content) t))
    (should (stringp (protagentic-prompts--validate-requirements-output invalid-content)))))

(ert-deftest protagentic-prompts-test-design-validation ()
  "Test design output validation."
  (let ((valid-content "# Design Document\n\n## Overview\n\n## Architecture\n\n## Components and Interfaces\n")
        (invalid-content "This is not a proper design document"))
    
    (should (eq (protagentic-prompts--validate-design-output valid-content) t))
    (should (stringp (protagentic-prompts--validate-design-output invalid-content)))))

(ert-deftest protagentic-prompts-test-tasks-validation ()
  "Test tasks output validation."
  (let ((valid-content "# Implementation Plan\n\n- [ ] Task 1\n  - Details\n  - _Requirements: 1.1_\n")
        (invalid-content "This is not a proper tasks document"))
    
    (should (eq (protagentic-prompts--validate-tasks-output valid-content) t))
    (should (stringp (protagentic-prompts--validate-tasks-output invalid-content)))))

;; Test public API

(ert-deftest protagentic-prompts-test-public-validation-api ()
  "Test public validation API."
  (let ((valid-requirements "# Requirements Document\n\n## Introduction\n\n## Requirements\n\n**User Story:** Test\n\n#### Acceptance Criteria\n"))
    
    (should (eq (protagentic-prompts-validate-output valid-requirements "requirements") t))
    (should (stringp (protagentic-prompts-validate-output "invalid" "requirements")))
    (should (stringp (protagentic-prompts-validate-output "anything" "unknown-type")))))

;; Test guidance functions

(ert-deftest protagentic-prompts-test-complexity-guidance ()
  "Test complexity-based guidance generation."
  (should (stringp (protagentic-prompts--get-complexity-guidance 'simple)))
  (should (stringp (protagentic-prompts--get-complexity-guidance 'moderate)))
  (should (stringp (protagentic-prompts--get-complexity-guidance 'complex)))
  (should (string-match-p "simplicity" (protagentic-prompts--get-complexity-guidance 'simple)))
  (should (string-match-p "modularity" (protagentic-prompts--get-complexity-guidance 'complex))))

(ert-deftest protagentic-prompts-test-task-sizing-guidance ()
  "Test task sizing guidance generation."
  (should (stringp (protagentic-prompts--get-task-sizing-guidance 'simple)))
  (should (stringp (protagentic-prompts--get-task-sizing-guidance 'moderate)))
  (should (stringp (protagentic-prompts--get-task-sizing-guidance 'complex)))
  (should (string-match-p "4-6" (protagentic-prompts--get-task-sizing-guidance 'simple)))
  (should (string-match-p "8-12" (protagentic-prompts--get-task-sizing-guidance 'complex))))

(provide 'protagentic-prompts-test)

;;; protagentic-prompts-test.el ends here