;;; protagentic-templates.el --- Document templates for Protagentic -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Template generation functions for requirements, design, and tasks documents.
;; Provides structured markdown templates with proper formatting.

;;; Code:

(require 'cl-lib)

(defun protagentic--generate-requirements-template (feature-description)
  "Generate a requirements document template based on FEATURE-DESCRIPTION.
Returns a formatted markdown string with user stories and acceptance criteria."
  (let ((feature-name (protagentic--extract-feature-name feature-description)))
    (format "# Requirements Document

## Introduction

%s

## Requirements

### Requirement 1

**User Story:** As a [role], I want [feature capability], so that [benefit/value]

#### Acceptance Criteria

1. WHEN [trigger event] THEN the system SHALL [expected response]
2. IF [precondition] THEN the system SHALL [required behavior]
3. WHEN [user action] AND [context] THEN the system SHALL [specific outcome]

### Requirement 2

**User Story:** As a [role], I want [feature capability], so that [benefit/value]

#### Acceptance Criteria

1. WHEN [trigger event] THEN the system SHALL [expected response]
2. WHEN [condition occurs] THEN the system SHALL [required behavior]

### Requirement 3

**User Story:** As a [role], I want [feature capability], so that [benefit/value]

#### Acceptance Criteria

1. WHEN [user performs action] THEN the system SHALL [expected outcome]
2. IF [error condition] THEN the system SHALL [error handling behavior]
3. WHEN [edge case scenario] THEN the system SHALL [appropriate response]

<!-- Add more requirements as needed following the same pattern -->
<!-- Each requirement should have:
     - A clear user story in the format above
     - 2-5 acceptance criteria in EARS format
     - Focus on specific, testable behaviors
-->"
            (protagentic--generate-feature-introduction feature-description feature-name))))

(defun protagentic--generate-design-template (requirements-content)
  "Generate a design document template based on REQUIREMENTS-CONTENT.
Analyzes requirements to suggest architecture and components."
  (let ((components (protagentic--extract-components-from-requirements requirements-content))
        (data-models (protagentic--extract-data-models-from-requirements requirements-content)))
    (format "# Design Document

## Overview

%s

## Architecture

### System Architecture

[Describe the overall system architecture, including major components and their relationships]

### Technology Stack

- **Frontend:** [Technology choices and rationale]
- **Backend:** [Technology choices and rationale]  
- **Database:** [Data storage approach and rationale]
- **External Services:** [Third-party integrations if any]

## Components and Interfaces

%s

## Data Models

%s

### Data Relationships

[Describe how data models relate to each other, including foreign keys, associations, etc.]

### Data Validation

[Specify validation rules, constraints, and business logic for data integrity]

## Error Handling

### Error Categories

1. **User Input Errors**
   - Invalid data format
   - Missing required fields
   - Business rule violations

2. **System Errors**
   - Database connection failures
   - External service unavailability
   - Resource limitations

3. **Security Errors**
   - Authentication failures
   - Authorization violations
   - Input validation failures

### Error Response Strategy

[Describe how errors will be handled, logged, and communicated to users]

## Testing Strategy

### Unit Testing

[Describe approach for testing individual components]

### Integration Testing

[Describe approach for testing component interactions]

### End-to-End Testing

[Describe approach for testing complete user workflows]

### Test Coverage Goals

[Specify coverage targets and critical paths that must be tested]"
            (protagentic--generate-design-overview requirements-content)
            (protagentic--generate-components-section components)
            (protagentic--generate-data-models-section data-models))))

(defun protagentic--generate-tasks-template (design-content)
  "Generate a tasks document template based on DESIGN-CONTENT.
Creates hierarchical implementation tasks with requirement references."
  (let ((tasks (protagentic--extract-implementation-tasks design-content)))
    (format "# Implementation Plan

%s

<!-- Task Guidelines:
- Each task should be focused on a specific coding activity
- Include requirement references for traceability
- Build incrementally - each task should build on previous work
- Focus on test-driven development where appropriate
- Ensure tasks are actionable by a developer
-->"
            (protagentic--generate-tasks-list tasks))))

(defun protagentic--extract-feature-name (feature-description)
  "Extract a concise feature name from FEATURE-DESCRIPTION."
  (let ((words (split-string (downcase feature-description) "[ \t\n]+" t)))
    (string-join (cl-subseq words 0 (min 4 (length words))) " ")))

(defun protagentic--generate-feature-introduction (feature-description feature-name)
  "Generate an introduction section for the feature based on FEATURE-DESCRIPTION and FEATURE-NAME."
  (format "%s

This document outlines the requirements for implementing %s. The requirements are structured using user stories and acceptance criteria in EARS (Easy Approach to Requirements Syntax) format to ensure clarity and testability.

Each requirement includes:
- A user story describing the feature from the user's perspective
- Specific acceptance criteria that define the expected system behavior
- Clear conditions and expected outcomes for testing and validation"
          feature-description
          feature-name))

(defun protagentic--extract-components-from-requirements (requirements-content)
  "Extract potential system components from REQUIREMENTS-CONTENT.
Returns a list of component suggestions based on user stories and acceptance criteria."
  (let ((components '()))
    ;; Look for common patterns in requirements that suggest components
    (when (string-match-p "user\\|authentication\\|login" requirements-content)
      (push "User Management Component" components))
    (when (string-match-p "data\\|store\\|save\\|database" requirements-content)
      (push "Data Access Layer" components))
    (when (string-match-p "api\\|service\\|endpoint" requirements-content)
      (push "API Service Layer" components))
    (when (string-match-p "interface\\|ui\\|display\\|view" requirements-content)
      (push "User Interface Component" components))
    (when (string-match-p "validation\\|validate\\|check" requirements-content)
      (push "Validation Component" components))
    
    ;; Always include core components
    (push "Core Business Logic" components)
    (push "Configuration Management" components)
    
    (reverse components)))

(defun protagentic--generate-components-section (components)
  "Generate the components section content from COMPONENTS list."
  (if components
      (concat "### Core Components\n\n"
              (mapconcat (lambda (component)
                           (format "#### %s\n\n[Describe the purpose, responsibilities, and key interfaces of this component]\n\n**Key Responsibilities:**\n- [Responsibility 1]\n- [Responsibility 2]\n\n**Interfaces:**\n- [Interface descriptions]"
                                   component))
                         components
                         "\n\n"))
    "### Core Components\n\n[Define the main components of your system and their responsibilities]"))

(defun protagentic--extract-data-models-from-requirements (requirements-content)
  "Extract potential data models from REQUIREMENTS-CONTENT."
  (let ((models '()))
    (when (string-match-p "user\\|account\\|profile" requirements-content)
      (push "User" models))
    (when (string-match-p "document\\|file\\|content" requirements-content)
      (push "Document" models))
    (when (string-match-p "project\\|workspace" requirements-content)
      (push "Project" models))
    (when (string-match-p "task\\|item\\|entry" requirements-content)
      (push "Task" models))
    
    (reverse models)))

(defun protagentic--generate-data-models-section (data-models)
  "Generate the data models section from DATA-MODELS list."
  (if data-models
      (concat "### Core Data Models\n\n"
              (mapconcat (lambda (model)
                           (format "#### %s Model\n\n```\n%s {\n  id: identifier\n  // Add relevant fields based on requirements\n  created_at: timestamp\n  updated_at: timestamp\n}\n```\n\n[Describe the purpose and key attributes of this model]"
                                   model model))
                         data-models
                         "\n\n"))
    "### Core Data Models\n\n[Define the main data structures your system will use]"))

(defun protagentic--generate-design-overview (requirements-content)
  "Generate design overview based on REQUIREMENTS-CONTENT."
  "This design document provides the technical architecture and implementation approach for the feature requirements. It defines the system components, data models, interfaces, and technical decisions needed to implement the specified functionality.

The design follows established software engineering principles including separation of concerns, modularity, and testability. Each component is designed to have clear responsibilities and well-defined interfaces to enable maintainable and scalable implementation.")

(defun protagentic--extract-implementation-tasks (design-content)
  "Extract implementation tasks from DESIGN-CONTENT.
Returns a structured list of tasks based on the design components."
  (let ((tasks '()))
    ;; Generate standard implementation tasks
    (push '("Set up project structure and core interfaces"
            "Create directory structure and define main interfaces"
            "Establish project foundation and coding standards")
          tasks)
    
    (push '("Implement core data models"
            "Create data model classes with validation"
            "Write unit tests for data models")
          tasks)
    
    (push '("Build data access layer"
            "Implement repository pattern for data operations"
            "Add database integration and connection management")
          tasks)
    
    (push '("Create business logic components"
            "Implement core business rules and workflows"
            "Add service layer with proper error handling")
          tasks)
    
    (push '("Develop user interface components"
            "Create UI components and user interaction handlers"
            "Implement user experience workflows")
          tasks)
    
    (push '("Add integration and testing"
            "Write integration tests for component interactions"
            "Implement end-to-end testing for user workflows")
          tasks)
    
    (reverse tasks)))

(defun protagentic--generate-tasks-list (tasks)
  "Generate formatted task list from TASKS structure."
  (let ((task-counter 1))
    (mapconcat
     (lambda (task-group)
       (let ((main-task (car task-group))
             (sub-tasks (cdr task-group))
             (current-counter task-counter))
         (setq task-counter (1+ task-counter))
         (concat
          (format "- [ ] %d. %s\n" current-counter main-task)
          (when sub-tasks
            (mapconcat (lambda (sub-task)
                         (format "  - %s\n" sub-task))
                       sub-tasks
                       ""))
          (format "  - _Requirements: [Reference specific requirements]_\n"))))
     tasks
     "\n")))

;; Content parsing and extraction functions

(defun protagentic--parse-requirements-content (requirements-content)
  "Parse REQUIREMENTS-CONTENT and extract structured information.
Returns a plist with extracted user stories, acceptance criteria, and metadata."
  (let ((user-stories '())
        (acceptance-criteria '())
        (requirements-count 0))
    
    ;; Extract user stories using regex
    (let ((story-regex "\\*\\*User Story:\\*\\*\\s-*\\(.*?\\)$")
          (pos 0))
      (while (string-match story-regex requirements-content pos)
        (push (match-string 1 requirements-content) user-stories)
        (setq pos (match-end 0))))
    
    ;; Extract acceptance criteria
    (let ((criteria-regex "^\\s-*[0-9]+\\.\\s-+\\(WHEN\\|IF\\)\\s-+\\(.*?\\)$")
          (pos 0))
      (while (string-match criteria-regex requirements-content pos)
        (push (match-string 0 requirements-content) acceptance-criteria)
        (setq pos (match-end 0))))
    
    ;; Count requirements sections
    (setq requirements-count 
          (length (split-string requirements-content "### Requirement [0-9]+" t)))
    
    (list :user-stories (reverse user-stories)
          :acceptance-criteria (reverse acceptance-criteria)
          :requirements-count (max 0 (1- requirements-count))
          :has-introduction (string-match-p "## Introduction" requirements-content))))

(defun protagentic--parse-design-content (design-content)
  "Parse DESIGN-CONTENT and extract architectural information.
Returns a plist with components, data models, and technical decisions."
  (let ((components '())
        (data-models '())
        (technologies '())
        (sections '()))
    
    ;; Extract component sections
    (let ((component-regex "#### \\([^\\n]+\\) Component")
          (pos 0))
      (while (string-match component-regex design-content pos)
        (push (match-string 1 design-content) components)
        (setq pos (match-end 0))))
    
    ;; Extract data models
    (let ((model-regex "#### \\([^\\n]+\\) Model")
          (pos 0))
      (while (string-match model-regex design-content pos)
        (push (match-string 1 design-content) data-models)
        (setq pos (match-end 0))))
    
    ;; Extract technology mentions
    (let ((tech-regex "\\*\\*\\([^:]+\\):\\*\\*\\s-*\\([^\\n]+\\)")
          (pos 0))
      (while (string-match tech-regex design-content pos)
        (push (cons (match-string 1 design-content) 
                    (match-string 2 design-content)) technologies)
        (setq pos (match-end 0))))
    
    ;; Identify main sections
    (dolist (section '("Overview" "Architecture" "Components" "Data Models" 
                       "Error Handling" "Testing Strategy"))
      (when (string-match-p (format "## %s" section) design-content)
        (push section sections)))
    
    (list :components (reverse components)
          :data-models (reverse data-models)
          :technologies (reverse technologies)
          :sections (reverse sections)
          :has-architecture (member "Architecture" sections)
          :has-testing-strategy (member "Testing Strategy" sections))))

(defun protagentic--extract-task-context (design-content requirements-content)
  "Extract context for task generation from DESIGN-CONTENT and REQUIREMENTS-CONTENT.
Returns information needed to generate relevant implementation tasks."
  (let ((design-info (protagentic--parse-design-content design-content))
        (requirements-info (protagentic--parse-requirements-content requirements-content)))
    
    (list :components (plist-get design-info :components)
          :data-models (plist-get design-info :data-models)
          :technologies (plist-get design-info :technologies)
          :user-stories (plist-get requirements-info :user-stories)
          :requirements-count (plist-get requirements-info :requirements-count)
          :complexity-level (protagentic--assess-complexity design-info requirements-info))))

(defun protagentic--assess-complexity (design-info requirements-info)
  "Assess the complexity level based on DESIGN-INFO and REQUIREMENTS-INFO.
Returns 'simple', 'moderate', or 'complex'."
  (let ((component-count (length (plist-get design-info :components)))
        (model-count (length (plist-get design-info :data-models)))
        (requirements-count (plist-get requirements-info :requirements-count)))
    
    (cond
     ((and (< component-count 3) (< model-count 2) (< requirements-count 4))
      'simple)
     ((and (< component-count 6) (< model-count 5) (< requirements-count 8))
      'moderate)
     (t 'complex))))

(defun protagentic--generate-contextual-tasks (task-context)
  "Generate contextual implementation tasks based on TASK-CONTEXT.
Returns a list of tasks tailored to the specific project needs."
  (let ((components (plist-get task-context :components))
        (data-models (plist-get task-context :data-models))
        (complexity (plist-get task-context :complexity-level))
        (tasks '()))
    
    ;; Always start with project setup
    (push '("Set up project structure and dependencies"
            "Initialize project with proper directory structure"
            "Configure build tools and development environment"
            "Set up version control and basic documentation")
          tasks)
    
    ;; Data layer tasks
    (when data-models
      (push `("Implement core data models"
              ,(format "Create %s with proper validation" 
                       (string-join data-models ", "))
              "Write unit tests for data model operations"
              "Add data serialization and persistence logic")
            tasks))
    
    ;; Component-specific tasks
    (when components
      (dolist (component components)
        (push `(,(format "Implement %s" component)
                ,(format "Create %s with defined interfaces" component)
                ,(format "Write unit tests for %s functionality" component)
                "Integrate with other system components")
              tasks)))
    
    ;; Integration and testing based on complexity
    (cond
     ((eq complexity 'simple)
      (push '("Add basic integration testing"
              "Write integration tests for core workflows"
              "Add end-to-end testing for main user paths")
            tasks))
     
     ((eq complexity 'moderate)
      (push '("Implement comprehensive testing strategy"
              "Create unit tests for all components"
              "Add integration tests for component interactions"
              "Implement end-to-end testing with multiple scenarios")
            tasks))
     
     ((eq complexity 'complex)
      (push '("Build robust testing and validation framework"
              "Implement comprehensive unit test coverage"
              "Create integration test suite with mocking"
              "Add performance and load testing"
              "Implement continuous integration pipeline")
            tasks)))
    
    ;; Final integration
    (push '("Finalize integration and documentation"
            "Ensure all components work together seamlessly"
            "Complete API documentation and user guides"
            "Perform final testing and quality assurance")
          tasks)
    
    (reverse tasks)))

(defun protagentic--enhance-tasks-with-requirements (tasks requirements-content)
  "Enhance TASKS with specific requirement references from REQUIREMENTS-CONTENT.
Returns tasks with proper requirement traceability."
  (let ((requirements-info (protagentic--parse-requirements-content requirements-content)))
    (cl-loop for task in tasks
             for task-index from 1
             collect (let ((main-task (car task))
                          (sub-tasks (cdr task)))
                      (append 
                       (list main-task)
                       sub-tasks
                       (list (format "_Requirements: %s_" 
                                   (protagentic--map-task-to-requirements 
                                    task-index main-task requirements-info))))))))

(defun protagentic--map-task-to-requirements (task-index main-task requirements-info)
  "Map TASK-INDEX and MAIN-TASK to relevant requirements from REQUIREMENTS-INFO.
Returns a string with requirement references."
  (let ((requirements-count (plist-get requirements-info :requirements-count)))
    (cond
     ;; Project setup maps to all requirements
     ((string-match-p "setup\\|structure\\|project" main-task)
      (if (> requirements-count 0)
          (format "1.1-%d.1" requirements-count)
        "All requirements"))
     
     ;; Data models typically map to data-related requirements
     ((string-match-p "data\\|model" main-task)
      (if (> requirements-count 2) "2.1-3.2" "1.2-2.1"))
     
     ;; Component implementation maps to functional requirements
     ((string-match-p "implement\\|component" main-task)
      (let ((req-num (min (+ task-index 1) requirements-count)))
        (format "%d.1-%d.3" req-num req-num)))
     
     ;; Testing maps to validation requirements
     ((string-match-p "test\\|integration" main-task)
      "All acceptance criteria")
     
     ;; Default mapping
     (t (format "%d.1" (min task-index requirements-count))))))

(provide 'protagentic-templates)

;;; protagentic-templates.el ends here