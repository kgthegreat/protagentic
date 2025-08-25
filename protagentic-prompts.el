;;; protagentic-prompts.el --- LLM prompt templates and management -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; LLM prompt template management for Protagentic including system prompts,
;; context builders, and customization support.

;;; Code:

(require 'cl-lib)

(defgroup protagentic-prompts nil
  "LLM prompt template settings for Protagentic."
  :group 'protagentic
  :prefix "protagentic-prompts-")

(defcustom protagentic-prompts-use-custom-templates nil
  "Whether to use custom prompt templates instead of built-in ones."
  :type 'boolean
  :group 'protagentic-prompts)

;; Built-in system prompts

(defconst protagentic-prompts--requirements-system-prompt
  "You are an expert software requirements analyst with extensive experience in agile development and requirements engineering. Your task is to create comprehensive, testable requirements documents that serve as the foundation for successful software projects.

Key principles:
- Write clear, unambiguous requirements that any developer can understand
- Use EARS (Easy Approach to Requirements Syntax) format for acceptance criteria
- Focus on user value and business outcomes
- Consider edge cases, error conditions, and non-functional requirements
- Ensure requirements are testable and verifiable
- Maintain traceability between user stories and acceptance criteria

Your output should be a well-structured markdown document that follows software engineering best practices."
  "System prompt for requirements generation.")

(defconst protagentic-prompts--design-system-prompt
  "You are a senior software architect with deep expertise in system design, software engineering patterns, and modern development practices. Your task is to create comprehensive technical designs that translate requirements into implementable solutions.

Key principles:
- Design for scalability, maintainability, and testability
- Choose appropriate architectural patterns and technologies
- Consider security, performance, and reliability from the start
- Design clear interfaces and component boundaries
- Document technical decisions and their rationale
- Follow SOLID principles and clean architecture concepts
- Consider both current needs and future extensibility

Your output should be a detailed technical design document that provides clear guidance for implementation teams."
  "System prompt for design generation.")

(defconst protagentic-prompts--tasks-system-prompt
  "You are an experienced technical lead and project manager who excels at breaking down complex software projects into manageable, actionable tasks. Your role is to create implementation plans that enable efficient, high-quality development.

Key principles:
- Create tasks that build incrementally and logically on each other
- Ensure each task is specific, actionable, and has clear completion criteria
- Prioritize test-driven development and quality assurance
- Maintain traceability to requirements and design decisions
- Consider dependencies and integration points
- Focus on coding, testing, and technical implementation activities
- Sequence tasks to enable early validation and feedback
- Make tasks appropriately sized for development iterations

Your output should be a comprehensive implementation plan that serves as a roadmap for development teams."
  "System prompt for tasks generation.")

;; Prompt template structures

(cl-defstruct protagentic-prompt-template
  "Structure for storing prompt templates."
  name
  system-prompt
  user-template
  context-builders
  validation-rules
  examples)

;; Built-in prompt templates

(defvar protagentic-prompts--requirements-template
  (make-protagentic-prompt-template
   :name "requirements"
   :system-prompt protagentic-prompts--requirements-system-prompt
   :user-template "Generate a comprehensive requirements document for the following feature:

Feature Description:
{FEATURE_DESCRIPTION}

{CONTEXT_SECTION}

Please create a requirements document with the following structure:

# Requirements Document

## Introduction
[Provide a clear introduction explaining the feature and its purpose]

## Requirements

### Requirement 1
**User Story:** As a [role], I want [capability], so that [benefit]

#### Acceptance Criteria
1. WHEN [trigger] THEN the system SHALL [response]
2. IF [condition] THEN the system SHALL [behavior]
[Continue with 2-5 acceptance criteria in EARS format]

[Continue with additional requirements as needed - typically 3-7 requirements total]

Guidelines:
- Use EARS format (Easy Approach to Requirements Syntax) for acceptance criteria
- Each requirement should have a clear user story and 2-5 acceptance criteria
- Focus on specific, testable behaviors
- Consider edge cases, error conditions, and security requirements
- Ensure requirements are complete, unambiguous, and verifiable
- Include both functional and non-functional requirements where relevant"
   :context-builders '(protagentic-prompts--build-requirements-context)
   :validation-rules '(protagentic-prompts--validate-requirements-output)))

(defvar protagentic-prompts--design-template
  (make-protagentic-prompt-template
   :name "design"
   :system-prompt protagentic-prompts--design-system-prompt
   :user-template "Create a comprehensive technical design document based on these requirements:

{REQUIREMENTS_CONTENT}

{CONTEXT_SECTION}

Please create a design document with the following structure:

# Design Document

## Overview
[Summarize the technical approach and key design decisions]

## Architecture
### System Architecture
[Describe the overall system architecture and component relationships]

### Technology Stack
- **Frontend:** [Technology choices and rationale]
- **Backend:** [Technology choices and rationale]
- **Database:** [Data storage approach and rationale]
- **External Services:** [Third-party integrations if any]

## Components and Interfaces
[Define major system components, their responsibilities, and interfaces]

## Data Models
[Specify data structures, relationships, validation rules, and persistence strategy]

## Error Handling
[Define error categories, handling strategies, and user communication approaches]

## Testing Strategy
[Outline unit, integration, and end-to-end testing approaches with coverage goals]

Guidelines:
- Base all design decisions on the provided requirements
- Choose modern, well-established technologies appropriate for the project scale
- Design for scalability, maintainability, and testability
- Include specific technical details and rationale for major decisions
- Consider security, performance, reliability, and accessibility requirements
- Design clear component boundaries and interfaces
- Plan for monitoring, logging, and observability"
   :context-builders '(protagentic-prompts--build-design-context)
   :validation-rules '(protagentic-prompts--validate-design-output)))

(defvar protagentic-prompts--tasks-template
  (make-protagentic-prompt-template
   :name "tasks"
   :system-prompt protagentic-prompts--tasks-system-prompt
   :user-template "Generate an actionable implementation plan based on this design and requirements:

Requirements:
{REQUIREMENTS_CONTENT}

Design:
{DESIGN_CONTENT}

{CONTEXT_SECTION}

Please create an implementation plan with the following structure:

# Implementation Plan

- [ ] 1. [First major task - typically project setup and foundation]
  - [Specific implementation details]
  - [Testing and validation requirements]
  - _Requirements: [Reference specific requirements by number]_

- [ ] 2. [Second major task - typically core data models or infrastructure]
  - [Specific implementation details]
  - [Integration considerations]
  - _Requirements: [Reference specific requirements by number]_

[Continue with additional tasks in logical sequence...]

Guidelines:
- Create 6-12 tasks that build incrementally on each other
- Each task should focus on specific coding, testing, or technical configuration activities
- Include requirement references for full traceability
- Prioritize test-driven development and early validation
- Ensure tasks are actionable by a developer without additional clarification
- Focus ONLY on technical implementation - avoid user testing, deployment, or business activities
- Sequence tasks to enable early integration and feedback
- Make each task appropriately sized (1-3 days of development work)
- Include both implementation and testing activities in each task"
   :context-builders '(protagentic-prompts--build-tasks-context)
   :validation-rules '(protagentic-prompts--validate-tasks-output)))

;; Context builders

(defun protagentic-prompts--build-requirements-context (generation-context)
  "Build additional context for requirements generation from GENERATION-CONTEXT.
Returns a string with relevant context information."
  (let ((context-parts '()))
    
    ;; Add project context if available
    (when (protagentic-generation-context-project-files generation-context)
      (push (format "Project Context:
- Existing files: %s
- This suggests a %s project"
                    (string-join (protagentic-generation-context-project-files generation-context) ", ")
                    (protagentic-prompts--infer-project-type 
                     (protagentic-generation-context-project-files generation-context)))
            context-parts))
    
    ;; Add spec name context
    (when (protagentic-generation-context-spec-name generation-context)
      (push (format "Spec Name: %s" (protagentic-generation-context-spec-name generation-context))
            context-parts))
    
    (if context-parts
        (concat "\n" (string-join context-parts "\n\n") "\n")
      "")))

(defun protagentic-prompts--build-design-context (generation-context)
  "Build additional context for design generation from GENERATION-CONTEXT.
Returns a string with relevant context information."
  (let ((context-parts '()))
    
    ;; Add project technology context
    (when (protagentic-generation-context-project-files generation-context)
      (let ((tech-stack (protagentic-prompts--infer-technology-stack 
                         (protagentic-generation-context-project-files generation-context))))
        (when tech-stack
          (push (format "Existing Technology Context:
- Detected technologies: %s
- Consider compatibility and consistency with existing codebase"
                        (string-join tech-stack ", "))
                context-parts))))
    
    ;; Add architectural guidance based on project size
    (let ((complexity (protagentic-prompts--assess-project-complexity generation-context)))
      (push (format "Project Complexity: %s
- %s"
                    (capitalize (symbol-name complexity))
                    (protagentic-prompts--get-complexity-guidance complexity))
            context-parts))
    
    (if context-parts
        (concat "\n" (string-join context-parts "\n\n") "\n")
      "")))

(defun protagentic-prompts--build-tasks-context (generation-context)
  "Build additional context for tasks generation from GENERATION-CONTEXT.
Returns a string with relevant context information."
  (let ((context-parts '()))
    
    ;; Add development environment context
    (when (protagentic-generation-context-project-files generation-context)
      (let ((dev-context (protagentic-prompts--analyze-development-context 
                          (protagentic-generation-context-project-files generation-context))))
        (when dev-context
          (push (format "Development Environment Context:
%s" dev-context) context-parts))))
    
    ;; Add task sizing guidance
    (let ((complexity (protagentic-prompts--assess-project-complexity generation-context)))
      (push (format "Task Sizing Guidance:
- Project complexity: %s
- %s"
                    (capitalize (symbol-name complexity))
                    (protagentic-prompts--get-task-sizing-guidance complexity))
            context-parts))
    
    (if context-parts
        (concat "\n" (string-join context-parts "\n\n") "\n")
      "")))

;; Context analysis helpers

(defun protagentic-prompts--infer-project-type (file-list)
  "Infer project type from FILE-LIST.
Returns a string describing the likely project type."
  (cond
   ((cl-some (lambda (f) (string-match-p "\\.el$" f)) file-list) "Emacs Lisp")
   ((cl-some (lambda (f) (string-match-p "\\.js$\\|\\.ts$" f)) file-list) "JavaScript/TypeScript")
   ((cl-some (lambda (f) (string-match-p "\\.py$" f)) file-list) "Python")
   ((cl-some (lambda (f) (string-match-p "\\.java$" f)) file-list) "Java")
   ((cl-some (lambda (f) (string-match-p "\\.rs$" f)) file-list) "Rust")
   ((cl-some (lambda (f) (string-match-p "\\.go$" f)) file-list) "Go")
   (t "Multi-language or Unknown")))

(defun protagentic-prompts--infer-technology-stack (file-list)
  "Infer technology stack from FILE-LIST.
Returns a list of detected technologies."
  (let ((technologies '()))
    (when (cl-some (lambda (f) (string-match-p "package\\.json" f)) file-list)
      (push "Node.js/npm" technologies))
    (when (cl-some (lambda (f) (string-match-p "requirements\\.txt\\|setup\\.py" f)) file-list)
      (push "Python" technologies))
    (when (cl-some (lambda (f) (string-match-p "Cargo\\.toml" f)) file-list)
      (push "Rust/Cargo" technologies))
    (when (cl-some (lambda (f) (string-match-p "go\\.mod" f)) file-list)
      (push "Go modules" technologies))
    (when (cl-some (lambda (f) (string-match-p "\\.el$" f)) file-list)
      (push "Emacs Lisp" technologies))
    technologies))

(defun protagentic-prompts--assess-project-complexity (generation-context)
  "Assess project complexity from GENERATION-CONTEXT.
Returns 'simple, 'moderate, or 'complex."
  (let ((req-length (length (or (protagentic-generation-context-requirements-content generation-context) "")))
        (design-length (length (or (protagentic-generation-context-design-content generation-context) "")))
        (file-count (length (or (protagentic-generation-context-project-files generation-context) '()))))
    
    (cond
     ((and (< req-length 2000) (< design-length 3000) (< file-count 5)) 'simple)
     ((and (< req-length 5000) (< design-length 8000) (< file-count 15)) 'moderate)
     (t 'complex))))

(defun protagentic-prompts--get-complexity-guidance (complexity)
  "Get architectural guidance based on COMPLEXITY level."
  (cl-case complexity
    (simple "Focus on simplicity and rapid development. Prefer straightforward solutions over complex patterns.")
    (moderate "Balance simplicity with extensibility. Use established patterns but avoid over-engineering.")
    (complex "Emphasize modularity, scalability, and maintainability. Use appropriate design patterns and architectural layers.")
    (t "Consider project scale and choose appropriate architectural complexity.")))

(defun protagentic-prompts--get-task-sizing-guidance (complexity)
  "Get task sizing guidance based on COMPLEXITY level."
  (cl-case complexity
    (simple "Create 4-6 focused tasks. Each task should be completable in 1-2 days.")
    (moderate "Create 6-10 well-defined tasks. Each task should be completable in 1-3 days.")
    (complex "Create 8-12 comprehensive tasks. Each task should be completable in 2-4 days with clear milestones.")
    (t "Size tasks appropriately for the project scope and team capacity.")))

(defun protagentic-prompts--analyze-development-context (file-list)
  "Analyze development context from FILE-LIST.
Returns a string with development environment insights."
  (let ((context-parts '()))
    
    (when (cl-some (lambda (f) (string-match-p "test" f)) file-list)
      (push "- Existing test infrastructure detected - maintain testing patterns" context-parts))
    
    (when (cl-some (lambda (f) (string-match-p "README\\|readme" f)) file-list)
      (push "- Documentation practices in place - update relevant docs" context-parts))
    
    (when (cl-some (lambda (f) (string-match-p "\\.git" f)) file-list)
      (push "- Git version control in use - consider commit strategies" context-parts))
    
    (if context-parts
        (string-join context-parts "\n")
      "- Standard development practices recommended")))

;; Prompt building and customization

(defun protagentic-prompts-build-prompt (template-name generation-context)
  "Build complete prompt for TEMPLATE-NAME using GENERATION-CONTEXT.
Returns a formatted prompt string ready for LLM consumption."
  (let ((template (protagentic-prompts--get-template template-name)))
    (unless template
      (error "Unknown prompt template: %s" template-name))
    
    (let* ((system-prompt (protagentic-prompt-template-system-prompt template))
           (user-template (protagentic-prompt-template-user-template template))
           (context-builders (protagentic-prompt-template-context-builders template))
           (context-content (protagentic-prompts--build-context context-builders generation-context))
           (user-prompt (protagentic-prompts--substitute-placeholders 
                        user-template generation-context context-content)))
      
      ;; Combine system and user prompts
      (format "%s\n\n%s" system-prompt user-prompt))))

(defun protagentic-prompts--get-template (template-name)
  "Get prompt template by TEMPLATE-NAME.
Returns custom template if available, otherwise built-in template."
  (if protagentic-prompts-use-custom-templates
      (or (protagentic-prompts--load-custom-template template-name)
          (protagentic-prompts--get-builtin-template template-name))
    (protagentic-prompts--get-builtin-template template-name)))

(defun protagentic-prompts--get-builtin-template (template-name)
  "Get built-in template by TEMPLATE-NAME."
  (cl-case (intern template-name)
    (requirements protagentic-prompts--requirements-template)
    (design protagentic-prompts--design-template)
    (tasks protagentic-prompts--tasks-template)
    (t nil)))

(defun protagentic-prompts--build-context (context-builders generation-context)
  "Build context using CONTEXT-BUILDERS and GENERATION-CONTEXT.
Returns combined context string."
  (when context-builders
    (let ((context-parts (mapcar (lambda (builder)
                                   (funcall builder generation-context))
                                 context-builders)))
      (string-join (cl-remove-if #'string-empty-p context-parts) "\n\n"))))

(defun protagentic-prompts--substitute-placeholders (template generation-context context-content)
  "Substitute placeholders in TEMPLATE using GENERATION-CONTEXT and CONTEXT-CONTENT.
Returns template with all placeholders replaced."
  (let ((result template))
    
    ;; Substitute main content placeholders
    (setq result (replace-regexp-in-string 
                  "{FEATURE_DESCRIPTION}"
                  (or (protagentic-generation-context-feature-description generation-context) "")
                  result))
    
    (setq result (replace-regexp-in-string 
                  "{REQUIREMENTS_CONTENT}"
                  (or (protagentic-generation-context-requirements-content generation-context) "")
                  result))
    
    (setq result (replace-regexp-in-string 
                  "{DESIGN_CONTENT}"
                  (or (protagentic-generation-context-design-content generation-context) "")
                  result))
    
    ;; Substitute context section
    (setq result (replace-regexp-in-string 
                  "{CONTEXT_SECTION}"
                  (or context-content "")
                  result))
    
    result))

;; Custom template management

(defun protagentic-prompts--load-custom-template (template-name)
  "Load custom template by TEMPLATE-NAME from configuration.
Returns custom template or nil if not found."
  ;; This would load from the custom prompts file
  ;; For now, return nil (not implemented)
  nil)

(defun protagentic-prompts-save-custom-template (template-name template)
  "Save custom TEMPLATE with TEMPLATE-NAME to configuration.
TEMPLATE should be a protagentic-prompt-template structure."
  ;; This would save to the custom prompts file
  ;; Implementation would go here
  (message "Custom template saving not yet implemented"))

;; Validation functions

(defun protagentic-prompts--validate-requirements-output (content)
  "Validate requirements CONTENT meets expected format.
Returns t if valid, otherwise returns error message."
  (cond
   ((not (string-match-p "# Requirements Document" content))
    "Missing main heading '# Requirements Document'")
   ((not (string-match-p "## Introduction" content))
    "Missing Introduction section")
   ((not (string-match-p "## Requirements" content))
    "Missing Requirements section")
   ((not (string-match-p "\\*\\*User Story:\\*\\*" content))
    "Missing user stories")
   ((not (string-match-p "#### Acceptance Criteria" content))
    "Missing acceptance criteria sections")
   (t t)))

(defun protagentic-prompts--validate-design-output (content)
  "Validate design CONTENT meets expected format.
Returns t if valid, otherwise returns error message."
  (cond
   ((not (string-match-p "# Design Document" content))
    "Missing main heading '# Design Document'")
   ((not (string-match-p "## Overview" content))
    "Missing Overview section")
   ((not (string-match-p "## Architecture" content))
    "Missing Architecture section")
   ((not (string-match-p "## Components and Interfaces" content))
    "Missing Components section")
   (t t)))

(defun protagentic-prompts--validate-tasks-output (content)
  "Validate tasks CONTENT meets expected format.
Returns t if valid, otherwise returns error message."
  (cond
   ((not (string-match-p "# Implementation Plan" content))
    "Missing main heading '# Implementation Plan'")
   ((not (string-match-p "- \\[ \\]" content))
    "Missing task checkboxes")
   ((not (string-match-p "_Requirements:" content))
    "Missing requirement references")
   (t t)))

;; Public API

(defun protagentic-prompts-generate-requirements-prompt (generation-context)
  "Generate requirements prompt using GENERATION-CONTEXT."
  (protagentic-prompts-build-prompt "requirements" generation-context))

(defun protagentic-prompts-generate-design-prompt (generation-context)
  "Generate design prompt using GENERATION-CONTEXT."
  (protagentic-prompts-build-prompt "design" generation-context))

(defun protagentic-prompts-generate-tasks-prompt (generation-context)
  "Generate tasks prompt using GENERATION-CONTEXT."
  (protagentic-prompts-build-prompt "tasks" generation-context))

(defun protagentic-prompts-validate-output (content content-type)
  "Validate CONTENT for CONTENT-TYPE.
Returns t if valid, otherwise returns error message."
  (cl-case (intern content-type)
    (requirements (protagentic-prompts--validate-requirements-output content))
    (design (protagentic-prompts--validate-design-output content))
    (tasks (protagentic-prompts--validate-tasks-output content))
    (t "Unknown content type for validation")))

(provide 'protagentic-prompts)

;;; protagentic-prompts.el ends here