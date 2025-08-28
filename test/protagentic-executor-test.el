;;; protagentic-executor-test.el --- Tests for protagentic-executor -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Comprehensive tests for the protagentic-executor module covering the entire
;; task execution flow from parsing to file generation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'protagentic-executor)

;; Test fixtures and mocks

(defvar protagentic-executor-test-tasks-content
  "# Implementation Plan

- [ ] 1. Set up project structure and core interfaces
  - Create directory structure for models, services, and API components
  - Define interfaces that establish system boundaries
  - _Requirements: 1.1_

- [x] 2. Implement data models and validation
  - Write TypeScript interfaces for all data models
  - Implement validation functions for data integrity
  - _Requirements: 2.1, 3.3, 1.2_

- [ ] 3. Create storage mechanism
  - Implement database connection utilities
  - Write connection management code
  - _Requirements: 2.1, 3.3_"
  "Sample tasks content for testing.")

(defvar protagentic-executor-test-requirements-content
  "# Requirements Document

## Introduction
Test requirements for the system.

## Requirements

### Requirement 1.1
**User Story:** As a developer, I want to set up the project structure, so that I can organize code properly.

#### Acceptance Criteria
1. WHEN the project is initialized THEN the system SHALL create proper directory structure
2. WHEN interfaces are defined THEN the system SHALL establish clear boundaries

### Requirement 2.1
**User Story:** As a user, I want data validation, so that data integrity is maintained.

#### Acceptance Criteria
1. WHEN data is input THEN the system SHALL validate it
2. WHEN validation fails THEN the system SHALL show appropriate errors"
  "Sample requirements content for testing.")

(defvar protagentic-executor-test-design-content
  "# Design Document

## Overview
This is a test design document for the system architecture.

## Architecture
The system follows a layered architecture with clear separation of concerns.

## Components and Interfaces
- **Data Layer**: Handles data persistence and validation
- **Service Layer**: Contains business logic
- **API Layer**: Provides external interfaces"
  "Sample design content for testing.")

(defvar protagentic-executor-test-llm-response-standard
  "I'll implement the project structure setup task.

**src/models/Task.js**
```javascript
class Task {
  constructor(id, title, description) {
    this.id = id;
    this.title = title;
    this.description = description;
    this.completed = false;
  }

  validate() {
    if (!this.title || this.title.trim().length === 0) {
      throw new Error('Task title is required');
    }
    return true;
  }
}

module.exports = Task;
```

**src/services/TaskService.js**
```javascript
const Task = require('../models/Task');

class TaskService {
  constructor() {
    this.tasks = [];
  }

  createTask(title, description) {
    const task = new Task(Date.now(), title, description);
    task.validate();
    this.tasks.push(task);
    return task;
  }

  getAllTasks() {
    return this.tasks;
  }
}

module.exports = TaskService;
```

**test/TaskService.test.js**
```javascript
const TaskService = require('../src/services/TaskService');

describe('TaskService', () => {
  let service;

  beforeEach(() => {
    service = new TaskService();
  });

  test('should create a new task', () => {
    const task = service.createTask('Test Task', 'Test Description');
    expect(task.title).toBe('Test Task');
    expect(task.completed).toBe(false);
  });

  test('should get all tasks', () => {
    service.createTask('Task 1', 'Description 1');
    service.createTask('Task 2', 'Description 2');
    
    const tasks = service.getAllTasks();
    expect(tasks).toHaveLength(2);
  });
});
```"
  "Sample LLM response with standard format.")

(defvar protagentic-executor-test-llm-response-alternative
  "Here's the implementation for the storage mechanism:

Create file: storage/DatabaseConnection.js

```javascript
class DatabaseConnection {
  constructor(config) {
    this.config = config;
    this.connected = false;
  }

  async connect() {
    // Connection logic here
    this.connected = true;
    return this;
  }

  async disconnect() {
    this.connected = false;
  }
}

module.exports = DatabaseConnection;
```

File: storage/ConnectionManager.js
```javascript
const DatabaseConnection = require('./DatabaseConnection');

class ConnectionManager {
  constructor() {
    this.connections = new Map();
  }

  async getConnection(name, config) {
    if (!this.connections.has(name)) {
      const conn = new DatabaseConnection(config);
      await conn.connect();
      this.connections.set(name, conn);
    }
    return this.connections.get(name);
  }
}

module.exports = ConnectionManager;
```"
  "Sample LLM response with alternative format.")

;; Mock functions

(defun protagentic-executor-test--mock-spec (name)
  "Create a mock spec structure for testing."
  (list :name name
        :requirements-file (format ".protagentic/specs/%s/requirements.md" name)
        :design-file (format ".protagentic/specs/%s/design.md" name)
        :tasks-file (format ".protagentic/specs/%s/tasks.md" name)))

(defun protagentic-executor-test--mock-read-file-content (filename)
  "Mock file reading function."
  (cond
   ((string-match-p "requirements\\.md$" filename)
    protagentic-executor-test-requirements-content)
   ((string-match-p "design\\.md$" filename)
    protagentic-executor-test-design-content)
   ((string-match-p "tasks\\.md$" filename)
    protagentic-executor-test-tasks-content)
   (t nil)))

(defun protagentic-executor-test--mock-llm-generate-content (prompt content-type context)
  "Mock LLM content generation."
  (cond
   ((string-match-p "project structure" prompt)
    protagentic-executor-test-llm-response-standard)
   ((string-match-p "storage mechanism" prompt)
    protagentic-executor-test-llm-response-alternative)
   (t "No implementation generated")))

;; Test suites

(ert-deftest protagentic-executor-test-parse-tasks ()
  "Test task parsing from markdown content."
  (let ((tasks (protagentic-executor-parse-tasks protagentic-executor-test-tasks-content)))
    
    ;; Should parse 3 tasks
    (should (= (length tasks) 3))
    
    ;; First task should be pending
    (let ((task1 (nth 0 tasks)))
      (should (equal (protagentic-task-description task1) 
                     "Set up project structure and core interfaces"))
      (should (eq (protagentic-task-status task1) 'pending))
      (should (equal (protagentic-task-requirements-refs task1) '("1.1")))
      (should (= (length (protagentic-task-details task1)) 2)))
    
    ;; Second task should be completed
    (let ((task2 (nth 1 tasks)))
      (should (equal (protagentic-task-description task2)
                     "Implement data models and validation"))
      (should (eq (protagentic-task-status task2) 'completed))
      (should (equal (protagentic-task-requirements-refs task2) '("2.1" "3.3" "1.2"))))
    
    ;; Third task should be pending
    (let ((task3 (nth 2 tasks)))
      (should (equal (protagentic-task-description task3)
                     "Create storage mechanism"))
      (should (eq (protagentic-task-status task3) 'pending))
      (should (equal (protagentic-task-requirements-refs task3) '("2.1" "3.3"))))))

(ert-deftest protagentic-executor-test-find-next-task ()
  "Test finding the next pending task."
  (let ((tasks (protagentic-executor-parse-tasks protagentic-executor-test-tasks-content)))
    (let ((next-task (protagentic-executor--find-next-task tasks)))
      
      ;; Should find the first pending task
      (should next-task)
      (should (equal (protagentic-task-description next-task)
                     "Set up project structure and core interfaces"))
      (should (eq (protagentic-task-status next-task) 'pending)))))

(ert-deftest protagentic-executor-test-build-prompt ()
  "Test code generation prompt building."
  (let* ((spec (protagentic-executor-test--mock-spec "test-spec"))
         (task (make-protagentic-task
                :description "Set up project structure"
                :details '("Create directory structure" "Define interfaces")
                :requirements-refs '("1.1" "2.1")))
         (context (make-protagentic-execution-context
                   :spec spec
                   :current-task task)))
    
    ;; Mock the file reading function
    (cl-letf (((symbol-function 'protagentic--read-file-content)
               #'protagentic-executor-test--mock-read-file-content))
      
      (let ((prompt (protagentic-executor--build-code-generation-prompt task context)))
        
        ;; Should contain task description
        (should (string-match-p "Set up project structure" prompt))
        
        ;; Should contain task details
        (should (string-match-p "Create directory structure" prompt))
        (should (string-match-p "Define interfaces" prompt))
        
        ;; Should contain requirements references
        (should (string-match-p "1.1, 2.1" prompt))
        
        ;; Should contain requirements document
        (should (string-match-p "Test requirements for the system" prompt))
        
        ;; Should contain design document
        (should (string-match-p "test design document" prompt))
        
        ;; Should contain output format instructions
        (should (string-match-p "\\*\\*filename\\.ext\\*\\*" prompt))
        (should (string-match-p "```language" prompt))))))

(ert-deftest protagentic-executor-test-parse-generated-files-standard ()
  "Test parsing files from standard LLM response format."
  (let ((files (protagentic-executor--parse-generated-files 
                protagentic-executor-test-llm-response-standard)))
    
    ;; Should extract 3 files
    (should (= (length files) 3))
    
    ;; Check first file
    (let ((task-file (assoc "src/models/Task.js" files)))
      (should task-file)
      (should (string-match-p "class Task" (cdr task-file)))
      (should (string-match-p "validate()" (cdr task-file))))
    
    ;; Check second file
    (let ((service-file (assoc "src/services/TaskService.js" files)))
      (should service-file)
      (should (string-match-p "class TaskService" (cdr service-file)))
      (should (string-match-p "createTask" (cdr service-file))))
    
    ;; Check test file
    (let ((test-file (assoc "test/TaskService.test.js" files)))
      (should test-file)
      (should (string-match-p "describe('TaskService'" (cdr test-file)))
      (should (string-match-p "should create a new task" (cdr test-file))))))

(ert-deftest protagentic-executor-test-parse-generated-files-alternative ()
  "Test parsing files from alternative LLM response formats."
  (let ((files (protagentic-executor--parse-generated-files 
                protagentic-executor-test-llm-response-alternative)))
    
    ;; Should extract 2 files
    (should (= (length files) 2))
    
    ;; Check first file (Create file: format)
    (let ((db-file (assoc "storage/DatabaseConnection.js" files)))
      (should db-file)
      (should (string-match-p "class DatabaseConnection" (cdr db-file)))
      (should (string-match-p "async connect" (cdr db-file))))
    
    ;; Check second file (File: format)
    (let ((manager-file (assoc "storage/ConnectionManager.js" files)))
      (should manager-file)
      (should (string-match-p "class ConnectionManager" (cdr manager-file)))
      (should (string-match-p "getConnection" (cdr manager-file))))))

(ert-deftest protagentic-executor-test-validate-generated-code ()
  "Test validation of generated code files."
  (let* ((valid-files '(("test.js" . "console.log('test');")))
         (invalid-files '(("" . "content") ("test.js" . "")))
         (context (make-protagentic-execution-context)))
    
    ;; Valid files should pass
    (let ((result (protagentic-executor--validate-generated-code valid-files context)))
      (should (protagentic-executor--validation-passed-p result))
      (should (null (plist-get result :errors))))
    
    ;; Invalid files should fail
    (let ((result (protagentic-executor--validate-generated-code invalid-files context)))
      (should-not (protagentic-executor--validation-passed-p result))
      (should (plist-get result :errors))
      (should (= (length (plist-get result :errors)) 2)))))

(ert-deftest protagentic-executor-test-execute-single-task ()
  "Test executing a single task end-to-end."
  (let* ((spec (protagentic-executor-test--mock-spec "test-spec"))
         (task (make-protagentic-task
                :description "Set up project structure and core interfaces"
                :details '("Create directory structure" "Define interfaces")
                :requirements-refs '("1.1")))
         (context (make-protagentic-execution-context
                   :spec spec
                   :current-task task
                   :project-root "/tmp/test-project"))
         (written-files '()))
    
    ;; Mock dependencies
    (cl-letf (((symbol-function 'protagentic--read-file-content)
               #'protagentic-executor-test--mock-read-file-content)
              ((symbol-function 'protagentic-llm-generate-content)
               #'protagentic-executor-test--mock-llm-generate-content)
              ((symbol-function 'protagentic-executor--write-generated-files)
               (lambda (files ctx)
                 (setq written-files files)))
              ((symbol-function 'protagentic-executor--update-task-status)
               (lambda (task status)
                 (setf (protagentic-task-status task) status))))
      
      (let ((result (protagentic-executor--execute-single-task task context)))
        
        ;; Should succeed
        (should (plist-get result :success))
        (should-not (plist-get result :error))
        
        ;; Should have generated files
        (should (plist-get result :files))
        (should (= (length (plist-get result :files)) 3))
        
        ;; Should have written files
        (should written-files)
        (should (= (length written-files) 3))
        
        ;; Task should be marked as completed
        (should (eq (protagentic-task-status task) 'completed))))))

(ert-deftest protagentic-executor-test-execute-single-task-failure ()
  "Test handling of task execution failures."
  (let* ((spec (protagentic-executor-test--mock-spec "test-spec"))
         (task (make-protagentic-task
                :description "Failing task"
                :details '("This will fail")
                :requirements-refs '("1.1")))
         (context (make-protagentic-execution-context
                   :spec spec
                   :current-task task)))
    
    ;; Mock LLM to return nil (failure)
    (cl-letf (((symbol-function 'protagentic--read-file-content)
               #'protagentic-executor-test--mock-read-file-content)
              ((symbol-function 'protagentic-llm-generate-content)
               (lambda (prompt content-type context) nil)))
      
      (let ((result (protagentic-executor--execute-single-task task context)))
        
        ;; Should fail
        (should-not (plist-get result :success))
        (should (plist-get result :error))
        (should (string-match-p "Failed to generate code" (plist-get result :error)))))))

(ert-deftest protagentic-executor-test-file-writing ()
  "Test file writing functionality."
  (let* ((files '(("test/example.js" . "console.log('test');")
                  ("src/main.js" . "function main() { return 'hello'; }")))
         (context (make-protagentic-execution-context
                   :project-root "/tmp/test-project"))
         (write-operations '()))
    
    ;; Mock the write-generated-files function to just record what would be written
    (cl-letf (((symbol-function 'protagentic-executor--write-generated-files)
               (lambda (files ctx)
                 (dolist (file files)
                   (let* ((filename (car file))
                          (content (cdr file))
                          (full-path (expand-file-name filename 
                                                      (protagentic-execution-context-project-root ctx))))
                     (push (list full-path content) write-operations))))))
      
      (protagentic-executor--write-generated-files files context)
      
      ;; Should have recorded write operations
      (should (= (length write-operations) 2))
      (should (cl-some (lambda (op) (string-match-p "test/example.js" (car op))) write-operations))
      (should (cl-some (lambda (op) (string-match-p "src/main.js" (car op))) write-operations)))))

(ert-deftest protagentic-executor-test-execute-next-task-return-value ()
  "Test that execute-next-task returns the completed task with generated files."
  (let* ((spec (protagentic-executor-test--mock-spec "test-spec"))
         (written-files '()))
    
    ;; Mock dependencies
    (cl-letf (((symbol-function 'protagentic--read-file-content)
               #'protagentic-executor-test--mock-read-file-content)
              ((symbol-function 'protagentic-llm-generate-content)
               #'protagentic-executor-test--mock-llm-generate-content)
              ((symbol-function 'protagentic-llm-available-p)
               (lambda () t))
              ((symbol-function 'protagentic--detect-project-root)
               (lambda () "/tmp/test-project"))
              ((symbol-function 'protagentic-executor--detect-technology-stack)
               (lambda () '("JavaScript")))
              ((symbol-function 'protagentic-executor--scan-existing-files)
               (lambda () '()))
              ((symbol-function 'protagentic-executor--write-generated-files)
               (lambda (files ctx) 
                 (setq written-files (mapcar #'car files)))))
      
      (let ((result (protagentic-executor-execute-next-task spec)))
        
        ;; Should return the completed task
        (should result)
        (should (protagentic-task-p result))
        
        ;; Task should have generated files populated
        (should (protagentic-task-generated-files result))
        (should (= (length (protagentic-task-generated-files result)) 3))
        
        ;; Task should be marked as completed
        (should (eq (protagentic-task-status result) 'completed))))))

(ert-deftest protagentic-executor-test-integration-full-flow ()
  "Integration test for the complete task execution flow."
  (let* ((spec (protagentic-executor-test--mock-spec "integration-test"))
         (written-files '())
         (updated-tasks '()))
    
    ;; Mock all external dependencies
    (cl-letf (((symbol-function 'protagentic--read-file-content)
               #'protagentic-executor-test--mock-read-file-content)
              ((symbol-function 'protagentic-llm-generate-content)
               #'protagentic-executor-test--mock-llm-generate-content)
              ((symbol-function 'protagentic-llm-available-p)
               (lambda () t))
              ((symbol-function 'protagentic--detect-project-root)
               (lambda () "/tmp/integration-test"))
              ((symbol-function 'protagentic-executor--detect-technology-stack)
               (lambda () '("JavaScript")))
              ((symbol-function 'protagentic-executor--scan-existing-files)
               (lambda () '()))
              ((symbol-function 'file-exists-p)
               (lambda (path) t))
              ((symbol-function 'make-directory)
               (lambda (dir recursive) t))
              ((symbol-function 'protagentic-executor--write-generated-files)
               (lambda (files ctx)
                 (dolist (file files)
                   (push (expand-file-name (car file) 
                                          (protagentic-execution-context-project-root ctx)) 
                         written-files))))
              ((symbol-function 'protagentic-executor--update-task-status)
               (lambda (task status)
                 (push (cons task status) updated-tasks)
                 (setf (protagentic-task-status task) status))))
      
      ;; Execute next task
      (let ((tasks (protagentic-executor--get-tasks spec)))
        (should (= (length tasks) 3))
        
        (let ((next-task (protagentic-executor--find-next-task tasks)))
          (should next-task)
          
          (let* ((context (protagentic-executor--build-execution-context spec next-task))
                 (result (protagentic-executor--execute-single-task next-task context)))
            
            ;; Should succeed
            (should (plist-get result :success))
            
            ;; Should have generated and written files
            (should (>= (length written-files) 3))
            
            ;; Should have updated task status
            (should updated-tasks)
            (should (eq (cdar updated-tasks) 'completed))))))))

;; Test runner helper

(defun protagentic-executor-test-run-all ()
  "Run all protagentic-executor tests."
  (interactive)
  (ert-run-tests-batch "protagentic-executor-test-"))

(provide 'protagentic-executor-test)

;;; protagentic-executor-test.el ends here