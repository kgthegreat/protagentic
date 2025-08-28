;;; protagentic-executor.el --- Task execution and code generation -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Task execution engine for Protagentic that executes implementation tasks
;; one by one and generates maintainable code following robust guidelines.

;;; Code:

(require 'cl-lib)
(require 'protagentic-core)
(require 'protagentic-llm)
(require 'protagentic-config)
(require 'protagentic-utils)

(defgroup protagentic-executor nil
  "Task execution settings for Protagentic."
  :group 'protagentic
  :prefix "protagentic-executor-")

;; Data structures
(cl-defstruct protagentic-task
  "Represents a single task from the tasks.md file."
  id
  description
  details
  requirements-refs
  status
  generated-files
  dependencies
  estimated-effort)

(cl-defstruct protagentic-execution-context
  "Context for task execution."
  spec
  current-task
  project-root
  technology-stack
  code-guidelines
  existing-files
  test-strategy)

;; Task parsing and execution
(defun protagentic-executor-parse-tasks (tasks-content)
  "Parse TASKS-CONTENT into structured task list.
Returns list of protagentic-task structures."
  (let ((tasks '())
        (task-id 0))
    (with-temp-buffer
      (insert tasks-content)
      (goto-char (point-min))
      
      ;; Find all task checkboxes (both pending and completed)
      (while (re-search-forward "^- \\[\\([x ]\\)\\] \\([0-9]+\\)\\. \\(.+\\)$" nil t)
        (let* ((checkbox-state (match-string 1))
               (task-num (match-string 2))
               (description (match-string 3))
               (details-and-refs (protagentic-executor--extract-task-details-and-refs))
               (details (car details-and-refs))
               (requirements-refs (cdr details-and-refs))
               (status (if (string= checkbox-state "x") 'completed 'pending)))
          
          (push (make-protagentic-task
                 :id (cl-incf task-id)
                 :description description
                 :details details
                 :requirements-refs requirements-refs
                 :status status
                 :generated-files '()
                 :dependencies '()
                 :estimated-effort 1)
                tasks))))
    
    (reverse tasks)))

(defun protagentic-executor--extract-task-details-and-refs ()
  "Extract task details and requirements refs from current buffer position.
Returns cons cell (details . requirements-refs)."
  (let ((details '())
        (requirements-refs '()))
    (forward-line 1)
    
    ;; Collect indented lines as details and extract requirements
    (while (and (not (eobp))
                (looking-at "^  \\(- \\|\\* \\)?\\(.+\\)$"))
      (let ((detail (match-string 2)))
        (if (string-match "_Requirements: \\([0-9., ]+\\)_" detail)
            ;; Extract requirements references
            (let ((ref-string (match-string 1 detail)))
              (setq requirements-refs (append requirements-refs (split-string ref-string "[, ]+" t))))
          ;; Regular detail line
          (push detail details)))
      (forward-line 1))
    
    (cons (reverse details) requirements-refs)))

(defun protagentic-executor--extract-task-details ()
  "Extract task details from current buffer position.
Returns list of detail strings."
  (car (protagentic-executor--extract-task-details-and-refs)))

(defun protagentic-executor--extract-requirements-refs ()
  "Extract requirements references from current task.
Returns list of requirement IDs."
  (let ((refs '())
        (start-pos (point)))
    (save-excursion
      ;; Look ahead to find the end of this task (next task or end of buffer)
      (let ((end-pos (if (re-search-forward "^- \\[" nil t)
                         (line-beginning-position)
                       (point-max))))
        (goto-char start-pos)
        (while (re-search-forward "_Requirements: \\([0-9., ]+\\)_" end-pos t)
          (let ((ref-string (match-string 1)))
            (setq refs (append refs (split-string ref-string "[, ]+" t)))))))
    refs))

;; Main execution function
(defun protagentic-executor-execute-next-task (&optional spec)
  "Execute the next pending task for SPEC.
If SPEC is not provided, finds the current spec automatically."
  (interactive)
  
  ;; Find spec if not provided
  (unless spec
    (setq spec (protagentic--find-current-spec)))
  
  (unless spec
    (error "No spec found. Create a spec first or navigate to a spec directory"))
  
  ;; Check prerequisites first
  (unless (protagentic-executor--check-prerequisites spec)
    (error "Prerequisites not met. See messages for details"))
  
  (let* ((tasks (protagentic-executor--get-tasks spec))
         (next-task (protagentic-executor--find-next-task tasks)))
    
    (if next-task
        (progn
          (message "🚀 Executing task: %s" (protagentic-task-description next-task))
          (let* ((context (protagentic-executor--build-execution-context spec next-task))
                 (result (protagentic-executor--execute-single-task next-task context)))
            (if (plist-get result :success)
                (progn
                  (message "✅ Task completed successfully")
                  ;; Update task with generated files
                  (setf (protagentic-task-generated-files next-task) 
                        (mapcar #'car (plist-get result :files)))
                  next-task) ; Return the completed task
              (progn
                (message "❌ Task failed: %s" (plist-get result :error))
                nil)))) ; Return nil on failure
      (progn
        (message "🎉 All tasks completed!")
        nil)))) ; Return nil when no more tasks))

;; Helper functions
(defun protagentic-executor--get-tasks (spec)
  "Get parsed tasks for SPEC."
  (let ((tasks-content (protagentic--read-file-content 
                       (protagentic-spec-tasks-file spec))))
    (protagentic-executor-parse-tasks tasks-content)))

(defun protagentic-executor--find-next-task (tasks)
  "Find next pending task in TASKS list."
  (cl-find-if (lambda (task) (eq (protagentic-task-status task) 'pending)) tasks))

(defun protagentic-executor--check-prerequisites (&optional spec)
  "Check if all prerequisites for task execution are met.
If SPEC is provided, skips the spec existence check.
Returns t if ready, nil otherwise with helpful messages."
  (let ((ready t))
    
    ;; Check if LLM is available
    (unless (protagentic-llm-available-p)
      (message "❌ LLM not available. Run M-x protagentic-setup-llm")
      (setq ready nil))
    
    ;; Check if we're in a project with specs (only if spec not provided)
    (unless (or spec (protagentic--find-current-spec))
      (message "❌ No spec found. Create a spec first with M-x protagentic-create-spec")
      (setq ready nil))
    
    ready))

(defun protagentic-executor--build-execution-context (spec task)
  "Build execution context for SPEC and TASK."
  (make-protagentic-execution-context
   :spec spec
   :current-task task
   :project-root (protagentic--detect-project-root)
   :technology-stack (protagentic-executor--detect-technology-stack)
   :existing-files (protagentic-executor--scan-existing-files)))

(defun protagentic-executor--execute-single-task (task context)
  "Execute single TASK with CONTEXT.
Returns execution result structure."
  (condition-case err
      (let* ((prompt (protagentic-executor--build-code-generation-prompt task context))
             (generated-code (protagentic-llm-generate-content prompt 'tasks context)))
        
        (if generated-code
            (let* ((files (protagentic-executor--parse-generated-files generated-code))
                   (validation-result (protagentic-executor--validate-generated-code files context)))
              
              (if (protagentic-executor--validation-passed-p validation-result)
                  (progn
                    (protagentic-executor--write-generated-files files context)
                    (protagentic-executor--update-task-status task 'completed)
                    (list :success t :files files))
                (list :success nil :error (protagentic-executor--get-validation-errors validation-result))))
          (list :success nil :error "Failed to generate code")))
    (error
     (list :success nil :error (format "Execution error: %s" (error-message-string err))))))

(defun protagentic-executor--build-code-generation-prompt (task context)
  "Build code generation prompt for TASK with CONTEXT."
  (let* ((spec (protagentic-execution-context-spec context))
         (requirements-content (protagentic--read-file-content (protagentic-spec-requirements-file spec)))
         (design-content (protagentic--read-file-content (protagentic-spec-design-file spec)))
         (task-desc (protagentic-task-description task))
         (task-details (string-join (protagentic-task-details task) "\n- "))
         (requirements-refs (string-join (protagentic-task-requirements-refs task) ", ")))
    
    (format "You are implementing a specific task from a software development spec. 

TASK TO IMPLEMENT:
%s

TASK DETAILS:
- %s

REQUIREMENTS REFERENCED: %s

REQUIREMENTS DOCUMENT:
%s

DESIGN DOCUMENT:
%s

INSTRUCTIONS:
1. Implement ONLY this specific task - do not implement functionality for other tasks
2. Follow the design patterns and architecture specified in the design document
3. Ensure your implementation satisfies the referenced requirements
4. Generate working, production-ready code with proper error handling
5. Include comprehensive tests for your implementation

OUTPUT FORMAT:
For each file you create, use this EXACT format:

**filename.ext**
```language
[file content here]
```

Example:
**src/TaskManager.js**
```javascript
class TaskManager {
  // implementation here
}
```

**test/TaskManager.test.js**
```javascript
describe('TaskManager', () => {
  // tests here
});
```

Generate all necessary files to complete this task."
            task-desc
            task-details
            requirements-refs
            (or requirements-content "No requirements document found")
            (or design-content "No design document found"))))

(defun protagentic-executor--parse-generated-files (generated-code)
  "Parse GENERATED-CODE into list of (filename . content) pairs."
  (let ((files '())
        (start 0))
    ;; Try multiple patterns to handle different LLM response formats
    (let ((patterns '(
                     ;; Primary format: ```filename: path/file.ext
                     "```filename: *\\([^\\n\\r]+\\) *[\\n\\r]+\\(\\(?:.\\|[\\n\\r]\\)*?\\)```"
                     ;; Language with comment filename: ```javascript\\n// filename: path/file.ext
                     "```[a-zA-Z0-9]+ *[\\n\\r]+// *filename: *\\([^\\n\\r]+\\) *[\\n\\r]+\\(\\(?:.\\|[\\n\\r]\\)*?\\)```"
                     ;; Language with comment filename (# style): ```python\\n# filename: path/file.ext  
                     "```[a-zA-Z0-9]+ *[\\n\\r]+# *filename: *\\([^\\n\\r]+\\) *[\\n\\r]+\\(\\(?:.\\|[\\n\\r]\\)*?\\)```"
                     ;; Common pattern: **filename.ext**\n```language
                     "\\*\\*\\([^*]+\\.[a-zA-Z0-9]+\\)\\*\\*[[:space:]]*\n```[a-zA-Z0-9]*[[:space:]]*\n\\(\\(?:.\\|\n\\)*?\\)```"
                     ;; Pattern: `filename.ext`\n```language
                     "`\\([^`]+\\.[a-zA-Z0-9]+\\)`[[:space:]]*\n```[a-zA-Z0-9]*[[:space:]]*\n\\(\\(?:.\\|\n\\)*?\\)```"
                     ;; Pattern: ### filename.ext or ## filename.ext
                     "#+\\s-*\\([^\\n\\r]+\\.[a-zA-Z0-9]+\\) *[\\n\\r]+```[a-zA-Z0-9]* *[\\n\\r]+\\(\\(?:.\\|[\\n\\r]\\)*?\\)```"
                     ;; Pattern: Create file: filename.ext
                     "[Cc]reate[^\n]*:[[:space:]]*\\([^\n]+\\.[a-zA-Z0-9]+\\)[[:space:]]*\n[[:space:]]*```[a-zA-Z0-9]*[[:space:]]*\n\\(\\(?:.\\|\n\\)*?\\)```"
                     ;; Pattern: File: filename.ext
                     "[Ff]ile:[[:space:]]*\\([^\n]+\\.[a-zA-Z0-9]+\\)[[:space:]]*\n```[a-zA-Z0-9]*[[:space:]]*\n\\(\\(?:.\\|\n\\)*?\\)```")))
      (dolist (pattern patterns)
        (setq start 0)
        (while (string-match pattern generated-code start)
          (let ((filename (string-trim (match-string 1 generated-code)))
                (file-content (string-trim (match-string 2 generated-code))))
            ;; Skip if we already have this file
            (unless (assoc filename files)
              (push (cons filename file-content) files))
            (setq start (match-end 0))))))
    
    ;; If no files found with patterns, try to extract from tool calls
    (when (null files)
      (setq files (protagentic-executor--parse-tool-calls generated-code)))
    
    (nreverse files)))

(defun protagentic-executor--parse-tool-calls (generated-code)
  "Parse tool calls like fsWrite from GENERATED-CODE.
Returns list of (filename . content) pairs."
  (let ((files '())
        (start 0))
    ;; Look for fsWrite tool calls - pattern matches XML-style tool calls
    (while (string-match "<invoke name=\"fsWrite\">\\s-*<parameter name=\"path\">\\([^<]+\\)</parameter>\\s-*<parameter name=\"text\">\\(\\(?:.\\|\n\\)*?\\)</parameter>\\s-*</invoke>" generated-code start)
      (let ((filename (string-trim (match-string 1 generated-code)))
            (file-content (string-trim (match-string 2 generated-code))))
        (unless (assoc filename files)
          (push (cons filename file-content) files))
        (setq start (match-end 0))))
    
    ;; Also look for simple file creation patterns in text
    (setq start 0)
    (while (string-match "I'll create \\([^\\s-]+\\.[a-zA-Z0-9]+\\):\\s-*\n\n```[a-zA-Z0-9]*\n\\(\\(?:.\\|\n\\)*?\\)\n```" generated-code start)
      (let ((filename (string-trim (match-string 1 generated-code)))
            (file-content (string-trim (match-string 2 generated-code))))
        (unless (assoc filename files)
          (push (cons filename file-content) files))
        (setq start (match-end 0))))
    
    files))

(defun protagentic-executor--validate-generated-code (files context)
  "Validate FILES against code quality standards in CONTEXT.
Returns validation result structure."
  (let ((errors '()))
    (dolist (file files)
      (let ((filename (car file))
            (content (cdr file)))
        (when (or (not filename) (string-empty-p filename))
          (push "Empty filename" errors))
        (when (or (not content) (string-empty-p content))
          (push (format "Empty content for %s" filename) errors))))
    (list :passed (null errors) :errors errors)))

(defun protagentic-executor--validation-passed-p (validation-result)
  "Check if VALIDATION-RESULT indicates passing validation."
  (plist-get validation-result :passed))

(defun protagentic-executor--get-validation-errors (validation-result)
  "Get validation errors from VALIDATION-RESULT."
  (string-join (plist-get validation-result :errors) "; "))

(defun protagentic-executor--write-generated-files (files context)
  "Write generated FILES using CONTEXT."
  (let ((project-root (protagentic-execution-context-project-root context)))
    (dolist (file files)
      (let* ((filename (car file))
             (content (cdr file))
             (full-path (expand-file-name filename project-root)))
        
        ;; Create directory if it doesn't exist
        (let ((dir (file-name-directory full-path)))
          (unless (file-exists-p dir)
            (make-directory dir t)))
        
        ;; Write file
        (with-temp-file full-path
          (insert content))
        
        (message "Generated: %s" filename)))))

(defun protagentic-executor--update-task-status (task new-status)
  "Update TASK status to NEW-STATUS."
  (setf (protagentic-task-status task) new-status))

(defun protagentic-executor--detect-technology-stack ()
  "Detect technology stack from project files."
  '("JavaScript")) ; Simple default for now

(defun protagentic-executor--scan-existing-files ()
  "Scan existing project files."
  (let ((project-root (protagentic--detect-project-root)))
    (when project-root
      (directory-files-recursively project-root "\\.[a-z]+$" nil))))

(provide 'protagentic-executor)

;;; protagentic-executor.el ends here