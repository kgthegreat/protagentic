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
        nil)))) ; Return nil when no more tasks

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
        
        (message "🔧 DEBUG: Generated code length: %d" (if generated-code (length generated-code) 0))
        
        (if generated-code
            (let* ((files (protagentic-executor--parse-generated-files generated-code))
                   (validation-result (protagentic-executor--validate-generated-code files context)))
              
              (message "🔧 DEBUG: Parsed %d files from LLM response" (length files))
              (message "🔧 DEBUG: Validation passed: %s" (protagentic-executor--validation-passed-p validation-result))
              
              (if (protagentic-executor--validation-passed-p validation-result)
                  (progn
                    (message "🔧 DEBUG: Writing files to disk...")
                    (protagentic-executor--write-generated-files files context)
                    (protagentic-executor--update-task-status task 'completed)
                    (message "🔧 DEBUG: Task marked as completed")
                    (list :success t :files files))
                (progn
                  (message "❌ DEBUG: Validation failed: %s" (protagentic-executor--get-validation-errors validation-result))
                  (list :success nil :error (protagentic-executor--get-validation-errors validation-result)))))
          (progn
            (message "❌ DEBUG: No generated code received from LLM")
            (list :success nil :error "Failed to generate code"))))
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
  (message "🔍 DEBUG: Starting file parsing...")
  (message "🔍 DEBUG: Generated code length: %d characters" (length generated-code))
  
  (let ((files '())
        (start 0))
    
    ;; Find all **filename** markers first
    (let ((file-positions '()))
      (while (string-match "\\*\\*\\([^*]+\\.[a-zA-Z0-9]+\\)\\*\\*" generated-code start)
        (let ((filename (match-string 1 generated-code))
              (pos (match-end 0)))
          (push (cons filename pos) file-positions)
          (setq start (match-end 0))))
      
      (setq file-positions (nreverse file-positions))
      (message "🔍 DEBUG: Found %d file markers" (length file-positions))
      
      ;; Extract content between markers
      (dotimes (i (length file-positions))
        (let* ((current-file (nth i file-positions))
               (next-file (nth (1+ i) file-positions))
               (filename (car current-file))
               (content-start (cdr current-file))
               (content-end (if next-file 
                               (- (cdr next-file) (length (format "**%s**" (car next-file))))
                             (length generated-code)))
               (content (substring generated-code content-start content-end)))
          
          ;; Clean up content - remove leading/trailing whitespace and empty lines
          (setq content (string-trim content))
          (when (string-match "^```[a-zA-Z0-9]*\n" content)
            (setq content (substring content (match-end 0))))
          (when (string-match "\n```$" content)
            (setq content (substring content 0 (match-beginning 0))))
          (setq content (string-trim content))
          
          (when (> (length content) 0)
            (message "🔍 DEBUG: Found file: %s (content length: %d)" filename (length content))
            (push (cons filename content) files)))))
    
    (message "🔍 DEBUG: Total files parsed: %d" (length files))
    (dolist (file files)
      (message "🔍 DEBUG: File: %s" (car file)))
    
    (nreverse files)))

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
    (message "📁 DEBUG: Project root: %s" project-root)
    (message "📁 DEBUG: Writing %d files..." (length files))
    
    (dolist (file files)
      (let* ((filename (car file))
             (content (cdr file))
             (full-path (expand-file-name filename project-root)))
        
        (message "📁 DEBUG: Processing file: %s" filename)
        (message "📁 DEBUG: Full path: %s" full-path)
        (message "📁 DEBUG: Content length: %d" (length content))
        
        ;; Create directory if needed
        (let ((dir (file-name-directory full-path)))
          (message "📁 DEBUG: Directory: %s" dir)
          (unless (file-directory-p dir)
            (message "📁 DEBUG: Creating directory: %s" dir)
            (make-directory dir t))
          (message "📁 DEBUG: Directory exists: %s" (file-directory-p dir)))
        
        ;; Write file
        (condition-case err
            (progn
              (message "📁 DEBUG: Writing file...")
              (with-temp-file full-path
                (insert content))
              (message "✅ Generated file: %s" full-path)
              (message "✅ File exists after write: %s" (file-exists-p full-path))
              (when (file-exists-p full-path)
                (message "✅ File size: %d bytes" (nth 7 (file-attributes full-path)))))
          (error
           (message "❌ ERROR writing file %s: %s" full-path (error-message-string err))))))))

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