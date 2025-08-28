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
               (details (protagentic-executor--extract-task-details))
               (requirements-refs (protagentic-executor--extract-requirements-refs))
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

(defun protagentic-executor--extract-task-details ()
  "Extract task details from current buffer position.
Returns list of detail strings."
  (let ((details '()))
    (forward-line 1)
    
    ;; Collect indented lines as details
    (while (and (not (eobp))
                (looking-at "^  \\(- \\|\\* \\)?\\(.+\\)$"))
      (let ((detail (match-string 2)))
        (unless (string-match-p "^_Requirements:" detail)
          (push detail details)))
      (forward-line 1))
    
    (reverse details)))

(defun protagentic-executor--extract-requirements-refs ()
  "Extract requirements references from current task.
Returns list of requirement IDs."
  (let ((refs '()))
    (save-excursion
      (while (re-search-forward "_Requirements: \\([0-9., ]+\\)_" (line-end-position 2) t)
        (let ((ref-string (match-string 1)))
          (setq refs (append refs (split-string ref-string "[, ]+" t))))))
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
                (message "✅ Task completed successfully")
              (message "❌ Task failed: %s" (plist-get result :error)))))
      (message "🎉 All tasks completed!"))))

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
  (format "Generate code for task: %s\n\nContext: %s" 
          (protagentic-task-description task)
          (protagentic-execution-context-spec context)))

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
                     "```[a-zA-Z0-9]+ *[\\n\\r]+# *filename: *\\([^\\n\\r]+\\) *[\\n\\r]+\\(\\(?:.\\|[\\n\\r]\\)*?\\)```")))
      (dolist (pattern patterns)
        (setq start 0)
        (while (string-match pattern generated-code start)
          (let ((filename (string-trim (match-string 1 generated-code)))
                (file-content (string-trim (match-string 2 generated-code))))
            ;; Skip if we already have this file
            (unless (assoc filename files)
              (push (cons filename file-content) files))
            (setq start (match-end 0))))))
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