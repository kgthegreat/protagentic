;;; protagentic-core.el --- Core data structures and workflow logic -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Core data structures and workflow logic for Protagentic.
;; Manages spec state, phase detection, and workflow validation.

;;; Code:

(require 'cl-lib)
(require 'protagentic-utils)

;; Phase constants
(defconst protagentic--phases '(requirements design tasks)
  "List of workflow phases in order.")

(defconst protagentic--phase-files
  '((requirements . "requirements.md")
    (design . "design.md")
    (tasks . "tasks.md"))
  "Mapping of phases to their corresponding file names.")

;; Spec data structure
(cl-defstruct (protagentic-spec (:constructor protagentic--make-spec))
  "Data structure representing a Protagentic spec."
  name
  root-path
  requirements-file
  design-file
  tasks-file
  current-phase)

(defun protagentic--create-spec-struct (spec-name)
  "Create a protagentic-spec structure for SPEC-NAME.
Returns nil if spec directory cannot be determined."
  (let ((spec-dir (protagentic--get-spec-directory spec-name)))
    (when spec-dir
      (protagentic--make-spec
       :name spec-name
       :root-path spec-dir
       :requirements-file (expand-file-name "requirements.md" spec-dir)
       :design-file (expand-file-name "design.md" spec-dir)
       :tasks-file (expand-file-name "tasks.md" spec-dir)
       :current-phase (protagentic--detect-current-phase spec-dir)))))

(defun protagentic--detect-current-phase (spec-path)
  "Detect the current phase of a spec based on existing files in SPEC-PATH.
Returns the most advanced completed phase, or nil if no files exist."
  (let ((requirements-exists (protagentic--file-exists-p 
                              (expand-file-name "requirements.md" spec-path)))
        (design-exists (protagentic--file-exists-p 
                        (expand-file-name "design.md" spec-path)))
        (tasks-exists (protagentic--file-exists-p 
                       (expand-file-name "tasks.md" spec-path))))
    (cond
     (tasks-exists 'tasks)
     (design-exists 'design)
     (requirements-exists 'requirements)
     (t nil))))

(defun protagentic--get-next-phase (current-phase)
  "Get the next phase after CURRENT-PHASE.
Returns nil if CURRENT-PHASE is the last phase."
  (let ((phase-index (cl-position current-phase protagentic--phases)))
    (when (and phase-index (< phase-index (1- (length protagentic--phases))))
      (nth (1+ phase-index) protagentic--phases))))

(defun protagentic--get-previous-phase (current-phase)
  "Get the phase before CURRENT-PHASE.
Returns nil if CURRENT-PHASE is the first phase."
  (let ((phase-index (cl-position current-phase protagentic--phases)))
    (when (and phase-index (> phase-index 0))
      (nth (1- phase-index) protagentic--phases))))

(defun protagentic--validate-phase-prerequisites (phase spec)
  "Validate that prerequisites for PHASE are met in SPEC.
Returns t if prerequisites are satisfied, otherwise signals an error."
  (let ((phase-index (cl-position phase protagentic--phases)))
    (unless phase-index
      (error "Invalid phase: %s" phase))
    
    ;; Check that all previous phases are complete
    (cl-loop for i from 0 below phase-index
             for required-phase = (nth i protagentic--phases)
             for required-file = (cdr (assq required-phase protagentic--phase-files))
             for file-path = (expand-file-name required-file (protagentic-spec-root-path spec))
             unless (protagentic--file-exists-p file-path)
             do (error "Cannot proceed to %s phase: %s phase is not complete (missing %s)"
                       phase required-phase required-file))
    t))

(defun protagentic--get-spec-status (spec)
  "Get a status summary for SPEC.
Returns a plist with phase completion status and next actions."
  (let* ((spec-path (protagentic-spec-root-path spec))
         (requirements-exists (protagentic--file-exists-p 
                               (protagentic-spec-requirements-file spec)))
         (design-exists (protagentic--file-exists-p 
                         (protagentic-spec-design-file spec)))
         (tasks-exists (protagentic--file-exists-p 
                        (protagentic-spec-tasks-file spec)))
         (current-phase (protagentic--detect-current-phase spec-path))
         (next-phase (protagentic--get-next-phase current-phase)))
    
    (list :spec-name (protagentic-spec-name spec)
          :requirements-complete requirements-exists
          :design-complete design-exists
          :tasks-complete tasks-exists
          :current-phase current-phase
          :next-phase next-phase
          :workflow-complete (and requirements-exists design-exists tasks-exists))))

(defun protagentic--find-current-spec ()
  "Find the spec associated with the current buffer or directory.
Returns a protagentic-spec structure or nil if no spec is found."
  (let* ((current-file (or buffer-file-name default-directory))
         (project-root (protagentic--detect-project-root))
         (specs-dir (when project-root
                      (expand-file-name protagentic-spec-directory project-root))))
    
    (when (and specs-dir (file-directory-p specs-dir))
      ;; Check if current file is within a spec directory
      (when (and current-file (string-prefix-p specs-dir current-file))
        (let* ((relative-path (file-relative-name current-file specs-dir))
               (spec-name (car (split-string relative-path "/"))))
          (when spec-name
            (protagentic--create-spec-struct spec-name)))))))

(defun protagentic--get-workflow-guidance (spec)
  "Get guidance message for next steps in the workflow for SPEC.
Returns a string with actionable next steps."
  (let ((status (protagentic--get-spec-status spec)))
    (cond
     ((plist-get status :workflow-complete)
      "Workflow complete! All phases (requirements, design, tasks) are finished.")
     
     ((not (plist-get status :requirements-complete))
      "Start by creating requirements: M-x protagentic-create-spec")
     
     ((not (plist-get status :design-complete))
      "Requirements complete. Generate design: M-x protagentic-generate-design")
     
     ((not (plist-get status :tasks-complete))
      "Design complete. Generate tasks: M-x protagentic-generate-tasks")
     
     (t "Unknown workflow state."))))

(defun protagentic--validate-spec-name (spec-name)
  "Validate that SPEC-NAME is suitable for use.
Returns the sanitized spec name or signals an error if invalid."
  (when (string-empty-p (string-trim spec-name))
    (error "Spec name cannot be empty"))
  
  (let ((sanitized (protagentic--sanitize-spec-name spec-name)))
    (when (string-empty-p sanitized)
      (error "Spec name '%s' contains no valid characters" spec-name))
    
    (when (> (length sanitized) 50)
      (error "Spec name too long (max 50 characters after sanitization)"))
    
    sanitized))

(defun protagentic--check-spec-exists (spec-name)
  "Check if a spec with SPEC-NAME already exists.
Returns t if spec exists, nil otherwise."
  (let ((spec-dir (protagentic--get-spec-directory spec-name)))
    (and spec-dir (file-directory-p spec-dir))))

(defun protagentic--get-phase-file-path (spec phase)
  "Get the file path for PHASE in SPEC.
Returns the full path to the phase file."
  (let ((filename (cdr (assq phase protagentic--phase-files))))
    (unless filename
      (error "Invalid phase: %s" phase))
    (expand-file-name filename (protagentic-spec-root-path spec))))

;; Enhanced workflow validation and guidance

(defun protagentic--validate-workflow-prerequisites (phase spec &optional interactive)
  "Validate workflow prerequisites for PHASE in SPEC.
If INTERACTIVE is t, provides guided assistance for missing prerequisites.
Returns t if valid, otherwise provides helpful guidance or signals error."
  (condition-case err
      (protagentic--validate-phase-prerequisites phase spec)
    (error
     (if interactive
         (protagentic--handle-prerequisite-error phase spec (error-message-string err))
       (signal (car err) (cdr err))))))

(defun protagentic--handle-prerequisite-error (phase spec error-message)
  "Handle prerequisite validation error for PHASE in SPEC with ERROR-MESSAGE.
Provides interactive guidance to resolve the issue."
  (let ((missing-phase (protagentic--extract-missing-phase error-message)))
    (with-output-to-temp-buffer "*Protagentic Workflow Help*"
      (princ (format "Protagentic: Workflow Prerequisite Missing\n"))
      (princ "==========================================\n\n")
      (princ (format "Spec: %s\n" (protagentic-spec-name spec)))
      (princ (format "Attempted Phase: %s\n" phase))
      (princ (format "Issue: %s\n\n" error-message))
      
      (princ "Workflow Order:\n")
      (princ "  1. Requirements (create user stories and acceptance criteria)\n")
      (princ "  2. Design (define architecture and technical approach)\n")
      (princ "  3. Tasks (generate implementation checklist)\n\n")
      
      (when missing-phase
        (princ (format "Next Step: Complete the %s phase first\n\n" missing-phase))
        (pcase (intern missing-phase)
          ('requirements
           (princ "To create requirements:\n")
           (princ "  M-x protagentic-create-spec\n")
           (princ "  - Provide a clear feature description\n")
           (princ "  - Review and refine the generated requirements\n"))
          ('design
           (princ "To generate design:\n")
           (princ "  M-x protagentic-generate-design\n")
           (princ "  - Ensure requirements are complete first\n")
           (princ "  - Review and enhance the generated design\n"))
          (tasks
           (princ "To generate tasks:\n")
           (princ "  M-x protagentic-generate-tasks\n")
           (princ "  - Ensure design is complete first\n")
           (princ "  - Review the generated implementation plan\n"))))
      
      (princ "\nCurrent Status:\n")
      (let ((status (protagentic--get-spec-status spec)))
        (princ (format "  Requirements: %s\n" 
                       (if (plist-get status :requirements-complete) "✓ Complete" "✗ Missing")))
        (princ (format "  Design: %s\n" 
                       (if (plist-get status :design-complete) "✓ Complete" "✗ Missing")))
        (princ (format "  Tasks: %s\n" 
                       (if (plist-get status :tasks-complete) "✓ Complete" "✗ Missing")))))
    
    ;; Ask user if they want to proceed with the missing phase
    (when missing-phase
      (when (y-or-n-p (format "Would you like to complete the %s phase now? " missing-phase))
        (pcase (intern missing-phase)
          ('requirements (call-interactively 'protagentic-create-spec))
          ('design (call-interactively 'protagentic-generate-design))
          ('tasks (call-interactively 'protagentic-generate-tasks)))))
    
    (error "Workflow prerequisite not met. See *Protagentic Workflow Help* for guidance")))

(defun protagentic--extract-missing-phase (error-message)
  "Extract the missing phase name from ERROR-MESSAGE.
Returns the phase name as a string, or nil if not found."
  (cond
   ((string-match-p "requirements.*not complete" error-message) "requirements")
   ((string-match-p "design.*not complete" error-message) "design")
   ((string-match-p "tasks.*not complete" error-message) "tasks")
   (t nil)))

(defun protagentic--validate-spec-name-interactive (spec-name)
  "Validate SPEC-NAME interactively with helpful guidance.
Returns sanitized name or provides guidance for improvement."
  (condition-case err
      (protagentic--validate-spec-name spec-name)
    (error
     (let ((error-msg (error-message-string err)))
       (with-output-to-temp-buffer "*Protagentic Name Help*"
         (princ "Protagentic: Spec Name Validation\n")
         (princ "=================================\n\n")
         (princ (format "Provided name: '%s'\n" spec-name))
         (princ (format "Issue: %s\n\n" error-msg))
         
         (princ "Spec Name Requirements:\n")
         (princ "  - Cannot be empty or only whitespace\n")
         (princ "  - Must contain at least one valid character (a-z, 0-9, -, _)\n")
         (princ "  - Maximum 50 characters after sanitization\n")
         (princ "  - Will be converted to kebab-case (lowercase with hyphens)\n\n")
         
         (princ "Examples of good spec names:\n")
         (princ "  - 'user-authentication'\n")
         (princ "  - 'file-upload-system'\n")
         (princ "  - 'api-rate-limiting'\n")
         (princ "  - 'dashboard-widgets'\n\n")
         
         (princ "The name will be used for:\n")
         (princ "  - Directory name in .protagentic/specs/\n")
         (princ "  - File organization and navigation\n")
         (princ "  - Reference in status displays\n"))
       
       (let ((new-name (read-string "Enter a new spec name: " 
                                    (protagentic--suggest-spec-name spec-name))))
         (protagentic--validate-spec-name-interactive new-name))))))

(defun protagentic--suggest-spec-name (invalid-name)
  "Suggest a corrected spec name based on INVALID-NAME.
Returns a suggested name that might pass validation."
  (let ((suggestion (downcase (string-trim invalid-name))))
    ;; Replace spaces and underscores with hyphens
    (setq suggestion (replace-regexp-in-string "[ _]+" "-" suggestion))
    ;; Remove invalid characters
    (setq suggestion (replace-regexp-in-string "[^a-z0-9-]" "" suggestion))
    ;; Remove multiple consecutive hyphens
    (setq suggestion (replace-regexp-in-string "-+" "-" suggestion))
    ;; Remove leading/trailing hyphens
    (setq suggestion (string-trim suggestion "-"))
    ;; Truncate if too long
    (when (> (length suggestion) 50)
      (setq suggestion (substring suggestion 0 50)))
    ;; Provide fallback if empty
    (if (string-empty-p suggestion)
        "my-feature"
      suggestion)))

(defun protagentic--validate-file-accessibility (file-path operation)
  "Validate that FILE-PATH is accessible for OPERATION.
OPERATION should be 'read', 'write', or 'delete'.
Provides helpful guidance if file is not accessible."
  (let ((exists (file-exists-p file-path))
        (readable (and (file-exists-p file-path) (file-readable-p file-path)))
        (writable (file-writable-p file-path))
        (parent-writable (file-writable-p (file-name-directory file-path))))
    
    (pcase (intern operation)
      ('read
       (cond
        ((not exists)
         (error "File does not exist: %s" file-path))
        ((not readable)
         (error "File is not readable (check permissions): %s" file-path))))
      
      ('write
       (cond
        ((and exists (not writable))
         (error "File is not writable (check permissions): %s" file-path))
        ((and (not exists) (not parent-writable))
         (error "Cannot create file (parent directory not writable): %s" file-path))))
      
      (delete
       (cond
        ((not exists)
         (error "Cannot delete file (does not exist): %s" file-path))
        ((not writable)
         (error "Cannot delete file (not writable): %s" file-path))))
      
      (t (error "Invalid operation: %s" operation)))
    
    t))

(defun protagentic--provide-workflow-guidance (spec)
  "Provide contextual workflow guidance for SPEC.
Returns a detailed guidance message based on current spec state."
  (let* ((status (protagentic--get-spec-status spec))
         (current-phase (plist-get status :current-phase))
         (next-phase (plist-get status :next-phase))
         (workflow-complete (plist-get status :workflow-complete)))
    
    (cond
     (workflow-complete
      "🎉 Congratulations! Your spec workflow is complete.\n\nAll phases (requirements, design, tasks) are finished. You can now:\n- Review the implementation tasks\n- Begin coding based on the task list\n- Use the tasks as a development roadmap")
     
     ((null current-phase)
      "📝 Getting Started\n\nYour spec journey begins with requirements gathering.\n\nNext step: Run M-x protagentic-create-spec to:\n- Define user stories\n- Specify acceptance criteria\n- Establish clear feature boundaries")
     
     ((eq current-phase 'requirements)
      "🏗️ Ready for Design Phase\n\nYour requirements are complete! Time to create the technical design.\n\nNext step: Run M-x protagentic-generate-design to:\n- Define system architecture\n- Specify components and interfaces\n- Plan data models and error handling")
     
     ((eq current-phase 'design)
      "✅ Ready for Implementation Planning\n\nYour design is solid! Now let's break it down into actionable tasks.\n\nNext step: Run M-x protagentic-generate-tasks to:\n- Create implementation checklist\n- Organize tasks by priority\n- Link tasks to requirements for traceability")
     
     (t
      (format "Current phase: %s\nNext phase: %s\n\nUse M-x protagentic-show-status for detailed information."
              current-phase next-phase)))))

(provide 'protagentic-core)

;;; protagentic-core.el ends here