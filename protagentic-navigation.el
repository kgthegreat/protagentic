;;; protagentic-navigation.el --- Navigation and status commands -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Navigation commands for moving between spec documents and displaying
;; workflow status and progress information.

;;; Code:

(require 'cl-lib)
(require 'protagentic-core)
(require 'protagentic-utils)

(defun protagentic-navigation-open-requirements ()
  "Open requirements document for current spec.
If not in a spec context, prompts user to select a spec."
  (interactive)
  
  (protagentic--validate-project-context)
  
  (let ((spec (or (protagentic--find-current-spec)
                  (protagentic--prompt-for-spec-navigation "Open requirements for which spec? "))))
    
    (unless spec
      (user-error "No spec found. Create a spec first with M-x protagentic-create-spec"))
    
    (let ((requirements-file (protagentic-spec-requirements-file spec)))
      (if (protagentic--file-exists-p requirements-file)
          (progn
            (find-file requirements-file)
            (message "Opened requirements for spec '%s'" (protagentic-spec-name spec)))
        (if (y-or-n-p (format "Requirements file doesn't exist for '%s'. Create it? " 
                               (protagentic-spec-name spec)))
            (protagentic-create-spec (protagentic-spec-name spec))
          (user-error "Requirements file not found"))))))

(defun protagentic-navigation-open-design ()
  "Open design document for current spec.
If not in a spec context, prompts user to select a spec."
  (interactive)
  
  (protagentic--validate-project-context)
  
  (let ((spec (or (protagentic--find-current-spec)
                  (protagentic--prompt-for-spec-navigation "Open design for which spec? "))))
    
    (unless spec
      (user-error "No spec found. Create a spec first with M-x protagentic-create-spec"))
    
    (let ((design-file (protagentic-spec-design-file spec)))
      (if (protagentic--file-exists-p design-file)
          (progn
            (find-file design-file)
            (message "Opened design for spec '%s'" (protagentic-spec-name spec)))
        (if (y-or-n-p (format "Design file doesn't exist for '%s'. Generate it? " 
                               (protagentic-spec-name spec)))
            (protagentic-generate-design)
          (user-error "Design file not found"))))))

(defun protagentic-navigation-open-tasks ()
  "Open tasks document for current spec.
If not in a spec context, prompts user to select a spec."
  (interactive)
  
  (protagentic--validate-project-context)
  
  (let ((spec (or (protagentic--find-current-spec)
                  (protagentic--prompt-for-spec-navigation "Open tasks for which spec? "))))
    
    (unless spec
      (user-error "No spec found. Create a spec first with M-x protagentic-create-spec"))
    
    (let ((tasks-file (protagentic-spec-tasks-file spec)))
      (if (protagentic--file-exists-p tasks-file)
          (progn
            (find-file tasks-file)
            (message "Opened tasks for spec '%s'" (protagentic-spec-name spec)))
        (if (y-or-n-p (format "Tasks file doesn't exist for '%s'. Generate it? " 
                               (protagentic-spec-name spec)))
            (protagentic-generate-tasks)
          (user-error "Tasks file not found"))))))

(defun protagentic-navigation-show-status ()
  "Show status of current spec workflow.
Displays completion status and next recommended actions."
  (interactive)
  
  (protagentic--validate-project-context)
  
  (let ((spec (or (protagentic--find-current-spec)
                  (protagentic--prompt-for-spec-navigation "Show status for which spec? "))))
    
    (if spec
        (protagentic--display-spec-status spec)
      (protagentic--display-project-overview))))

(defun protagentic--display-spec-status (spec)
  "Display detailed status information for SPEC in a temporary buffer."
  (let ((status (protagentic--get-spec-status spec))
        (guidance (protagentic--get-workflow-guidance spec)))
    
    (with-output-to-temp-buffer "*Protagentic Status*"
      (princ (format "Protagentic Spec Status: %s\n" (protagentic-spec-name spec)))
      (princ "================================\n\n")
      
      ;; Phase completion status
      (princ "Phase Completion:\n")
      (princ (format "  Requirements: %s %s\n" 
                     (if (plist-get status :requirements-complete) "✓" "✗")
                     (if (plist-get status :requirements-complete) 
                         (format "(%s)" (file-name-nondirectory 
                                         (protagentic-spec-requirements-file spec)))
                       "(not created)")))
      (princ (format "  Design: %s %s\n" 
                     (if (plist-get status :design-complete) "✓" "✗")
                     (if (plist-get status :design-complete) 
                         (format "(%s)" (file-name-nondirectory 
                                         (protagentic-spec-design-file spec)))
                       "(not created)")))
      (princ (format "  Tasks: %s %s\n\n" 
                     (if (plist-get status :tasks-complete) "✓" "✗")
                     (if (plist-get status :tasks-complete) 
                         (format "(%s)" (file-name-nondirectory 
                                         (protagentic-spec-tasks-file spec)))
                       "(not created)")))
      
      ;; Current phase and next steps
      (princ (format "Current Phase: %s\n" 
                     (or (plist-get status :current-phase) "Not started")))
      (princ (format "Next Phase: %s\n\n" 
                     (or (plist-get status :next-phase) "Complete")))
      
      ;; Workflow guidance
      (princ "Next Steps:\n")
      (princ (format "  %s\n\n" guidance))
      
      ;; File paths
      (princ "File Locations:\n")
      (princ (format "  Spec Directory: %s\n" (protagentic-spec-root-path spec)))
      (princ (format "  Requirements: %s\n" (protagentic-spec-requirements-file spec)))
      (princ (format "  Design: %s\n" (protagentic-spec-design-file spec)))
      (princ (format "  Tasks: %s\n" (protagentic-spec-tasks-file spec)))
      
      ;; Quick actions
      (princ "\nQuick Actions:\n")
      (princ "  M-x protagentic-open-requirements - Open requirements document\n")
      (princ "  M-x protagentic-open-design - Open design document\n")
      (princ "  M-x protagentic-open-tasks - Open tasks document\n")
      (unless (plist-get status :workflow-complete)
        (cond
         ((not (plist-get status :requirements-complete))
          (princ "  M-x protagentic-create-spec - Create/update requirements\n"))
         ((not (plist-get status :design-complete))
          (princ "  M-x protagentic-generate-design - Generate design document\n"))
         ((not (plist-get status :tasks-complete))
          (princ "  M-x protagentic-generate-tasks - Generate implementation tasks\n")))))))

(defun protagentic--display-project-overview ()
  "Display overview of all specs in the current project."
  (let ((specs (protagentic--get-project-specs)))
    (with-output-to-temp-buffer "*Protagentic Overview*"
      (princ "Protagentic Project Overview\n")
      (princ "===========================\n\n")
      
      (if specs
          (progn
            (princ (format "Found %d spec(s) in current project:\n\n" (length specs)))
            (dolist (spec-name specs)
              (let* ((spec (protagentic--create-spec-struct spec-name))
                     (status (when spec (protagentic--get-spec-status spec))))
                (if status
                    (progn
                      (princ (format "%s:\n" spec-name))
                      (princ (format "  Progress: %s/%s phases complete\n"
                                     (+ (if (plist-get status :requirements-complete) 1 0)
                                        (if (plist-get status :design-complete) 1 0)
                                        (if (plist-get status :tasks-complete) 1 0))
                                     3))
                      (princ (format "  Status: %s\n"
                                     (if (plist-get status :workflow-complete)
                                         "Complete"
                                       (format "%s phase" 
                                               (or (plist-get status :current-phase) "Not started")))))
                      (princ "\n"))
                  (princ (format "%s: Error reading status\n\n" spec-name))))))
        (princ "No specs found in current project.\n\n"))
      
      (princ "Commands:\n")
      (princ "  M-x protagentic-create-spec - Create a new spec\n")
      (princ "  M-x protagentic-list-specs - List all specs with detailed status\n")
      (princ "  M-x protagentic-show-status - Show status for a specific spec\n"))))

(defun protagentic--prompt-for-spec-navigation (prompt-message)
  "Prompt user to select a spec for navigation with PROMPT-MESSAGE.
Returns a protagentic-spec structure or nil if cancelled."
  (let ((available-specs (protagentic--get-project-specs)))
    (if available-specs
        (let ((selected-spec (completing-read prompt-message available-specs nil t)))
          (when selected-spec
            (protagentic--create-spec-struct selected-spec)))
      (progn
        (message "No specs found in current project")
        nil))))

(defun protagentic-navigation-switch-to-phase (phase)
  "Switch to a specific PHASE document for the current spec.
PHASE should be one of 'requirements', 'design', or 'tasks'."
  (interactive 
   (list (intern (completing-read "Switch to phase: " 
                                  '("requirements" "design" "tasks") 
                                  nil t))))
  
  (pcase phase
    ('requirements (protagentic-navigation-open-requirements))
    ('design (protagentic-navigation-open-design))
    ('tasks (protagentic-navigation-open-tasks))
    (t (user-error "Invalid phase: %s" phase))))

(defun protagentic-navigation-next-phase ()
  "Navigate to the next phase document in the workflow.
Automatically determines the appropriate next phase based on current context."
  (interactive)
  
  (let ((spec (or (protagentic--find-current-spec)
                  (protagentic--prompt-for-spec-navigation "Navigate next phase for which spec? "))))
    
    (unless spec
      (user-error "No spec found"))
    
    (let* ((status (protagentic--get-spec-status spec))
           (next-phase (plist-get status :next-phase)))
      
      (cond
       ((eq next-phase 'design)
        (if (plist-get status :design-complete)
            (protagentic-navigation-open-design)
          (if (y-or-n-p "Design not generated yet. Generate it now? ")
              (protagentic-generate-design)
            (message "Generate design first with M-x protagentic-generate-design"))))
       
       ((eq next-phase 'tasks)
        (if (plist-get status :tasks-complete)
            (protagentic-navigation-open-tasks)
          (if (y-or-n-p "Tasks not generated yet. Generate them now? ")
              (protagentic-generate-tasks)
            (message "Generate tasks first with M-x protagentic-generate-tasks"))))
       
       ((null next-phase)
        (if (plist-get status :workflow-complete)
            (message "Workflow complete! All phases are finished.")
          (message "Start by creating requirements with M-x protagentic-create-spec")))
       
       (t (user-error "Unknown next phase: %s" next-phase))))))

(provide 'protagentic-navigation)

;;; protagentic-navigation.el ends here