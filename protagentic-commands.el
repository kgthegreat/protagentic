;;; protagentic-commands.el --- Interactive commands for Protagentic -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Interactive commands for creating specs, generating design documents,
;; and managing the Protagentic workflow.

;;; Code:

(require 'cl-lib)
(require 'protagentic-core)
(require 'protagentic-utils)
(require 'protagentic-templates)
(require 'protagentic-generator)
(require 'protagentic-config)

(defun protagentic-commands-create-spec (spec-name)
  "Create a new spec with SPEC-NAME and initialize requirements phase.
Prompts for feature description and generates initial requirements document."
  (interactive "sSpec name: ")
  
  ;; Validate project context
  (protagentic--validate-project-context)
  
  ;; Validate and sanitize spec name
  (let ((sanitized-name (protagentic--validate-spec-name spec-name)))
    
    ;; Check if spec already exists
    (when (protagentic--check-spec-exists sanitized-name)
      (unless (y-or-n-p (format "Spec '%s' already exists. Overwrite? " sanitized-name))
        (user-error "Spec creation cancelled")))
    
    ;; Get feature description from user
    (let ((feature-description (protagentic--prompt-for-feature-description sanitized-name)))
      
      ;; Create spec structure
      (let ((spec (protagentic--create-spec-struct sanitized-name)))
        (unless spec
          (error "Failed to create spec structure for '%s'" sanitized-name))
        
        ;; Ensure spec directory exists
        (protagentic--ensure-spec-directory (protagentic-spec-root-path spec))
        
        ;; Generate and create requirements document
        (let* ((generation-mode (protagentic-config-prompt-for-generation-mode))
               (result (protagentic-generator-generate-requirements feature-description generation-mode)))
          
          (if (protagentic-generation-result-success result)
              (progn
                (protagentic--create-file-with-content 
                 (protagentic-spec-requirements-file spec)
                 (protagentic-generation-result-content result))
                
                ;; Show generation info
                (when (protagentic-generation-result-tokens-used result)
                  (message "Generated using %s mode. Tokens: %d, Cost: $%.4f" 
                           (protagentic-generation-result-generation-mode-used result)
                           (protagentic-generation-result-tokens-used result)
                           (or (protagentic-generation-result-cost-estimate result) 0))))
            
            (error "Requirements generation failed: %s" 
                   (or (protagentic-generation-result-error-message result) "Unknown error"))))
        
        ;; Open the requirements file if configured to do so
        (when protagentic-auto-open-generated-files
          (find-file (protagentic-spec-requirements-file spec)))
        
        ;; Provide user feedback
        (message "Created spec '%s' with initial requirements. Review and refine the requirements, then run M-x protagentic-generate-design" 
                 sanitized-name)
        
        ;; Return the spec for further processing if needed
        spec))))

(defun protagentic--prompt-for-feature-description (spec-name)
  "Prompt user for a detailed feature description for SPEC-NAME.
Returns the feature description string."
  (let ((description ""))
    (while (string-empty-p (string-trim description))
      (setq description 
            (read-string 
             (format "Describe the '%s' feature (what should it do?): " spec-name)
             nil nil "")))
    
    ;; Optionally prompt for additional context
    (let ((additional-context 
           (read-string "Any additional context or constraints (optional): " nil nil "")))
      (if (string-empty-p (string-trim additional-context))
          description
        (format "%s\n\nAdditional Context:\n%s" description additional-context)))))

(defun protagentic-commands-generate-design ()
  "Generate design document from existing requirements.
Validates prerequisites and creates design.md based on requirements content."
  (interactive)
  
  ;; Validate project context
  (protagentic--validate-project-context)
  
  ;; Find current spec or prompt user to select one
  (let ((spec (or (protagentic--find-current-spec)
                  (protagentic--prompt-for-spec "Generate design for which spec? "))))
    
    (unless spec
      (user-error "No spec found. Create a spec first with M-x protagentic-create-spec"))
    
    ;; Validate that requirements exist
    (protagentic--validate-phase-prerequisites 'design spec)
    
    ;; Read requirements content
    (let ((requirements-content (protagentic--read-file-content 
                                 (protagentic-spec-requirements-file spec))))
      (unless requirements-content
        (error "Could not read requirements file: %s" 
               (protagentic-spec-requirements-file spec)))
      
      ;; Generate design document
      (let* ((generation-mode (protagentic-config-prompt-for-generation-mode))
             (result (protagentic-generator-generate-design requirements-content generation-mode)))
        
        (if (protagentic-generation-result-success result)
            (progn
              (protagentic--create-file-with-content 
               (protagentic-spec-design-file spec)
               (protagentic-generation-result-content result))
              
              ;; Show generation info
              (when (protagentic-generation-result-tokens-used result)
                (message "Generated using %s mode. Tokens: %d, Cost: $%.4f" 
                         (protagentic-generation-result-generation-mode-used result)
                         (protagentic-generation-result-tokens-used result)
                         (or (protagentic-generation-result-cost-estimate result) 0))))
          
          (error "Design generation failed: %s" 
                 (or (protagentic-generation-result-error-message result) "Unknown error"))))
      
      ;; Open the design file if configured to do so
      (when protagentic-auto-open-generated-files
        (find-file (protagentic-spec-design-file spec)))
      
      ;; Provide user feedback
      (message "Generated design document for '%s'. Review and refine the design, then run M-x protagentic-generate-tasks" 
               (protagentic-spec-name spec)))))

(defun protagentic-commands-generate-tasks ()
  "Generate task list from existing design document.
Validates prerequisites and creates tasks.md based on design and requirements."
  (interactive)
  
  ;; Validate project context
  (protagentic--validate-project-context)
  
  ;; Find current spec or prompt user to select one
  (let ((spec (or (protagentic--find-current-spec)
                  (protagentic--prompt-for-spec "Generate tasks for which spec? "))))
    
    (unless spec
      (user-error "No spec found. Create a spec first with M-x protagentic-create-spec"))
    
    ;; Validate that design exists
    (protagentic--validate-phase-prerequisites 'tasks spec)
    
    ;; Read design and requirements content
    (let ((design-content (protagentic--read-file-content 
                           (protagentic-spec-design-file spec)))
          (requirements-content (protagentic--read-file-content 
                                 (protagentic-spec-requirements-file spec))))
      
      (unless design-content
        (error "Could not read design file: %s" 
               (protagentic-spec-design-file spec)))
      
      (unless requirements-content
        (error "Could not read requirements file: %s" 
               (protagentic-spec-requirements-file spec)))
      
      ;; Generate tasks document
      (let* ((generation-mode (protagentic-config-prompt-for-generation-mode))
             (result (protagentic-generator-generate-tasks design-content requirements-content generation-mode)))
        
        (if (protagentic-generation-result-success result)
            (progn
              (protagentic--create-file-with-content 
               (protagentic-spec-tasks-file spec)
               (protagentic-generation-result-content result))
              
              ;; Show generation info
              (when (protagentic-generation-result-tokens-used result)
                (message "Generated using %s mode. Tokens: %d, Cost: $%.4f" 
                         (protagentic-generation-result-generation-mode-used result)
                         (protagentic-generation-result-tokens-used result)
                         (or (protagentic-generation-result-cost-estimate result) 0))))
          
          (error "Tasks generation failed: %s" 
                 (or (protagentic-generation-result-error-message result) "Unknown error"))))
      
      ;; Open the tasks file if configured to do so
      (when protagentic-auto-open-generated-files
        (find-file (protagentic-spec-tasks-file spec)))
      
      ;; Provide user feedback
      (message "Generated implementation tasks for '%s'. The spec workflow is now complete!" 
               (protagentic-spec-name spec)))))

(defun protagentic--prompt-for-spec (prompt-message)
  "Prompt user to select a spec with PROMPT-MESSAGE.
Returns a protagentic-spec structure or nil if cancelled."
  (let ((available-specs (protagentic--get-project-specs)))
    (if available-specs
        (let ((selected-spec (completing-read prompt-message available-specs nil t)))
          (when selected-spec
            (protagentic--create-spec-struct selected-spec)))
      (progn
        (message "No specs found in current project")
        nil))))

(defun protagentic-commands-list-specs ()
  "List all specs in the current project with their status.
Displays a summary of each spec's completion status."
  (interactive)
  
  ;; Validate project context
  (protagentic--validate-project-context)
  
  (let ((specs (protagentic--get-project-specs)))
    (if specs
        (with-output-to-temp-buffer "*Protagentic Specs*"
          (princ "Protagentic Specs in Current Project\n")
          (princ "=====================================\n\n")
          
          (dolist (spec-name specs)
            (let* ((spec (protagentic--create-spec-struct spec-name))
                   (status (when spec (protagentic--get-spec-status spec))))
              (if status
                  (progn
                    (princ (format "Spec: %s\n" spec-name))
                    (princ (format "  Requirements: %s\n" 
                                   (if (plist-get status :requirements-complete) "✓" "✗")))
                    (princ (format "  Design: %s\n" 
                                   (if (plist-get status :design-complete) "✓" "✗")))
                    (princ (format "  Tasks: %s\n" 
                                   (if (plist-get status :tasks-complete) "✓" "✗")))
                    (princ (format "  Status: %s\n" 
                                   (if (plist-get status :workflow-complete) 
                                       "Complete" 
                                     (format "In Progress (%s phase)" 
                                             (or (plist-get status :current-phase) "Not started")))))
                    (princ "\n"))
                (princ (format "Spec: %s (Error reading status)\n\n" spec-name))))))
      (message "No specs found in current project"))))

(defun protagentic-commands-delete-spec (spec-name)
  "Delete a spec and all its associated files.
Prompts for confirmation before deletion."
  (interactive 
   (list (let ((specs (protagentic--get-project-specs)))
           (if specs
               (completing-read "Delete which spec? " specs nil t)
             (user-error "No specs found in current project")))))
  
  (let ((spec (protagentic--create-spec-struct spec-name)))
    (unless spec
      (user-error "Spec '%s' not found" spec-name))
    
    (when (yes-or-no-p (format "Really delete spec '%s' and all its files? " spec-name))
      (let ((spec-dir (protagentic-spec-root-path spec)))
        (when (file-directory-p spec-dir)
          (delete-directory spec-dir t)
          (message "Deleted spec '%s'" spec-name))))))

(defun protagentic-commands-refine-requirements ()
  "Interactively refine requirements for the current spec.
Provides guided prompts to improve and expand requirements."
  (interactive)
  
  (protagentic--validate-project-context)
  
  (let ((spec (or (protagentic--find-current-spec)
                  (protagentic--prompt-for-spec "Refine requirements for which spec? "))))
    
    (unless spec
      (user-error "No spec found. Create a spec first with M-x protagentic-create-spec"))
    
    (unless (protagentic--file-exists-p (protagentic-spec-requirements-file spec))
      (user-error "Requirements file not found. Create requirements first"))
    
    ;; Read current requirements
    (let ((current-content (protagentic--read-file-content 
                            (protagentic-spec-requirements-file spec))))
      
      ;; Analyze current requirements
      (let ((analysis (protagentic--analyze-requirements-quality current-content)))
        
        ;; Show analysis and get user input for improvements
        (protagentic--interactive-requirements-refinement spec current-content analysis)))))

(defun protagentic--analyze-requirements-quality (requirements-content)
  "Analyze the quality of REQUIREMENTS-CONTENT and identify improvement areas.
Returns a plist with analysis results and suggestions."
  (let ((parsed (protagentic--parse-requirements-content requirements-content))
        (suggestions '())
        (quality-score 0))
    
    ;; Check for user stories
    (let ((story-count (length (plist-get parsed :user-stories))))
      (if (> story-count 0)
          (setq quality-score (+ quality-score 25))
        (push "Add user stories in \"As a [role], I want [feature], so that [benefit]\" format" suggestions)))
    
    ;; Check for acceptance criteria
    (let ((criteria-count (length (plist-get parsed :acceptance-criteria))))
      (if (> criteria-count 0)
          (setq quality-score (+ quality-score 25))
        (push "Add acceptance criteria using WHEN/THEN or IF/THEN format" suggestions)))
    
    ;; Check for introduction
    (if (plist-get parsed :has-introduction)
        (setq quality-score (+ quality-score 15))
      (push "Add a clear introduction explaining the feature purpose" suggestions))
    
    ;; Check requirements count
    (let ((req-count (plist-get parsed :requirements-count)))
      (cond
       ((< req-count 2)
        (push "Consider adding more requirements to cover edge cases and different user scenarios" suggestions))
       ((> req-count 1)
        (setq quality-score (+ quality-score 20)))))
    
    ;; Check for specific patterns that indicate quality
    (when (string-match-p "SHALL\\|MUST\\|SHOULD" requirements-content)
      (setq quality-score (+ quality-score 15)))
    
    (list :quality-score quality-score
          :suggestions suggestions
          :user-story-count (length (plist-get parsed :user-stories))
          :criteria-count (length (plist-get parsed :acceptance-criteria))
          :requirements-count (plist-get parsed :requirements-count))))

(defun protagentic--interactive-requirements-refinement (spec current-content analysis)
  "Conduct interactive refinement session for SPEC requirements.
Uses CURRENT-CONTENT and ANALYSIS to guide the refinement process."
  (let ((quality-score (plist-get analysis :quality-score))
        (suggestions (plist-get analysis :suggestions)))
    
    ;; Display current analysis
    (with-output-to-temp-buffer "*Requirements Analysis*"
      (princ (format "Requirements Analysis for '%s'\n" (protagentic-spec-name spec)))
      (princ "=====================================\n\n")
      (princ (format "Quality Score: %d/100\n\n" quality-score))
      
      (princ "Current Status:\n")
      (princ (format "  User Stories: %d\n" (plist-get analysis :user-story-count)))
      (princ (format "  Acceptance Criteria: %d\n" (plist-get analysis :criteria-count)))
      (princ (format "  Requirements Sections: %d\n\n" (plist-get analysis :requirements-count)))
      
      (when suggestions
        (princ "Improvement Suggestions:\n")
        (dolist (suggestion suggestions)
          (princ (format "  • %s\n" suggestion)))
        (princ "\n"))
      
      (princ "Refinement Options:\n")
      (princ "  1. Add new requirement\n")
      (princ "  2. Improve existing requirements\n")
      (princ "  3. Add acceptance criteria\n")
      (princ "  4. Review and finalize\n"))
    
    ;; Interactive refinement loop
    (protagentic--requirements-refinement-loop spec current-content)))

(defun protagentic--requirements-refinement-loop (spec current-content)
  "Main refinement loop for SPEC requirements with CURRENT-CONTENT."
  (let ((continue t)
        (updated-content current-content))
    
    (while continue
      (let ((choice (read-char-choice 
                     "Choose action: (1) Add requirement (2) Improve existing (3) Add criteria (4) Finalize (q) Quit: "
                     '(?1 ?2 ?3 ?4 ?q))))
        
        (pcase choice
          (?1 (setq updated-content (protagentic--add-new-requirement updated-content)))
          (?2 (setq updated-content (protagentic--improve-existing-requirements updated-content)))
          (?3 (setq updated-content (protagentic--add-acceptance-criteria updated-content)))
          (?4 (protagentic--finalize-requirements spec updated-content)
              (setq continue nil))
          (?q (setq continue nil)
              (message "Requirements refinement cancelled")))))
    
    updated-content))

(defun protagentic--add-new-requirement (current-content)
  "Add a new requirement to CURRENT-CONTENT.
Prompts user for requirement details and returns updated content."
  (let* ((role (read-string "User role (e.g., 'developer', 'user'): "))
         (feature (read-string "What feature do they want? "))
         (benefit (read-string "What benefit/value does it provide? "))
         (criteria-count (string-to-number 
                          (read-string "How many acceptance criteria? (1-5): " "2")))
         (criteria '()))
    
    ;; Collect acceptance criteria
    (dotimes (i criteria-count)
      (let ((criterion (read-string (format "Acceptance criterion %d (WHEN/IF format): " (1+ i)))))
        (push criterion criteria)))
    
    ;; Find the next requirement number
    (let ((req-number (protagentic--get-next-requirement-number current-content)))
      
      ;; Build new requirement section
      (let ((new-requirement 
             (format "\n### Requirement %d\n\n**User Story:** As a %s, I want %s, so that %s\n\n#### Acceptance Criteria\n\n%s\n"
                     req-number role feature benefit
                     (mapconcat (lambda (criterion) 
                                  (format "%d. %s" 
                                          (1+ (cl-position criterion criteria))
                                          criterion))
                                (reverse criteria) "\n"))))
        
        ;; Insert before any trailing comments
        (if (string-match "<!-- Add more requirements" current-content)
            (replace-match (concat new-requirement "<!-- Add more requirements") nil nil current-content)
          (concat current-content new-requirement))))))

(defun protagentic--get-next-requirement-number (content)
  "Get the next requirement number from CONTENT."
  (let ((numbers '())
        (pos 0))
    (while (string-match "### Requirement \\([0-9]+\\)" content pos)
      (push (string-to-number (match-string 1 content)) numbers)
      (setq pos (match-end 0)))
    (if numbers
        (1+ (apply 'max numbers))
      1)))

(defun protagentic--improve-existing-requirements (current-content)
  "Improve existing requirements in CURRENT-CONTENT.
Returns updated content with improvements."
  (message "Opening requirements file for manual editing...")
  (let ((temp-file (make-temp-file "protagentic-requirements" nil ".md")))
    (with-temp-file temp-file
      (insert current-content))
    
    (find-file temp-file)
    (message "Edit the requirements, then save and close the buffer to continue")
    
    ;; Wait for user to finish editing
    (while (get-buffer (file-name-nondirectory temp-file))
      (sit-for 1))
    
    ;; Read the updated content
    (with-temp-buffer
      (insert-file-contents temp-file)
      (delete-file temp-file)
      (buffer-string))))

(defun protagentic--add-acceptance-criteria (current-content)
  "Add acceptance criteria to existing requirements in CURRENT-CONTENT."
  (let ((requirements (protagentic--extract-requirement-sections current-content)))
    (if requirements
        (let* ((req-names (mapcar 'car requirements))
               (selected (completing-read "Add criteria to which requirement? " req-names nil t))
               (new-criterion (read-string "New acceptance criterion (WHEN/IF format): ")))
          
          ;; Find and update the selected requirement
          (protagentic--insert-criterion-into-requirement current-content selected new-criterion))
      (progn
        (message "No existing requirements found to add criteria to")
        current-content))))

(defun protagentic--extract-requirement-sections (content)
  "Extract requirement sections from CONTENT.
Returns list of (name . content) pairs."
  (let ((sections '())
        (pos 0))
    (while (string-match "### \\(Requirement [0-9]+\\)" content pos)
      (let ((req-name (match-string 1 content))
            (start (match-beginning 0)))
        (setq pos (match-end 0))
        ;; Find next requirement or end of content
        (let ((end (if (string-match "### Requirement [0-9]+" content pos)
                       (match-beginning 0)
                     (length content))))
          (push (cons req-name (substring content start end)) sections))))
    (reverse sections)))

(defun protagentic--insert-criterion-into-requirement (content req-name new-criterion)
  "Insert NEW-CRITERION into REQ-NAME section of CONTENT."
  (let ((req-pattern (format "\\(### %s.*?#### Acceptance Criteria.*?\\)\\(\\(?:[0-9]+\\. .*\n\\)*\\)" 
                             (regexp-quote req-name))))
    (if (string-match req-pattern content)
        (let* ((before (match-string 1 content))
               (existing-criteria (match-string 2 content))
               (after (substring content (match-end 0)))
               (next-num (1+ (length (split-string existing-criteria "\n" t))))
               (new-criteria-line (format "%d. %s\n" next-num new-criterion)))
          (concat before existing-criteria new-criteria-line after))
      content)))

(defun protagentic--finalize-requirements (spec updated-content)
  "Finalize requirements for SPEC with UPDATED-CONTENT.
Saves the updated content and provides completion feedback."
  (protagentic--create-file-with-content 
   (protagentic-spec-requirements-file spec)
   updated-content)
  
  (message "Requirements updated for spec '%s'. Ready to generate design with M-x protagentic-generate-design" 
           (protagentic-spec-name spec))
  
  (when protagentic-auto-open-generated-files
    (find-file (protagentic-spec-requirements-file spec))))

(defun protagentic-commands-refine-design ()
  "Interactively refine design document for the current spec.
Provides guided prompts to improve and expand the design."
  (interactive)
  
  (protagentic--validate-project-context)
  
  (let ((spec (or (protagentic--find-current-spec)
                  (protagentic--prompt-for-spec "Refine design for which spec? "))))
    
    (unless spec
      (user-error "No spec found. Create a spec first with M-x protagentic-create-spec"))
    
    (unless (protagentic--file-exists-p (protagentic-spec-design-file spec))
      (user-error "Design file not found. Generate design first with M-x protagentic-generate-design"))
    
    ;; Read current design
    (let ((current-content (protagentic--read-file-content 
                            (protagentic-spec-design-file spec))))
      
      ;; Analyze current design
      (let ((analysis (protagentic--analyze-design-quality current-content)))
        
        ;; Show analysis and get user input for improvements
        (protagentic--interactive-design-refinement spec current-content analysis)))))

(defun protagentic--analyze-design-quality (design-content)
  "Analyze the quality of DESIGN-CONTENT and identify improvement areas.
Returns a plist with analysis results and suggestions."
  (let ((parsed (protagentic--parse-design-content design-content))
        (suggestions '())
        (quality-score 0))
    
    ;; Check for required sections
    (let ((sections (plist-get parsed :sections)))
      (dolist (required-section '("Overview" "Architecture" "Components" "Data Models" "Error Handling" "Testing Strategy"))
        (if (member required-section sections)
            (setq quality-score (+ quality-score 15))
          (push (format "Add %s section with detailed information" required-section) suggestions))))
    
    ;; Check for components
    (let ((component-count (length (plist-get parsed :components))))
      (if (> component-count 0)
          (setq quality-score (+ quality-score 10))
        (push "Define system components and their responsibilities" suggestions)))
    
    ;; Check for data models
    (let ((model-count (length (plist-get parsed :data-models))))
      (if (> model-count 0)
          (setq quality-score (+ quality-score 10))
        (push "Define data models and their relationships" suggestions)))
    
    ;; Check for technology decisions
    (if (plist-get parsed :technologies)
        (setq quality-score (+ quality-score 5))
      (push "Specify technology stack and rationale" suggestions))
    
    (list :quality-score quality-score
          :suggestions suggestions
          :component-count (length (plist-get parsed :components))
          :model-count (length (plist-get parsed :data-models))
          :section-count (length (plist-get parsed :sections)))))

(defun protagentic--interactive-design-refinement (spec current-content analysis)
  "Conduct interactive refinement session for SPEC design.
Uses CURRENT-CONTENT and ANALYSIS to guide the refinement process."
  (let ((quality-score (plist-get analysis :quality-score))
        (suggestions (plist-get analysis :suggestions)))
    
    ;; Display current analysis
    (with-output-to-temp-buffer "*Design Analysis*"
      (princ (format "Design Analysis for '%s'\n" (protagentic-spec-name spec)))
      (princ "==================================\n\n")
      (princ (format "Quality Score: %d/100\n\n" quality-score))
      
      (princ "Current Status:\n")
      (princ (format "  Components Defined: %d\n" (plist-get analysis :component-count)))
      (princ (format "  Data Models: %d\n" (plist-get analysis :model-count)))
      (princ (format "  Design Sections: %d/6\n\n" (plist-get analysis :section-count)))
      
      (when suggestions
        (princ "Improvement Suggestions:\n")
        (dolist (suggestion suggestions)
          (princ (format "  • %s\n" suggestion)))
        (princ "\n"))
      
      (princ "Refinement Options:\n")
      (princ "  1. Add/modify components\n")
      (princ "  2. Define data models\n")
      (princ "  3. Specify technology stack\n")
      (princ "  4. Add error handling details\n")
      (princ "  5. Enhance testing strategy\n")
      (princ "  6. Manual editing\n")
      (princ "  7. Review and finalize\n"))
    
    ;; Interactive refinement loop
    (protagentic--design-refinement-loop spec current-content)))

(defun protagentic--design-refinement-loop (spec current-content)
  "Main refinement loop for SPEC design with CURRENT-CONTENT."
  (let ((continue t)
        (updated-content current-content))
    
    (while continue
      (let ((choice (read-char-choice 
                     "Choose: (1) Components (2) Data models (3) Tech stack (4) Errors (5) Testing (6) Manual edit (7) Finalize (q) Quit: "
                     '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?q))))
        
        (pcase choice
          (?1 (setq updated-content (protagentic--add-design-component updated-content)))
          (?2 (setq updated-content (protagentic--add-data-model updated-content)))
          (?3 (setq updated-content (protagentic--enhance-technology-section updated-content)))
          (?4 (setq updated-content (protagentic--enhance-error-handling updated-content)))
          (?5 (setq updated-content (protagentic--enhance-testing-strategy updated-content)))
          (?6 (setq updated-content (protagentic--manual-design-editing updated-content)))
          (?7 (protagentic--finalize-design spec updated-content)
              (setq continue nil))
          (?q (setq continue nil)
              (message "Design refinement cancelled")))))
    
    updated-content))

(defun protagentic--add-design-component (current-content)
  "Add a new component to the design CURRENT-CONTENT."
  (let* ((component-name (read-string "Component name: "))
         (component-purpose (read-string "Component purpose: "))
         (responsibilities (read-string "Key responsibilities (comma-separated): "))
         (interfaces (read-string "Main interfaces (comma-separated): ")))
    
    (let ((component-section 
           (format "\n#### %s\n\n%s\n\n**Key Responsibilities:**\n%s\n\n**Interfaces:**\n%s\n"
                   component-name
                   component-purpose
                   (mapconcat (lambda (resp) (format "- %s" (string-trim resp)))
                              (split-string responsibilities ",") "\n")
                   (mapconcat (lambda (iface) (format "- %s" (string-trim iface)))
                              (split-string interfaces ",") "\n"))))
      
      ;; Insert into Components section
      (if (string-match "## Components and Interfaces" current-content)
          (let ((insert-pos (string-match "\n## " current-content (match-end 0))))
            (if insert-pos
                (concat (substring current-content 0 insert-pos)
                        component-section
                        (substring current-content insert-pos))
              (concat current-content component-section)))
        (concat current-content "\n## Components and Interfaces\n" component-section)))))

(defun protagentic--add-data-model (current-content)
  "Add a new data model to the design CURRENT-CONTENT."
  (let* ((model-name (read-string "Data model name: "))
         (model-purpose (read-string "Model purpose: "))
         (fields (read-string "Key fields (comma-separated): ")))
    
    (let ((model-section 
           (format "\n#### %s Model\n\n```\n%s {\n%s\n  created_at: timestamp\n  updated_at: timestamp\n}\n```\n\n%s\n"
                   model-name
                   model-name
                   (mapconcat (lambda (field) (format "  %s: type" (string-trim field)))
                              (split-string fields ",") "\n")
                   model-purpose)))
      
      ;; Insert into Data Models section
      (if (string-match "## Data Models" current-content)
          (let ((insert-pos (string-match "\n## " current-content (match-end 0))))
            (if insert-pos
                (concat (substring current-content 0 insert-pos)
                        model-section
                        (substring current-content insert-pos))
              (concat current-content model-section)))
        (concat current-content "\n## Data Models\n" model-section)))))

;; LLM Configuration Commands

(defun protagentic-commands-setup-llm ()
  "Setup LLM integration for Protagentic.
Runs the interactive configuration wizard."
  (interactive)
  (protagentic-config-setup))

(defun protagentic-commands-show-config ()
  "Show current Protagentic configuration and status."
  (interactive)
  (protagentic-config-show-status))

(defun protagentic-commands-set-generation-mode ()
  "Set the default content generation mode."
  (interactive)
  (call-interactively 'protagentic-config-set-generation-mode))

(defun protagentic-commands-validate-api-key ()
  "Validate the configured OpenAI API key."
  (interactive)
  (protagentic-config-validate-api-key))

(defun protagentic-commands-reset-usage-stats ()
  "Reset LLM usage statistics."
  (interactive)
  (protagentic-config-reset-usage-stats))

;; Enhanced Generation Commands

(defun protagentic-commands-regenerate-requirements ()
  "Regenerate requirements for the current spec with mode selection.
Allows switching between template and LLM generation."
  (interactive)
  (protagentic--validate-project-context)
  
  (let ((spec (or (protagentic--find-current-spec)
                  (protagentic--prompt-for-spec "Regenerate requirements for which spec? "))))
    
    (unless spec
      (user-error "No spec found. Create a spec first with M-x protagentic-create-spec"))
    
    (unless (protagentic--file-exists-p (protagentic-spec-requirements-file spec))
      (user-error "Requirements file not found. Create requirements first"))
    
    ;; Get original feature description or prompt for new one
    (let ((feature-description (read-string "Feature description: " 
                                           (protagentic--extract-feature-description-from-requirements spec))))
      
      (when (y-or-n-p "This will overwrite existing requirements. Continue? ")
        (let* ((generation-mode (protagentic-config-prompt-for-generation-mode))
               (result (protagentic-generator-generate-requirements feature-description generation-mode)))
          
          (if (protagentic-generation-result-success result)
              (progn
                (protagentic--create-file-with-content 
                 (protagentic-spec-requirements-file spec)
                 (protagentic-generation-result-content result))
                
                (message "Requirements regenerated for '%s' using %s mode" 
                         (protagentic-spec-name spec)
                         (protagentic-generation-result-generation-mode-used result))
                
                (when protagentic-auto-open-generated-files
                  (find-file (protagentic-spec-requirements-file spec))))
            
            (error "Requirements regeneration failed: %s" 
                   (or (protagentic-generation-result-error-message result) "Unknown error"))))))))

(defun protagentic-commands-regenerate-design ()
  "Regenerate design document for the current spec with mode selection."
  (interactive)
  (protagentic--validate-project-context)
  
  (let ((spec (or (protagentic--find-current-spec)
                  (protagentic--prompt-for-spec "Regenerate design for which spec? "))))
    
    (unless spec
      (user-error "No spec found. Create a spec first with M-x protagentic-create-spec"))
    
    (protagentic--validate-phase-prerequisites 'design spec)
    
    (when (y-or-n-p "This will overwrite existing design. Continue? ")
      (let* ((requirements-content (protagentic--read-file-content 
                                   (protagentic-spec-requirements-file spec)))
             (generation-mode (protagentic-config-prompt-for-generation-mode))
             (result (protagentic-generator-generate-design requirements-content generation-mode)))
        
        (if (protagentic-generation-result-success result)
            (progn
              (protagentic--create-file-with-content 
               (protagentic-spec-design-file spec)
               (protagentic-generation-result-content result))
              
              (message "Design regenerated for '%s' using %s mode" 
                       (protagentic-spec-name spec)
                       (protagentic-generation-result-generation-mode-used result))
              
              (when protagentic-auto-open-generated-files
                (find-file (protagentic-spec-design-file spec))))
          
          (error "Design regeneration failed: %s" 
                 (or (protagentic-generation-result-error-message result) "Unknown error")))))))

(defun protagentic-commands-regenerate-tasks ()
  "Regenerate tasks document for the current spec with mode selection."
  (interactive)
  (protagentic--validate-project-context)
  
  (let ((spec (or (protagentic--find-current-spec)
                  (protagentic--prompt-for-spec "Regenerate tasks for which spec? "))))
    
    (unless spec
      (user-error "No spec found. Create a spec first with M-x protagentic-create-spec"))
    
    (protagentic--validate-phase-prerequisites 'tasks spec)
    
    (when (y-or-n-p "This will overwrite existing tasks. Continue? ")
      (let* ((design-content (protagentic--read-file-content 
                             (protagentic-spec-design-file spec)))
             (requirements-content (protagentic--read-file-content 
                                   (protagentic-spec-requirements-file spec)))
             (generation-mode (protagentic-config-prompt-for-generation-mode))
             (result (protagentic-generator-generate-tasks design-content requirements-content generation-mode)))
        
        (if (protagentic-generation-result-success result)
            (progn
              (protagentic--create-file-with-content 
               (protagentic-spec-tasks-file spec)
               (protagentic-generation-result-content result))
              
              (message "Tasks regenerated for '%s' using %s mode" 
                       (protagentic-spec-name spec)
                       (protagentic-generation-result-generation-mode-used result))
              
              (when protagentic-auto-open-generated-files
                (find-file (protagentic-spec-tasks-file spec))))
          
          (error "Tasks regeneration failed: %s" 
                 (or (protagentic-generation-result-error-message result) "Unknown error")))))))

;; Helper functions for enhanced commands

(defun protagentic--extract-feature-description-from-requirements (spec)
  "Extract feature description from existing requirements file for SPEC.
Returns a reasonable default description based on spec name if extraction fails."
  (condition-case nil
      (let ((content (protagentic--read-file-content (protagentic-spec-requirements-file spec))))
        (when content
          ;; Try to extract from introduction section
          (if (string-match "## Introduction\n\n\\([^#]+\\)" content)
              (string-trim (match-string 1 content))
            ;; Fallback to spec name
            (format "%s feature" (replace-regexp-in-string "-" " " (protagentic-spec-name spec))))))
    (error (format "%s feature" (replace-regexp-in-string "-" " " (protagentic-spec-name spec))))))

(defun protagentic--enhance-technology-section (current-content)
  "Enhance the technology stack section in CURRENT-CONTENT."
  (let* ((frontend-tech (read-string "Frontend technology: "))
         (backend-tech (read-string "Backend technology: "))
         (database-tech (read-string "Database technology: ")))
    (replace-regexp-in-string
     "- \\*\\*Frontend:\\*\\* \\[Technology choices and rationale\\]"
     (format "- **Frontend:** %s" frontend-tech)
     (replace-regexp-in-string
      "- \\*\\*Backend:\\*\\* \\[Technology choices and rationale\\]"
      (format "- **Backend:** %s" backend-tech)
      (replace-regexp-in-string
       "- \\*\\*Database:\\*\\* \\[Data storage approach and rationale\\]"
       (format "- **Database:** %s" database-tech)
       current-content)))))

(defun protagentic--enhance-error-handling (current-content)
  "Enhance the error handling section in CURRENT-CONTENT."
  (message "Opening design file for manual error handling enhancement...")
  (protagentic--manual-design-editing current-content))

(defun protagentic--enhance-testing-strategy (current-content)
  "Enhance the testing strategy section in CURRENT-CONTENT."
  (message "Opening design file for manual testing strategy enhancement...")
  (protagentic--manual-design-editing current-content))

(defun protagentic--manual-design-editing (current-content)
  "Open CURRENT-CONTENT for manual editing and return updated content."
  (let ((temp-file (make-temp-file "protagentic-design" nil ".md")))
    (with-temp-file temp-file
      (insert current-content))
    
    (find-file temp-file)
    (message "Edit the design document, then save and close the buffer to continue")
    
    ;; Wait for user to finish editing
    (while (get-buffer (file-name-nondirectory temp-file))
      (sit-for 1))
    
    ;; Read the updated content
    (with-temp-buffer
      (insert-file-contents temp-file)
      (delete-file temp-file)
      (buffer-string))))

(defun protagentic--finalize-design (spec updated-content)
  "Finalize design for SPEC with UPDATED-CONTENT.
Saves the updated content and provides completion feedback."
  (protagentic--create-file-with-content 
   (protagentic-spec-design-file spec)
   updated-content)
  
  (message "Design updated for spec '%s'. Ready to generate tasks with M-x protagentic-generate-tasks" 
           (protagentic-spec-name spec))
  
  (when protagentic-auto-open-generated-files
    (find-file (protagentic-spec-design-file spec))))

(provide 'protagentic-commands)

;;; protagentic-commands.el ends here

;;;###autoload
(defun protagentic-execute-next-task (spec-name)
  "Execute the next pending task for SPEC-NAME.
Generates code files based on the task requirements and design."
  (interactive (list (protagentic--read-spec-name "Execute task for spec: ")))
  
  (let ((spec (protagentic--create-spec-struct spec-name)))
    (unless spec
      (error "Spec '%s' not found" spec-name))
    
    ;; Validate that tasks exist
    (unless (protagentic--file-exists-p (protagentic-spec-tasks-file spec))
      (error "No tasks file found. Generate tasks first with M-x protagentic-generate-tasks"))
    
    (require 'protagentic-executor)
    (let ((result (protagentic-executor-execute-next-task spec)))
      (if result
          (message "✓ Task completed successfully. Generated files: %s"
                   (string-join (protagentic-task-generated-files result) ", "))
        (message "All tasks completed or execution failed")))))

;;;###autoload
(defun protagentic-execute-all-tasks (spec-name)
  "Execute all pending tasks for SPEC-NAME.
Generates complete project implementation based on tasks."
  (interactive (list (protagentic--read-spec-name "Execute all tasks for spec: ")))
  
  (let ((spec (protagentic--create-spec-struct spec-name)))
    (unless spec
      (error "Spec '%s' not found" spec-name))
    
    ;; Validate that tasks exist
    (unless (protagentic--file-exists-p (protagentic-spec-tasks-file spec))
      (error "No tasks file found. Generate tasks first with M-x protagentic-generate-tasks"))
    
    (require 'protagentic-executor)
    (let ((completed-count 0)
          (failed-count 0)
          (max-iterations 20)) ; Prevent infinite loops
      
      (message "Starting execution of all tasks...")
      
      (cl-block execution-loop
        (dotimes (i max-iterations)
          (let ((result (condition-case err
                            (protagentic-executor-execute-next-task spec)
                          (error 
                           (message "✗ Task execution error: %s" (error-message-string err))
                           nil))))
            (cond
             (result
              (cl-incf completed-count)
              (message "Progress: %d tasks completed" completed-count))
             
             ((protagentic-executor--has-pending-tasks-p spec)
              (cl-incf failed-count)
              (message "⚠ Task failed, continuing with next task"))
             
             (t
              (message "✓ All tasks completed! Generated files for %d tasks" completed-count)
              (cl-return-from execution-loop))))))
      
      (when (> failed-count 0)
        (message "⚠ Completed %d tasks, %d failed. Check logs for details" 
                 completed-count failed-count))))

;;;###autoload
(defun protagentic-validate-code-quality (spec-name)
  "Validate code quality for generated files in SPEC-NAME."
  (interactive (list (protagentic--read-spec-name "Validate code quality for spec: ")))
  
  (let ((spec (protagentic--create-spec-struct spec-name)))
    (unless spec
      (error "Spec '%s' not found" spec-name))
    
    (require 'protagentic-quality)
    (let* ((project-root (protagentic--detect-project-root))
           (generated-files (protagentic-executor--get-generated-files spec))
           (total-errors 0)
           (total-warnings 0))
      
      (if generated-files
          (progn
            (with-output-to-temp-buffer "*Protagentic Quality Report*"
              (princ (format "Code Quality Report for '%s'\n" spec-name))
              (princ "=====================================\n\n")
              
              (dolist (file generated-files)
                (let ((file-path (expand-file-name file project-root)))
                  (when (file-exists-p file-path)
                    (let ((result (protagentic-quality-check-file file-path)))
                      (when result
                        (let ((errors (plist-get result :errors))
                              (warnings (plist-get result :warnings))
                              (score (plist-get result :score)))
                          
                          (princ (format "File: %s (Score: %d/100)\n" file score))
                          (princ "----------------------------------------\n")
                          
                          (when errors
                            (princ "ERRORS:\n")
                            (dolist (error errors)
                              (princ (format "  ✗ %s\n" error)))
                            (princ "\n"))
                          
                          (when warnings
                            (princ "WARNINGS:\n")
                            (dolist (warning warnings)
                              (princ (format "  ⚠ %s\n" warning)))
                            (princ "\n"))
                          
                          (setq total-errors (+ total-errors (length errors)))
                          (setq total-warnings (+ total-warnings (length warnings)))
                          
                          (princ "\n")))))))
              
              (princ (format "SUMMARY: %d errors, %d warnings across %d files\n"
                             total-errors total-warnings (length generated-files)))
              
              (if (= total-errors 0)
                  (princ "✓ No critical issues found!")
                (princ "✗ Critical issues need to be addressed before deployment")))
            
            (message "Quality validation complete. See *Protagentic Quality Report* for details"))
        
        (message "No generated files found for spec '%s'" spec-name))))

;;;###autoload
(defun protagentic-show-task-status (spec-name)
  "Show task execution status for SPEC-NAME."
  (interactive (list (protagentic--read-spec-name "Show task status for spec: ")))
  
  (let ((spec (protagentic--create-spec-struct spec-name)))
    (unless spec
      (error "Spec '%s' not found" spec-name))
    
    (unless (protagentic--file-exists-p (protagentic-spec-tasks-file spec))
      (error "No tasks file found for spec '%s'" spec-name))
    
    (require 'protagentic-executor)
    (let ((tasks (protagentic-executor--get-tasks spec)))
      (with-output-to-temp-buffer "*Protagentic Task Status*"
        (princ (format "Task Status for '%s'\n" spec-name))
        (princ "========================\n\n")
        
        (let ((pending 0) (completed 0) (failed 0))
          (dolist (task tasks)
            (let ((status (protagentic-task-status task))
                  (id (protagentic-task-id task))
                  (desc (protagentic-task-description task)))
              
              (pcase status
                ('pending 
                 (cl-incf pending)
                 (princ (format "⏳ Task %d: %s\n" id desc)))
                ('completed 
                 (cl-incf completed)
                 (princ (format "✓ Task %d: %s\n" id desc))
                 (let ((files (protagentic-task-generated-files task)))
                   (when files
                     (princ (format "   Generated: %s\n" (string-join files ", "))))))
                ('failed 
                 (cl-incf failed)
                 (princ (format "✗ Task %d: %s\n" id desc))))))
          
          (princ (format "\nSUMMARY: %d completed, %d pending, %d failed\n"
                         completed pending failed))
          
          (when (> pending 0)
            (princ "\nNext: Run M-x protagentic-execute-next-task to continue\n")))))))))

(provide 'protagentic-commands)

;;; protagentic-commands.el ends here