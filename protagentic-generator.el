;;; protagentic-generator.el --- Content generation orchestrator -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Content generation orchestrator for Protagentic that coordinates between
;; LLM-powered and template-based generation with intelligent fallback.

;;; Code:

(require 'cl-lib)
(require 'protagentic-llm)
(require 'protagentic-templates)
(require 'protagentic-config)
(require 'protagentic-prompts)

(defgroup protagentic-generator nil
  "Content generation settings for Protagentic."
  :group 'protagentic
  :prefix "protagentic-generator-")

(defcustom protagentic-generator-fallback-timeout 30
  "Timeout in seconds before falling back from LLM to template generation."
  :type 'integer
  :group 'protagentic-generator)

(defcustom protagentic-generator-retry-attempts 2
  "Number of retry attempts for LLM generation before fallback."
  :type 'integer
  :group 'protagentic-generator)

;; Data structures

(cl-defstruct protagentic-generation-context
  "Context information for content generation."
  feature-description
  requirements-content
  design-content
  project-files
  generation-mode
  user-preferences
  spec-name)

(cl-defstruct protagentic-generation-result
  "Result of content generation operation."
  content
  generation-mode-used
  success
  error-message
  tokens-used
  cost-estimate
  fallback-used
  generation-time)

;; Generation orchestration

(defun protagentic-generator-generate-requirements (feature-description &optional mode)
  "Generate requirements document for FEATURE-DESCRIPTION using specified MODE.
MODE can be 'llm, 'template, or 'hybrid. If nil, uses configured default.
Returns a protagentic-generation-result structure."
  (let* ((generation-mode (or mode (protagentic-config-get-generation-mode)))
         (context (make-protagentic-generation-context
                   :feature-description feature-description
                   :generation-mode generation-mode
                   :spec-name (protagentic-generator--extract-spec-name-from-description feature-description)))
         (start-time (current-time)))
    
    (message "Generating requirements using %s mode..." generation-mode)
    
    (let ((result (protagentic-generator--execute-generation 
                   context 'requirements #'protagentic-generator--generate-requirements-content)))
      
      (setf (protagentic-generation-result-generation-time result)
            (float-time (time-subtract (current-time) start-time)))
      
      (protagentic-generator--log-generation-result result 'requirements)
      result)))

(defun protagentic-generator-generate-design (requirements-content &optional mode)
  "Generate design document based on REQUIREMENTS-CONTENT using specified MODE.
Returns a protagentic-generation-result structure."
  (let* ((generation-mode (or mode (protagentic-config-get-generation-mode)))
         (context (make-protagentic-generation-context
                   :requirements-content requirements-content
                   :generation-mode generation-mode))
         (start-time (current-time)))
    
    (message "Generating design using %s mode..." generation-mode)
    
    (let ((result (protagentic-generator--execute-generation 
                   context 'design #'protagentic-generator--generate-design-content)))
      
      (setf (protagentic-generation-result-generation-time result)
            (float-time (time-subtract (current-time) start-time)))
      
      (protagentic-generator--log-generation-result result 'design)
      result)))

(defun protagentic-generator-generate-tasks (design-content requirements-content &optional mode)
  "Generate tasks document based on DESIGN-CONTENT and REQUIREMENTS-CONTENT using MODE.
Returns a protagentic-generation-result structure."
  (let* ((generation-mode (or mode (protagentic-config-get-generation-mode)))
         (context (make-protagentic-generation-context
                   :design-content design-content
                   :requirements-content requirements-content
                   :generation-mode generation-mode))
         (start-time (current-time)))
    
    (message "Generating tasks using %s mode..." generation-mode)
    
    (let ((result (protagentic-generator--execute-generation 
                   context 'tasks #'protagentic-generator--generate-tasks-content)))
      
      (setf (protagentic-generation-result-generation-time result)
            (float-time (time-subtract (current-time) start-time)))
      
      (protagentic-generator--log-generation-result result 'tasks)
      result)))

;; Core generation execution

(defun protagentic-generator--execute-generation (context content-type generator-func)
  "Execute generation using CONTEXT for CONTENT-TYPE with GENERATOR-FUNC.
Handles mode selection, fallback logic, and error recovery.
Returns a protagentic-generation-result structure."
  (let ((mode (protagentic-generation-context-generation-mode context)))
    (pcase mode
      ('template
       (protagentic-generator--generate-with-template context content-type generator-func))
      
      ('llm
       (protagentic-generator--generate-with-llm context content-type generator-func))
      
      ('hybrid
       (protagentic-generator--generate-with-hybrid context content-type generator-func))
      
      (t
       (make-protagentic-generation-result
        :content nil
        :generation-mode-used mode
        :success nil
        :error-message (format "Invalid generation mode: %s" mode)
        :fallback-used nil)))))

(defun protagentic-generator--generate-with-template (context content-type generator-func)
  "Generate content using template-based approach.
CONTEXT provides generation context, CONTENT-TYPE specifies the type,
GENERATOR-FUNC is the template generation function."
  (condition-case err
      (let ((content (funcall generator-func context 'template)))
        (make-protagentic-generation-result
         :content content
         :generation-mode-used 'template
         :success t
         :error-message nil
         :tokens-used 0
         :cost-estimate 0.0
         :fallback-used nil))
    (error
     (make-protagentic-generation-result
      :content nil
      :generation-mode-used 'template
      :success nil
      :error-message (format "Template generation failed: %s" (error-message-string err))
      :fallback-used nil))))

(defun protagentic-generator--generate-with-llm (context content-type generator-func)
  "Generate content using LLM approach with retry logic.
Falls back to template generation on failure."
  (if (not (protagentic-llm-available-p))
      ;; LLM not available, return fallback result immediately
      (progn
        (message "LLM not available, falling back to template generation")
        (let ((fallback-result (protagentic-generator--generate-with-template 
                               context content-type generator-func)))
          (setf (protagentic-generation-result-fallback-used fallback-result) t)
          fallback-result))
    
    ;; LLM is available, attempt generation with retries
    (let ((attempts 0)
          (max-attempts protagentic-generator-retry-attempts)
          (result nil))
      
      ;; Attempt LLM generation with retries
      (while (and (< attempts max-attempts) (not result))
        (cl-incf attempts)
        (message "LLM generation attempt %d/%d..." attempts max-attempts)
        
        (condition-case err
            (let* ((llm-content (funcall generator-func context 'llm))
                   (tokens-used (protagentic-generator--estimate-tokens llm-content))
                   (cost (protagentic-llm-estimate-cost llm-content)))
              
              ;; Track usage
              (protagentic-config-track-usage tokens-used cost)
              
              (setq result (make-protagentic-generation-result
                            :content llm-content
                            :generation-mode-used 'llm
                            :success t
                            :error-message nil
                            :tokens-used tokens-used
                            :cost-estimate cost
                            :fallback-used nil)))
          
          (error
           (message "LLM generation attempt %d failed: %s" attempts (error-message-string err))
           (when (>= attempts max-attempts)
             (message "All LLM attempts failed, falling back to template generation")
             (let ((fallback-result (protagentic-generator--generate-with-template 
                                    context content-type generator-func)))
               (setf (protagentic-generation-result-fallback-used fallback-result) t)
               (setf (protagentic-generation-result-error-message fallback-result)
                     (format "LLM failed after %d attempts: %s. Used template fallback." 
                             max-attempts (error-message-string err)))
               (setq result fallback-result))))))
      
      (or result
          (make-protagentic-generation-result
           :content nil
           :generation-mode-used 'llm
           :success nil
           :error-message "LLM generation failed and no fallback available"
           :fallback-used nil)))))

(defun protagentic-generator--generate-with-hybrid (context content-type generator-func)
  "Generate content using hybrid approach (LLM with template fallback).
Always attempts LLM first, falls back to template on any failure."
  (let ((llm-result (protagentic-generator--generate-with-llm context content-type generator-func)))
    (if (protagentic-generation-result-success llm-result)
        llm-result
      ;; LLM failed, use template fallback
      (message "Hybrid mode: LLM failed, using template fallback")
      (let ((template-result (protagentic-generator--generate-with-template 
                             context content-type generator-func)))
        (setf (protagentic-generation-result-fallback-used template-result) t)
        (setf (protagentic-generation-result-generation-mode-used template-result) 'hybrid)
        template-result))))

;; Content generation functions

(defun protagentic-generator--generate-requirements-content (context mode)
  "Generate requirements content using CONTEXT and MODE.
Returns the generated content as a string."
  (let ((feature-description (protagentic-generation-context-feature-description context)))
    (pcase mode
      ('template
       (protagentic--generate-requirements-template feature-description))
      
      ('llm
       (let ((prompt (protagentic-prompts-generate-requirements-prompt context)))
         (protagentic-llm-generate-content prompt 'requirements context)))
      
      (t (error "Invalid generation mode for requirements: %s" mode)))))

(defun protagentic-generator--generate-design-content (context mode)
  "Generate design content using CONTEXT and MODE.
Returns the generated content as a string."
  (let ((requirements-content (protagentic-generation-context-requirements-content context)))
    (pcase mode
      ('template
       (protagentic--generate-design-template requirements-content))
      
      ('llm
       (let ((prompt (protagentic-prompts-generate-design-prompt context)))
         (protagentic-llm-generate-content prompt 'design context)))
      
      (t (error "Invalid generation mode for design: %s" mode)))))

(defun protagentic-generator--generate-tasks-content (context mode)
  "Generate tasks content using CONTEXT and MODE.
Returns the generated content as a string."
  (let ((design-content (protagentic-generation-context-design-content context))
        (requirements-content (protagentic-generation-context-requirements-content context)))
    (pcase mode
      ('template
       (protagentic--generate-tasks-template design-content))
      
      ('llm
       (let ((prompt (protagentic-prompts-generate-tasks-prompt context)))
         (protagentic-llm-generate-content prompt 'tasks context)))
      
      (t (error "Invalid generation mode for tasks: %s" mode)))))



;; Utility functions

(defun protagentic-generator--estimate-tokens (content)
  "Estimate token count for CONTENT.
Returns approximate number of tokens."
  (if content
      (/ (length content) 4) ; Rough estimation: 4 characters per token
    0))

(defun protagentic-generator--extract-spec-name-from-description (description)
  "Extract a potential spec name from DESCRIPTION.
Returns a sanitized spec name suggestion."
  (let ((words (split-string (downcase description) "[ \t\n]+" t)))
    (string-join (cl-subseq words 0 (min 3 (length words))) "-")))

(defun protagentic-generator--log-generation-result (result content-type)
  "Log generation RESULT for CONTENT-TYPE.
Provides user feedback about the generation process."
  (let ((mode (protagentic-generation-result-generation-mode-used result))
        (success (protagentic-generation-result-success result))
        (fallback (protagentic-generation-result-fallback-used result))
        (time (protagentic-generation-result-generation-time result))
        (tokens (protagentic-generation-result-tokens-used result))
        (cost (protagentic-generation-result-cost-estimate result)))
    
    (if success
        (progn
          (message "✓ %s generation completed using %s mode%s (%.1fs)"
                   (capitalize (symbol-name content-type))
                   mode
                   (if fallback " with fallback" "")
                   (or time 0))
          (when (and tokens cost (> tokens 0))
            (message "  Usage: %d tokens, estimated cost: $%.4f" tokens cost)))
      (message "✗ %s generation failed: %s"
               (capitalize (symbol-name content-type))
               (or (protagentic-generation-result-error-message result) "Unknown error")))))

;; Mode selection helpers

(defun protagentic-generator-select-mode-interactively (operation)
  "Interactively select generation mode for OPERATION.
Returns the selected mode symbol."
  (let ((default-mode (protagentic-config-get-generation-mode)))
    (if (protagentic-config-prompt-for-mode)
        (intern (completing-read 
                 (format "Generation mode for %s: " operation)
                 '(("template" . "Fast template-based generation")
                   ("llm" . "High-quality LLM generation") 
                   ("hybrid" . "LLM with template fallback"))
                 nil t nil nil (symbol-name default-mode)))
      default-mode)))

(defun protagentic-generator-check-prerequisites (mode)
  "Check if prerequisites for MODE are satisfied.
Returns t if ready, otherwise provides guidance and returns nil."
  (pcase mode
    ((or 'llm 'hybrid)
     (cond
      ((not (protagentic-config-get-api-key))
       (message "LLM mode requires API key configuration. Run: M-x protagentic-config-setup")
       nil)
      
      ((not (protagentic-llm-available-p))
       (message "LLM service not available. Check network connection and API key.")
       nil)
      
      (t t)))
    
    ('template t)
    
    (t
     (message "Invalid generation mode: %s" mode)
     nil)))

(provide 'protagentic-generator)

;;; protagentic-generator.el ends here