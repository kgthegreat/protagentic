;;; protagentic-llm-integration-test.el --- Integration tests for LLM functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Integration tests for Protagentic LLM functionality including end-to-end workflows.

;;; Code:

(require 'ert)
(require 'protagentic-llm)
(require 'protagentic-config)
(require 'protagentic-generator)
(require 'protagentic-prompts)

;; Test utilities for LLM integration

(defvar protagentic-llm-test-mock-responses
  '((requirements . "# Requirements Document\n\n## Introduction\n\nThis feature provides user authentication.\n\n## Requirements\n\n### Requirement 1\n\n**User Story:** As a user, I want to log in, so that I can access my account\n\n#### Acceptance Criteria\n\n1. WHEN user enters valid credentials THEN system SHALL authenticate user\n2. IF credentials are invalid THEN system SHALL show error message")
    (design . "# Design Document\n\n## Overview\n\nAuthentication system using JWT tokens.\n\n## Architecture\n\n### System Architecture\n\nClient-server architecture with token-based auth.\n\n## Components and Interfaces\n\n#### Auth Component\n\nHandles user authentication and token management.\n\n## Data Models\n\n#### User Model\n\n```\nUser {\n  id: string\n  email: string\n  password_hash: string\n}\n```")
    (tasks . "# Implementation Plan\n\n- [ ] 1. Set up authentication infrastructure\n  - Create user model and database schema\n  - Set up JWT token handling\n  - _Requirements: 1.1_\n\n- [ ] 2. Implement login endpoint\n  - Create login API endpoint\n  - Add credential validation\n  - _Requirements: 1.1, 1.2_"))
  "Mock responses for different content types.")

(defun protagentic-llm-test-mock-api-call (prompt content-type &optional context)
  "Mock LLM API call returning predefined responses."
  (let ((response (cdr (assoc content-type protagentic-llm-test-mock-responses))))
    (if response
        response
      (error "No mock response for content type: %s" content-type))))

(defmacro protagentic-llm-test-with-mocked-api (&rest body)
  "Execute BODY with mocked LLM API calls."
  `(cl-letf (((symbol-function 'protagentic-llm-generate-content)
              #'protagentic-llm-test-mock-api-call)
             ((symbol-function 'protagentic-llm-available-p)
              (lambda () t))
             ((symbol-function 'protagentic-config-get-api-key)
              (lambda () "mock-api-key"))
             ((symbol-function 'protagentic-config-track-usage)
              (lambda (tokens cost) nil))
             ;; Mock interactive functions to prevent prompts
             ((symbol-function 'protagentic-config-prompt-for-generation-mode)
              (lambda () 'llm))
             ((symbol-function 'completing-read)
              (lambda (prompt collection &rest args) 
                (if collection (caar collection) "")))
             ((symbol-function 'read-string)
              (lambda (prompt &optional initial-input history default-value inherit-input-method)
                (or default-value ""))))
     ,@body))

;; Integration tests

(ert-deftest protagentic-llm-integration-test-full-workflow ()
  "Test complete LLM-powered spec generation workflow."
  (protagentic-llm-test-with-mocked-api
   (let ((feature-description "User authentication system"))
     
     ;; Test requirements generation
     (let ((req-result (protagentic-generator-generate-requirements feature-description 'llm)))
       (should (protagentic-generation-result-success req-result))
       (should (string-match-p "Requirements Document" (protagentic-generation-result-content req-result)))
       (should (eq (protagentic-generation-result-generation-mode-used req-result) 'llm))
       
       ;; Test design generation
       (let ((design-result (protagentic-generator-generate-design 
                            (protagentic-generation-result-content req-result) 'llm)))
         (should (protagentic-generation-result-success design-result))
         (should (string-match-p "Design Document" (protagentic-generation-result-content design-result)))
         
         ;; Test tasks generation
         (let ((tasks-result (protagentic-generator-generate-tasks 
                             (protagentic-generation-result-content design-result)
                             (protagentic-generation-result-content req-result) 'llm)))
           (should (protagentic-generation-result-success tasks-result))
           (should (string-match-p "Implementation Plan" (protagentic-generation-result-content tasks-result)))))))))

(ert-deftest protagentic-llm-integration-test-fallback-mechanism ()
  "Test fallback from LLM to template generation."
  (cl-letf (((symbol-function 'protagentic-llm-generate-content)
             (lambda (prompt content-type &optional context)
               (error "Mock LLM failure")))
            ((symbol-function 'protagentic-llm-available-p)
             (lambda () t))
            ((symbol-function 'protagentic-config-get-api-key)
             (lambda () "mock-api-key"))
            ((symbol-function 'protagentic--generate-requirements-template)
             (lambda (desc) "# Requirements Document\n\nTemplate-generated requirements")))
    
    ;; Test hybrid mode fallback
    (let ((result (protagentic-generator-generate-requirements "Test feature" 'hybrid)))
      (should (protagentic-generation-result-success result))
      (should (protagentic-generation-result-fallback-used result))
      (should (string-match-p "Template-generated" (protagentic-generation-result-content result))))))

(ert-deftest protagentic-llm-integration-test-prompt-generation ()
  "Test end-to-end prompt generation and validation."
  (let ((context (make-protagentic-generation-context
                  :feature-description "User authentication system"
                  :spec-name "auth-system"
                  :project-files '("app.py" "requirements.txt"))))
    
    ;; Test requirements prompt
    (let ((req-prompt (protagentic-prompts-generate-requirements-prompt context)))
      (should (stringp req-prompt))
      (should (> (length req-prompt) 500))
      (should (string-match-p "User authentication system" req-prompt))
      (should (string-match-p "requirements analyst" req-prompt))
      (should (string-match-p "Python" req-prompt))) ; Should detect Python from files
    
    ;; Test design prompt
    (setf (protagentic-generation-context-requirements-content context) "Test requirements")
    (let ((design-prompt (protagentic-prompts-generate-design-prompt context)))
      (should (stringp design-prompt))
      (should (> (length design-prompt) 500))
      (should (string-match-p "Test requirements" design-prompt))
      (should (string-match-p "software architect" design-prompt)))
    
    ;; Test tasks prompt
    (setf (protagentic-generation-context-design-content context) "Test design")
    (let ((tasks-prompt (protagentic-prompts-generate-tasks-prompt context)))
      (should (stringp tasks-prompt))
      (should (> (length tasks-prompt) 500))
      (should (string-match-p "Test requirements" tasks-prompt))
      (should (string-match-p "Test design" tasks-prompt))
      (should (string-match-p "technical lead" tasks-prompt)))))



(ert-deftest protagentic-llm-integration-test-configuration-workflow ()
  "Test LLM configuration and setup workflow."
  (let ((protagentic-config-enable-usage-tracking t)
        (protagentic-config-monthly-cost-limit 10.0))
    
    ;; Test usage tracking
    (let ((initial-stats (make-protagentic-usage-stats
                          :daily-tokens 0 :daily-cost 0.0
                          :monthly-tokens 0 :monthly-cost 0.0
                          :request-count 0
                          :current-month (format-time-string "%Y-%m"))))
      
      ;; Mock usage tracking
      (cl-letf (((symbol-function 'protagentic-config-load-usage-stats)
                 (lambda () initial-stats))
                ((symbol-function 'protagentic-config-save-usage-stats)
                 (lambda (stats) stats)))
        
        ;; Test tracking usage
        (protagentic-config-track-usage 100 0.20)
        
        ;; Test cost limit checking
        (let ((high-cost-stats (make-protagentic-usage-stats
                               :monthly-cost 12.0)))
          ;; Mock y-or-n-p to avoid user input
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
            ;; Function should complete successfully without throwing errors
            (should (progn (protagentic-config-check-cost-limits high-cost-stats) t))))))))

(ert-deftest protagentic-llm-integration-test-error-scenarios ()
  "Test various error scenarios and recovery mechanisms."
  ;; Test API key missing
  (cl-letf (((symbol-function 'protagentic-config-get-api-key)
             (lambda () nil)))
    (should-not (protagentic-generator-check-prerequisites 'llm)))
  
  ;; Test network unavailable
  (cl-letf (((symbol-function 'protagentic-config-get-api-key)
             (lambda () "test-key"))
            ((symbol-function 'protagentic-llm-available-p)
             (lambda () nil)))
    (should-not (protagentic-generator-check-prerequisites 'llm)))
  
  ;; Test malformed LLM response handling
  (cl-letf (((symbol-function 'protagentic-llm-generate-content)
             (lambda (prompt content-type &optional context)
               "This is not a proper document format")))
    
    (let ((result (protagentic-generator--generate-requirements-content 
                   (make-protagentic-generation-context 
                    :feature-description "Test") 'llm)))
      (should (stringp result))
      ;; Should still return content even if malformed
      (should (> (length result) 0)))))

(ert-deftest protagentic-llm-integration-test-context-analysis ()
  "Test project context analysis and prompt enhancement."
  ;; Test Python project detection
  (let ((python-context (make-protagentic-generation-context
                         :project-files '("app.py" "requirements.txt" "setup.py"))))
    (should (member "Python" (protagentic-prompts--infer-technology-stack 
                             (protagentic-generation-context-project-files python-context))))
    (should (equal (protagentic-prompts--infer-project-type 
                   (protagentic-generation-context-project-files python-context)) "Python")))
  
  ;; Test JavaScript project detection
  (let ((js-context (make-protagentic-generation-context
                     :project-files '("app.js" "package.json" "webpack.config.js"))))
    (should (member "Node.js/npm" (protagentic-prompts--infer-technology-stack 
                                  (protagentic-generation-context-project-files js-context))))
    (should (equal (protagentic-prompts--infer-project-type 
                   (protagentic-generation-context-project-files js-context)) "JavaScript/TypeScript")))
  
  ;; Test complexity assessment
  (let ((simple-context (make-protagentic-generation-context
                         :requirements-content "Short"
                         :design-content "Short"
                         :project-files '("one.py"))))
    (should (eq (protagentic-prompts--assess-project-complexity simple-context) 'simple)))
  
  (let ((complex-context (make-protagentic-generation-context
                          :requirements-content (make-string 6000 ?x)
                          :design-content (make-string 9000 ?x)
                          :project-files (make-list 20 "file.py"))))
    (should (eq (protagentic-prompts--assess-project-complexity complex-context) 'complex))))



(ert-deftest protagentic-llm-integration-test-generation-modes ()
  "Test different generation modes and their behavior."
  (protagentic-llm-test-with-mocked-api
   ;; Test template mode
   (cl-letf (((symbol-function 'protagentic--generate-requirements-template)
              (lambda (desc) "Template requirements")))
     (let ((result (protagentic-generator-generate-requirements "Test" 'template)))
       (should (protagentic-generation-result-success result))
       (should (eq (protagentic-generation-result-generation-mode-used result) 'template))
       (should-not (protagentic-generation-result-fallback-used result))))
   
   ;; Test LLM mode
   (let ((result (protagentic-generator-generate-requirements "Test" 'llm)))
     (should (protagentic-generation-result-success result))
     (should (eq (protagentic-generation-result-generation-mode-used result) 'llm))
     (should-not (protagentic-generation-result-fallback-used result)))
   
   ;; Test hybrid mode (should use LLM when available)
   (let ((result (protagentic-generator-generate-requirements "Test" 'hybrid)))
     (should (protagentic-generation-result-success result))
     ;; In hybrid mode with working LLM, should use LLM
     (should (eq (protagentic-generation-result-generation-mode-used result) 'llm)))))

;; Performance and stress tests

(ert-deftest protagentic-llm-integration-test-large-content ()
  "Test handling of large content generation."
  (protagentic-llm-test-with-mocked-api
   (let ((large-description (make-string 5000 ?x)))
     (let ((result (protagentic-generator-generate-requirements large-description 'llm)))
       (should (protagentic-generation-result-success result))
       (should (> (length (protagentic-generation-result-content result)) 100))))))

(ert-deftest protagentic-llm-integration-test-concurrent-requests ()
  "Test handling of multiple concurrent generation requests."
  (protagentic-llm-test-with-mocked-api
   (let ((results '()))
     ;; Simulate multiple requests
     (dotimes (i 3)
       (let ((result (protagentic-generator-generate-requirements 
                     (format "Feature %d" i) 'llm)))
         (push result results)))
     
     ;; All should succeed
     (should (cl-every #'protagentic-generation-result-success results))
     (should (= (length results) 3)))))

;; Run all LLM integration tests
(defun protagentic-llm-run-integration-tests ()
  "Run all LLM integration tests."
  (interactive)
  (ert "protagentic-llm-integration-test-"))

(provide 'protagentic-llm-integration-test)

;;; protagentic-llm-integration-test.el ends here