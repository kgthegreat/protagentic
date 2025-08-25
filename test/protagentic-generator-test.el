;;; protagentic-generator-test.el --- Tests for protagentic-generator -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Unit tests for protagentic-generator module.

;;; Code:

(require 'ert)
(require 'protagentic-generator)
(require 'protagentic-prompts)

;; Test data structures

(ert-deftest protagentic-generator-test-generation-context-structure ()
  "Test generation context structure creation and access."
  (let ((context (make-protagentic-generation-context
                  :feature-description "Test feature"
                  :requirements-content "Test requirements"
                  :design-content "Test design"
                  :project-files '("file1.el" "file2.el")
                  :generation-mode 'llm
                  :user-preferences '(:key value)
                  :spec-name "test-spec")))
    (should (equal (protagentic-generation-context-feature-description context) "Test feature"))
    (should (equal (protagentic-generation-context-requirements-content context) "Test requirements"))
    (should (equal (protagentic-generation-context-design-content context) "Test design"))
    (should (equal (protagentic-generation-context-project-files context) '("file1.el" "file2.el")))
    (should (equal (protagentic-generation-context-generation-mode context) 'llm))
    (should (equal (protagentic-generation-context-user-preferences context) '(:key value)))
    (should (equal (protagentic-generation-context-spec-name context) "test-spec"))))

(ert-deftest protagentic-generator-test-generation-result-structure ()
  "Test generation result structure creation and access."
  (let ((result (make-protagentic-generation-result
                 :content "Generated content"
                 :generation-mode-used 'template
                 :success t
                 :error-message nil
                 :tokens-used 150
                 :cost-estimate 0.30
                 :fallback-used nil
                 :generation-time 2.5)))
    (should (equal (protagentic-generation-result-content result) "Generated content"))
    (should (equal (protagentic-generation-result-generation-mode-used result) 'template))
    (should (equal (protagentic-generation-result-success result) t))
    (should (equal (protagentic-generation-result-error-message result) nil))
    (should (equal (protagentic-generation-result-tokens-used result) 150))
    (should (equal (protagentic-generation-result-cost-estimate result) 0.30))
    (should (equal (protagentic-generation-result-fallback-used result) nil))
    (should (equal (protagentic-generation-result-generation-time result) 2.5))))

;; Test utility functions

(ert-deftest protagentic-generator-test-token-estimation ()
  "Test token count estimation."
  (should (equal (protagentic-generator--estimate-tokens nil) 0))
  (should (equal (protagentic-generator--estimate-tokens "") 0))
  (should (equal (protagentic-generator--estimate-tokens "test") 1))
  (should (equal (protagentic-generator--estimate-tokens "this is a test") 3))
  (should (> (protagentic-generator--estimate-tokens "this is a longer test string") 5)))

(ert-deftest protagentic-generator-test-spec-name-extraction ()
  "Test spec name extraction from feature description."
  (should (equal (protagentic-generator--extract-spec-name-from-description 
                  "User authentication system") "user-authentication-system"))
  (should (equal (protagentic-generator--extract-spec-name-from-description 
                  "File upload") "file-upload"))
  (should (equal (protagentic-generator--extract-spec-name-from-description 
                  "API rate limiting feature") "api-rate-limiting"))
  (should (equal (protagentic-generator--extract-spec-name-from-description 
                  "Very long feature description with many words") "very-long-feature")))

;; Test prompt builders

(ert-deftest protagentic-generator-test-requirements-prompt-builder ()
  "Test requirements prompt generation."
  (let* ((context (make-protagentic-generation-context
                   :feature-description "Test feature"
                   :spec-name "test-spec"))
         (prompt (protagentic-prompts-generate-requirements-prompt context)))
    (should (stringp prompt))
    (should (> (length prompt) 100))
    (should (string-match-p "Test feature" prompt))
    (should (string-match-p "Requirements Document" prompt))
    (should (string-match-p "User Story" prompt))
    (should (string-match-p "Acceptance Criteria" prompt))
    (should (string-match-p "EARS format" prompt))))

(ert-deftest protagentic-generator-test-design-prompt-builder ()
  "Test design prompt generation."
  (let* ((context (make-protagentic-generation-context
                   :requirements-content "Test requirements content"
                   :project-files '("test.py")))
         (prompt (protagentic-prompts-generate-design-prompt context)))
    (should (stringp prompt))
    (should (> (length prompt) 100))
    (should (string-match-p "Test requirements content" prompt))
    (should (string-match-p "Design Document" prompt))
    (should (string-match-p "Architecture" prompt))
    (should (string-match-p "Technology Stack" prompt))
    (should (string-match-p "Components and Interfaces" prompt))))

(ert-deftest protagentic-generator-test-tasks-prompt-builder ()
  "Test tasks prompt generation."
  (let* ((context (make-protagentic-generation-context
                   :requirements-content "Test requirements"
                   :design-content "Test design"))
         (prompt (protagentic-prompts-generate-tasks-prompt context)))
    (should (stringp prompt))
    (should (> (length prompt) 100))
    (should (string-match-p "Test requirements" prompt))
    (should (string-match-p "Test design" prompt))
    (should (string-match-p "Implementation Plan" prompt))
    (should (string-match-p "Requirements:" prompt))
    (should (string-match-p "technical implementation activities" prompt))))

;; Test mode selection and prerequisites

(ert-deftest protagentic-generator-test-mode-prerequisites ()
  "Test generation mode prerequisite checking."
  ;; Template mode should always be available
  (should (protagentic-generator-check-prerequisites 'template))
  
  ;; Invalid mode should return nil
  (should-not (protagentic-generator-check-prerequisites 'invalid-mode)))

;; Test template generation (mock)

(ert-deftest protagentic-generator-test-template-generation ()
  "Test template-based generation logic."
  (let ((context (make-protagentic-generation-context
                  :feature-description "Test feature"
                  :generation-mode 'template)))
    
    ;; Mock the template generation function
    (cl-letf (((symbol-function 'protagentic-generator--generate-requirements-content)
               (lambda (ctx mode) 
                 (should (eq mode 'template))
                 "Mock requirements content")))
      
      (let ((result (protagentic-generator--generate-with-template 
                     context 'requirements 
                     #'protagentic-generator--generate-requirements-content)))
        (should (protagentic-generation-result-success result))
        (should (equal (protagentic-generation-result-content result) "Mock requirements content"))
        (should (eq (protagentic-generation-result-generation-mode-used result) 'template))
        (should (equal (protagentic-generation-result-tokens-used result) 0))
        (should (equal (protagentic-generation-result-cost-estimate result) 0.0))
        (should-not (protagentic-generation-result-fallback-used result))))))

;; Test error handling

(ert-deftest protagentic-generator-test-template-generation-error ()
  "Test template generation error handling."
  (let ((context (make-protagentic-generation-context
                  :feature-description "Test feature"
                  :generation-mode 'template)))
    
    ;; Mock a failing template generation function
    (cl-letf (((symbol-function 'protagentic-generator--generate-requirements-content)
               (lambda (ctx mode) 
                 (error "Mock template error"))))
      
      (let ((result (protagentic-generator--generate-with-template 
                     context 'requirements 
                     #'protagentic-generator--generate-requirements-content)))
        (should-not (protagentic-generation-result-success result))
        (should (null (protagentic-generation-result-content result)))
        (should (eq (protagentic-generation-result-generation-mode-used result) 'template))
        (should (string-match-p "Mock template error" 
                               (protagentic-generation-result-error-message result)))))))

;; Test generation execution logic

(ert-deftest protagentic-generator-test-execution-mode-selection ()
  "Test generation execution with different modes."
  (let ((context (make-protagentic-generation-context :generation-mode 'template)))
    
    ;; Mock the template generation
    (cl-letf (((symbol-function 'protagentic-generator--generate-with-template)
               (lambda (ctx type func)
                 (make-protagentic-generation-result
                  :content "Template result"
                  :generation-mode-used 'template
                  :success t))))
      
      (let ((result (protagentic-generator--execute-generation 
                     context 'requirements 
                     #'protagentic-generator--generate-requirements-content)))
        (should (protagentic-generation-result-success result))
        (should (equal (protagentic-generation-result-content result) "Template result"))
        (should (eq (protagentic-generation-result-generation-mode-used result) 'template)))))
  
  ;; Test invalid mode
  (let ((context (make-protagentic-generation-context :generation-mode 'invalid)))
    (let ((result (protagentic-generator--execute-generation 
                   context 'requirements 
                   #'protagentic-generator--generate-requirements-content)))
      (should-not (protagentic-generation-result-success result))
      (should (string-match-p "Invalid generation mode" 
                             (protagentic-generation-result-error-message result))))))

;; Test content generation functions

(ert-deftest protagentic-generator-test-requirements-content-generation ()
  "Test requirements content generation routing."
  (let ((context (make-protagentic-generation-context
                  :feature-description "Test feature")))
    
    ;; Mock template generation
    (cl-letf (((symbol-function 'protagentic--generate-requirements-template)
               (lambda (desc) "Template requirements")))
      
      (let ((content (protagentic-generator--generate-requirements-content context 'template)))
        (should (equal content "Template requirements"))))
    
    ;; Test invalid mode
    (should-error (protagentic-generator--generate-requirements-content context 'invalid))))

(ert-deftest protagentic-generator-test-design-content-generation ()
  "Test design content generation routing."
  (let ((context (make-protagentic-generation-context
                  :requirements-content "Test requirements")))
    
    ;; Mock template generation
    (cl-letf (((symbol-function 'protagentic--generate-design-template)
               (lambda (req) "Template design")))
      
      (let ((content (protagentic-generator--generate-design-content context 'template)))
        (should (equal content "Template design"))))
    
    ;; Test invalid mode
    (should-error (protagentic-generator--generate-design-content context 'invalid))))

(ert-deftest protagentic-generator-test-tasks-content-generation ()
  "Test tasks content generation routing."
  (let ((context (make-protagentic-generation-context
                  :design-content "Test design"
                  :requirements-content "Test requirements")))
    
    ;; Mock template generation
    (cl-letf (((symbol-function 'protagentic--generate-tasks-template)
               (lambda (design) "Template tasks")))
      
      (let ((content (protagentic-generator--generate-tasks-content context 'template)))
        (should (equal content "Template tasks"))))
    
    ;; Test invalid mode
    (should-error (protagentic-generator--generate-tasks-content context 'invalid))))

;; Test logging functionality

(ert-deftest protagentic-generator-test-result-logging ()
  "Test generation result logging."
  (let ((success-result (make-protagentic-generation-result
                         :content "Test content"
                         :generation-mode-used 'template
                         :success t
                         :generation-time 1.5
                         :tokens-used 100
                         :cost-estimate 0.20))
        (failure-result (make-protagentic-generation-result
                         :success nil
                         :error-message "Test error"
                         :generation-mode-used 'llm)))
    
    ;; Test that logging doesn't throw errors
    (should (progn (protagentic-generator--log-generation-result success-result 'requirements) t))
    
    ;; Test failure logging - should return the error message, not throw error
    (should (stringp (protagentic-generator--log-generation-result failure-result 'design)))))

(provide 'protagentic-generator-test)

;;; protagentic-generator-test.el ends here