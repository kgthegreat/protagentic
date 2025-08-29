;;; protagentic-llm.el --- LLM integration for Protagentic -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; LLM service layer for Protagentic providing OpenAI API integration.
;; Handles API communication, request formatting, and response parsing.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)

(defgroup protagentic-llm nil
  "LLM integration settings for Protagentic."
  :group 'protagentic
  :prefix "protagentic-llm-")

(defcustom protagentic-llm-api-base-url "https://api.openai.com/v1"
  "Base URL for OpenAI API."
  :type 'string
  :group 'protagentic-llm)

(defcustom protagentic-llm-default-model "gpt-4"
  "Default OpenAI model to use for content generation."
  :type 'string
  :group 'protagentic-llm)

(defcustom protagentic-llm-default-max-tokens 4000
  "Default maximum tokens for LLM responses."
  :type 'integer
  :group 'protagentic-llm)

(defcustom protagentic-llm-default-temperature 0.7
  "Default temperature for LLM responses (0.0-2.0)."
  :type 'float
  :group 'protagentic-llm)

(defcustom protagentic-llm-request-timeout 60
  "Timeout in seconds for LLM API requests."
  :type 'integer
  :group 'protagentic-llm)

(defcustom protagentic-llm-api-key nil
  "OpenAI API key for LLM integration.
It's recommended to use environment variables or auth-source instead."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "API Key"))
  :group 'protagentic-llm)

;; Data structures

(cl-defstruct protagentic-llm-request
  "Structure representing an LLM API request."
  prompt
  model
  max-tokens
  temperature
  context
  type
  timestamp)

(cl-defstruct protagentic-llm-response
  "Structure representing an LLM API response."
  content
  tokens-used
  model
  finish-reason
  error
  timestamp)

;; API client functions

(defun protagentic-llm--get-api-key ()
  "Retrieve the OpenAI API key from configuration.
Returns nil if not configured."
  (protagentic-config-get-api-key))

(defun protagentic-llm--create-headers (api-key)
  "Create HTTP headers for OpenAI API request with API-KEY."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(format "Bearer %s" api-key))
    ("User-Agent" . "Protagentic-Emacs/1.0")))

(defun protagentic-llm--create-request-body (prompt &optional model max-tokens temperature)
  "Create request body for OpenAI API.
PROMPT is the text to send to the LLM.
MODEL, MAX-TOKENS, and TEMPERATURE are optional parameters."
  (let ((body `((model . ,(or model protagentic-llm-default-model))
                (messages . [((role . "user") (content . ,prompt))])
                (max_tokens . ,(or max-tokens protagentic-llm-default-max-tokens))
                (temperature . ,(or temperature protagentic-llm-default-temperature)))))
    (json-encode body)))

(defun protagentic-llm--parse-response (response-buffer)
  "Parse OpenAI API response from RESPONSE-BUFFER.
Returns a protagentic-llm-response structure."
  (with-current-buffer response-buffer
    (goto-char (point-min))
    (let ((status-line (buffer-substring-no-properties 
                       (line-beginning-position) (line-end-position))))
      (if (string-match "HTTP/[0-9.]+ 200" status-line)
          (progn
            ;; Find the JSON response body
            (search-forward "\n\n" nil t)
            (let* ((json-response (json-read))
                   (choices (cdr (assoc 'choices json-response)))
                   (first-choice (aref choices 0))
                   (message (cdr (assoc 'message first-choice)))
                   (content (cdr (assoc 'content message)))
                   (usage (cdr (assoc 'usage json-response)))
                   (tokens-used (cdr (assoc 'total_tokens usage)))
                   (finish-reason (cdr (assoc 'finish_reason first-choice))))
              (make-protagentic-llm-response
               :content content
               :tokens-used tokens-used
               :model (cdr (assoc 'model json-response))
               :finish-reason finish-reason
               :error nil
               :timestamp (current-time))))
        ;; Handle error response
        (search-forward "\n\n" nil t)
        (let* ((error-json (ignore-errors (json-read)))
               (error-msg (if error-json
                             (cdr (assoc 'message (cdr (assoc 'error error-json))))
                           "Unknown API error")))
          (make-protagentic-llm-response
           :content nil
           :tokens-used 0
           :model nil
           :finish-reason nil
           :error error-msg
           :timestamp (current-time)))))))

(defun protagentic-llm--make-request (prompt &optional model max-tokens temperature)
  "Make synchronous request to OpenAI API.
PROMPT is the text to send. MODEL, MAX-TOKENS, and TEMPERATURE are optional.
Returns a protagentic-llm-response structure."
  (let ((api-key (protagentic-llm--get-api-key)))
    (unless api-key
      (error "OpenAI API key not configured. Set OPENAI_API_KEY environment variable or configure protagentic-llm-api-key"))
    
    (let* ((url-request-method "POST")
           (url-request-extra-headers (protagentic-llm--create-headers api-key))
           (url-request-data (protagentic-llm--create-request-body 
                             prompt model max-tokens temperature))
           (url (concat protagentic-llm-api-base-url "/chat/completions"))
           (response-buffer nil)
           (model-name (or model protagentic-llm-default-model))
           (request-size (length url-request-data)))
      
      (message "🔧 Using model: %s" model-name)
      (message "📊 Request payload: %d bytes" request-size)
      (message "🌐 Sending request to: %s" url)
      
      (condition-case err
          (progn
            (message "📡 Making HTTP request...")
            (let ((start-time (current-time)))
              (setq response-buffer (url-retrieve-synchronously url nil nil protagentic-llm-request-timeout))
              (let ((request-time (float-time (time-subtract (current-time) start-time))))
                (message "📥 HTTP response received (%.1fs)" request-time)))
            
            (if response-buffer
                (progn
                  (message "🔍 Parsing API response...")
                  (let ((parsed-response (protagentic-llm--parse-response response-buffer)))
                    (kill-buffer response-buffer)
                    (if (protagentic-llm-response-error parsed-response)
                        (message "❌ API returned error: %s" (protagentic-llm-response-error parsed-response))
                      (message "✅ Response parsed successfully - %d tokens used" 
                               (or (protagentic-llm-response-tokens-used parsed-response) 0)))
                    parsed-response))
              (progn
                (message "❌ No response received from API")
                (make-protagentic-llm-response
                 :content nil
                 :tokens-used 0
                 :model nil
                 :finish-reason nil
                 :error "Request timeout or connection failed"
                 :timestamp (current-time)))))
        (error
         (message "❌ Request failed with error: %s" (error-message-string err))
         (when response-buffer
           (kill-buffer response-buffer))
         (make-protagentic-llm-response
          :content nil
          :tokens-used 0
          :model nil
          :finish-reason nil
          :error (format "Request failed: %s" (error-message-string err))
          :timestamp (current-time)))))))

;; Public API functions

(defun protagentic-llm-generate-content (prompt content-type &optional context)
  "Generate content using LLM for given PROMPT and CONTENT-TYPE.
CONTENT-TYPE should be one of 'requirements, 'design, or 'tasks.
CONTEXT is optional additional context information.
Returns the generated content as a string or nil on error."
  (message "🔧 Preparing LLM request...")
  (let* ((request (make-protagentic-llm-request
                   :prompt prompt
                   :model protagentic-llm-default-model
                   :max-tokens protagentic-llm-default-max-tokens
                   :temperature protagentic-llm-default-temperature
                   :context context
                   :type content-type
                   :timestamp (current-time))))
    
    (message "📡 Making API request...")
    (let* ((response (protagentic-llm--make-request 
                     (protagentic-llm-request-prompt request)
                     (protagentic-llm-request-model request)
                     (protagentic-llm-request-max-tokens request)
                     (protagentic-llm-request-temperature request))))
      
      (if (protagentic-llm-response-error response)
          (progn
            (message "❌ LLM generation failed: %s" (protagentic-llm-response-error response))
            nil)
        (progn
          (message "🔄 Processing LLM response...")
          (let ((raw-content (protagentic-llm-response-content response)))
            (message "📝 Raw response length: %d characters" (length raw-content))
            ;; Parse and format the response
            (let ((formatted-content (protagentic-llm--parse-and-format-content raw-content content-type)))
              (message "✅ Content formatting completed")
              formatted-content)))))))

(defun protagentic-llm-validate-credentials ()
  "Validate OpenAI API credentials.
Returns t if credentials are valid, nil otherwise."
  (let ((api-key (protagentic-llm--get-api-key)))
    (if (not api-key)
        (progn
          (message "No API key configured")
          nil)
      (let ((response (protagentic-llm--make-request "Test" "gpt-3.5-turbo" 10 0.1)))
        (if (protagentic-llm-response-error response)
            (progn
              (message "API validation failed: %s" (protagentic-llm-response-error response))
              nil)
          (progn
            (message "API credentials validated successfully")
            t))))))

(defun protagentic-llm-estimate-cost (prompt &optional model)
  "Estimate the cost for generating content with PROMPT using MODEL.
Returns estimated cost in USD as a float."
  (let* ((model-name (or model protagentic-llm-default-model))
         (estimated-tokens (+ (/ (length prompt) 4) ; Rough token estimation
                             protagentic-llm-default-max-tokens))
         (cost-per-1k-tokens (cond
                             ((string-match-p "gpt-4" model-name) 0.03)
                             ((string-match-p "gpt-3.5-turbo" model-name) 0.002)
                             (t 0.002)))) ; Default to gpt-3.5-turbo pricing
    (* estimated-tokens (/ cost-per-1k-tokens 1000.0))))

(defun protagentic-llm-available-p ()
  "Check if LLM service is available.
Returns t if API key is configured and network is available."
  (and (protagentic-llm--get-api-key)
       (condition-case nil
           (let ((response (url-retrieve-synchronously "https://api.openai.com" nil nil 5)))
             (when response
               (kill-buffer response)
               t))
         (error nil))))

;; Response parsing and formatting

(defun protagentic-llm--parse-and-format-content (raw-content content-type)
  "Parse and format RAW-CONTENT for CONTENT-TYPE.
Applies content-specific formatting and validation.
Returns formatted content string."
  (when raw-content
    (let ((cleaned-content (protagentic-llm--clean-raw-content raw-content)))
      (pcase content-type
        ('requirements (protagentic-llm--format-requirements-content cleaned-content))
        ('design (protagentic-llm--format-design-content cleaned-content))
        ('tasks (protagentic-llm--format-tasks-content cleaned-content))
        (t cleaned-content)))))

(defun protagentic-llm--clean-raw-content (content)
  "Clean RAW-CONTENT by removing common LLM artifacts.
Returns cleaned content string."
  (let ((cleaned content))
    ;; Remove common LLM prefixes/suffixes
    (setq cleaned (replace-regexp-in-string "^Here's\\s+" "" cleaned))
    (setq cleaned (replace-regexp-in-string "^Here is\\s+" "" cleaned))
    (setq cleaned (replace-regexp-in-string "^I'll\\s+" "" cleaned))
    (setq cleaned (replace-regexp-in-string "^I will\\s+" "" cleaned))
    
    ;; Remove markdown code block markers if they wrap the entire content
    (when (and (string-match-p "^```" cleaned)
               (string-match-p "```$" cleaned))
      (setq cleaned (replace-regexp-in-string "^```[a-z]*\n?" "" cleaned))
      (setq cleaned (replace-regexp-in-string "\n?```$" "" cleaned)))
    
    ;; Normalize line endings and spacing
    (setq cleaned (replace-regexp-in-string "\r\n" "\n" cleaned))
    (setq cleaned (replace-regexp-in-string "\n\n\n+" "\n\n" cleaned))
    
    ;; Ensure content ends with single newline
    (setq cleaned (string-trim-right cleaned))
    (concat cleaned "\n")))

(defun protagentic-llm--format-requirements-content (content)
  "Format requirements CONTENT to ensure proper structure.
Returns formatted requirements content."
  (let ((formatted content))
    ;; Ensure proper heading hierarchy
    (setq formatted (protagentic-llm--fix-heading-hierarchy formatted))
    
    ;; Fix user story formatting
    (setq formatted (protagentic-llm--fix-user-story-formatting formatted))
    
    ;; Fix acceptance criteria formatting
    (setq formatted (protagentic-llm--fix-acceptance-criteria-formatting formatted))
    
    ;; Validate and fix EARS format
    (setq formatted (protagentic-llm--fix-ears-format formatted))
    
    formatted))

(defun protagentic-llm--format-design-content (content)
  "Format design CONTENT to ensure proper structure.
Returns formatted design content."
  (let ((formatted content))
    ;; Ensure proper heading hierarchy
    (setq formatted (protagentic-llm--fix-heading-hierarchy formatted))
    
    ;; Fix technology stack formatting
    (setq formatted (protagentic-llm--fix-technology-stack-formatting formatted))
    
    ;; Fix component descriptions
    (setq formatted (protagentic-llm--fix-component-formatting formatted))
    
    formatted))

(defun protagentic-llm--format-tasks-content (content)
  "Format tasks CONTENT to ensure proper structure.
Returns formatted tasks content."
  (let ((formatted content))
    ;; Ensure proper heading hierarchy
    (setq formatted (protagentic-llm--fix-heading-hierarchy formatted))
    
    ;; Fix task checkbox formatting
    (setq formatted (protagentic-llm--fix-task-checkbox-formatting formatted))
    
    ;; Fix requirement references
    (setq formatted (protagentic-llm--fix-requirement-references formatted))
    
    formatted))

;; Content formatting helpers

(defun protagentic-llm--fix-heading-hierarchy (content)
  "Fix markdown heading hierarchy in CONTENT.
Ensures proper H1, H2, H3 structure."
  (let ((lines (split-string content "\n"))
        (fixed-lines '())
        (in-main-doc nil))
    
    (dolist (line lines)
      (cond
       ;; Main document heading (should be H1)
       ((string-match-p "^#+\\s-*\\(Requirements\\|Design\\|Implementation Plan\\)\\s-*Document" line)
        (setq in-main-doc t)
        (push (replace-regexp-in-string "^#+\\s-*" "# " line) fixed-lines))
       
       ;; Major sections (should be H2)
       ((and in-main-doc (string-match-p "^#+\\s-*\\(Introduction\\|Requirements\\|Overview\\|Architecture\\)" line))
        (push (replace-regexp-in-string "^#+\\s-*" "## " line) fixed-lines))
       
       ;; Subsections (should be H3)
       ((and in-main-doc (string-match-p "^#+\\s-*\\(Requirement\\s-+[0-9]\\|System Architecture\\|Technology Stack\\)" line))
        (push (replace-regexp-in-string "^#+\\s-*" "### " line) fixed-lines))
       
       ;; Sub-subsections (should be H4)
       ((and in-main-doc (string-match-p "^#+\\s-*\\(Acceptance Criteria\\|Key Responsibilities\\)" line))
        (push (replace-regexp-in-string "^#+\\s-*" "#### " line) fixed-lines))
       
       ;; Keep other lines as-is
       (t (push line fixed-lines))))
    
    (string-join (reverse fixed-lines) "\n")))

(defun protagentic-llm--fix-user-story-formatting (content)
  "Fix user story formatting in CONTENT.
Ensures consistent **User Story:** format."
  (let ((fixed content))
    ;; Replace various user story formats
    (setq fixed (replace-regexp-in-string "\\*?\\*?[Uu]ser [Ss]tory\\*?\\*?:" "**User Story:**" fixed))
    (setq fixed (replace-regexp-in-string "^user story:" "**User Story:**" fixed))
    fixed))

(defun protagentic-llm--fix-acceptance-criteria-formatting (content)
  "Fix acceptance criteria formatting in CONTENT.
Ensures consistent #### Acceptance Criteria format."
  (replace-regexp-in-string 
   "#+\\s-*[Aa]cceptance\\s-+[Cc]riteria\\s*:?"
   "#### Acceptance Criteria"
   content))

(defun protagentic-llm--fix-ears-format (content)
  "Fix EARS format in acceptance criteria in CONTENT.
Ensures proper WHEN/IF/THEN/SHALL structure."
  (let ((fixed content))
    ;; Fix common EARS format issues
    (setq fixed (replace-regexp-in-string 
                 "\\b[Ww][Hh][Ee][Nn]\\b" "WHEN" fixed))
    (setq fixed (replace-regexp-in-string 
                 "\\b[Ii][Ff]\\b" "IF" fixed))
    (setq fixed (replace-regexp-in-string 
                 "\\b[Tt][Hh][Ee][Nn]\\b" "THEN" fixed))
    (setq fixed (replace-regexp-in-string 
                 "\\bthe system [Ss][Hh][Aa][Ll][Ll]\\b" "the system SHALL" fixed))
    (setq fixed (replace-regexp-in-string 
                 "\\bsystem [Ss][Hh][Aa][Ll][Ll]\\b" "system SHALL" fixed))
    fixed))

(defun protagentic-llm--fix-technology-stack-formatting (content)
  "Fix technology stack formatting in CONTENT.
Ensures consistent bullet point format."
  (let ((fixed content))
    ;; Fix technology stack bullet points
    (setq fixed (replace-regexp-in-string 
                 "^\\s-*[-*]\\s-*\\*\\*\\([^:]+\\):\\*\\*"
                 "- **\\1:**"
                 fixed t))
    fixed))

(defun protagentic-llm--fix-component-formatting (content)
  "Fix component section formatting in CONTENT.
Ensures consistent component descriptions."
  (let ((fixed content))
    ;; Fix component headings
    (setq fixed (replace-regexp-in-string 
                 "^#+\\s-*\\([^\\n]+\\)\\s-+Component\\s*$"
                 "#### \\1 Component"
                 fixed t))
    fixed))

(defun protagentic-llm--fix-task-checkbox-formatting (content)
  "Fix task checkbox formatting in CONTENT.
Ensures consistent - [ ] format."
  (let ((fixed content))
    ;; Fix various checkbox formats
    (setq fixed (replace-regexp-in-string "^\\* \\[ \\]" "- [ ]" fixed))
    (setq fixed (replace-regexp-in-string "^- \\[\\]" "- [ ]" fixed))
    
    ;; Convert numbered lists to checkboxes
    (setq fixed (replace-regexp-in-string "^\\([0-9]+\\)\\." "- [ ] \\1." fixed))
    
    fixed))

(defun protagentic-llm--fix-requirement-references (content)
  "Fix requirement reference formatting in CONTENT.
Ensures consistent _Requirements: X.Y_ format."
  (let ((fixed content))
    ;; Fix various requirement reference formats
    (setq fixed (replace-regexp-in-string "Requirements: \\([0-9., ]+\\)" "_Requirements: \\1_" fixed))
    (setq fixed (replace-regexp-in-string "\\*Requirements \\([0-9.]+\\)\\*" "_Requirements: \\1_" fixed))
    (setq fixed (replace-regexp-in-string "_Requirement: \\([0-9.]+\\)_" "_Requirements: \\1_" fixed))
    fixed))

;; Content validation and repair

(defun protagentic-llm--validate-and-repair-content (content content-type)
  "Validate CONTENT for CONTENT-TYPE and attempt automatic repairs.
Returns repaired content or original if no repairs needed."
  (require 'protagentic-prompts)
  (let ((validation-result (protagentic-prompts-validate-output content (symbol-name content-type))))
    (if (eq validation-result t)
        content
      ;; Attempt repairs based on validation errors
      (protagentic-llm--attempt-content-repair content content-type validation-result))))

(defun protagentic-llm--attempt-content-repair (content content-type error-message)
  "Attempt to repair CONTENT for CONTENT-TYPE based on ERROR-MESSAGE.
Returns repaired content."
  (let ((repaired content))
    (cond
     ;; Missing main heading
     ((string-match-p "Missing main heading" error-message)
      (setq repaired (protagentic-llm--add-missing-main-heading repaired content-type)))
     
     ;; Missing sections
     ((string-match-p "Missing.*section" error-message)
      (setq repaired (protagentic-llm--add-missing-sections repaired content-type error-message)))
     
     ;; Missing user stories or acceptance criteria
     ((string-match-p "Missing.*user stories\\|Missing.*acceptance criteria" error-message)
      (setq repaired (protagentic-llm--fix-requirements-structure repaired)))
     
     ;; Missing task checkboxes
     ((string-match-p "Missing task checkboxes" error-message)
      (setq repaired (protagentic-llm--add-task-checkboxes repaired))))
    
    repaired))

(defun protagentic-llm--add-missing-main-heading (content content-type)
  "Add missing main heading to CONTENT for CONTENT-TYPE."
  (let ((heading (pcase content-type
                   ('requirements "# Requirements Document")
                   ('design "# Design Document")
                   ('tasks "# Implementation Plan")
                   (t "# Document"))))
    (if (string-match-p "^#" content)
        content
      (concat heading "\n\n" content))))

(defun protagentic-llm--add-missing-sections (content content-type error-message)
  "Add missing sections to CONTENT for CONTENT-TYPE based on ERROR-MESSAGE."
  (let ((sections-to-add '()))
    (when (string-match-p "Missing Introduction" error-message)
      (push "## Introduction\n\n[Introduction content]\n" sections-to-add))
    (when (string-match-p "Missing Requirements section" error-message)
      (push "## Requirements\n\n[Requirements content]\n" sections-to-add))
    (when (string-match-p "Missing Overview" error-message)
      (push "## Overview\n\n[Overview content]\n" sections-to-add))
    (when (string-match-p "Missing Architecture" error-message)
      (push "## Architecture\n\n[Architecture content]\n" sections-to-add))
    
    (if sections-to-add
        (concat content "\n\n" (string-join (reverse sections-to-add) "\n"))
      content)))

(defun protagentic-llm--fix-requirements-structure (content)
  "Fix requirements structure in CONTENT by adding missing elements."
  (let ((fixed content))
    ;; Add user story format if missing
    (unless (string-match-p "\\*\\*User Story:\\*\\*" fixed)
      (setq fixed (replace-regexp-in-string 
                   "\\(### Requirement [0-9]+\\)\\s-*\n"
                   "\\1\n\n**User Story:** As a [role], I want [capability], so that [benefit]\n"
                   fixed)))
    
    ;; Add acceptance criteria if missing
    (unless (string-match-p "#### Acceptance Criteria" fixed)
      (setq fixed (replace-regexp-in-string 
                   "\\(\\*\\*User Story:\\*\\*[^\n]+\\)\n"
                   "\\1\n\n#### Acceptance Criteria\n\n1. WHEN [condition] THEN the system SHALL [response]\n"
                   fixed)))
    
    fixed))

(defun protagentic-llm--add-task-checkboxes (content)
  "Add task checkboxes to CONTENT if missing."
  (replace-regexp-in-string 
   "^\\s-*\\([0-9]+\\)\\."
   "- [ ] \\1."
   content t))

(provide 'protagentic-llm)

;;; protagentic-llm.el ends here