;;; protagentic-config.el --- Configuration management for Protagentic -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Configuration management for Protagentic including secure credential storage,
;; user preferences, and LLM integration settings.

;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'cus-edit)

(defgroup protagentic-config nil
  "Configuration settings for Protagentic."
  :group 'protagentic
  :prefix "protagentic-config-")

;; Generation mode preferences
(defcustom protagentic-config-default-generation-mode 'template
  "Default content generation mode.
Can be 'template for template-based generation, 'llm for LLM-powered generation,
or 'hybrid for intelligent fallback."
  :type '(choice (const :tag "Template-based (fast, offline)" template)
                 (const :tag "LLM-powered (high quality)" llm)
                 (const :tag "Hybrid (LLM with template fallback)" hybrid))
  :group 'protagentic-config)

(defcustom protagentic-config-prompt-for-mode t
  "Whether to prompt user for generation mode on each operation.
If nil, uses protagentic-config-default-generation-mode."
  :type 'boolean
  :group 'protagentic-config)

;; LLM configuration
(defcustom protagentic-config-llm-model "gpt-4"
  "Default LLM model for content generation."
  :type '(choice (const "gpt-4")
                 (const "gpt-4-turbo")
                 (const "gpt-3.5-turbo")
                 (string :tag "Custom model"))
  :group 'protagentic-config)

(defcustom protagentic-config-llm-max-tokens 4000
  "Maximum tokens for LLM responses."
  :type 'integer
  :group 'protagentic-config)

(defcustom protagentic-config-llm-temperature 0.7
  "Temperature setting for LLM responses (0.0-2.0).
Lower values produce more focused, deterministic output.
Higher values produce more creative, varied output."
  :type 'float
  :group 'protagentic-config)

;; Cost management
(defcustom protagentic-config-monthly-cost-limit 50.0
  "Monthly cost limit for LLM usage in USD.
Set to 0 to disable cost limiting."
  :type 'float
  :group 'protagentic-config)

(defcustom protagentic-config-cost-warning-threshold 0.8
  "Warn when monthly costs reach this fraction of the limit.
For example, 0.8 means warn at 80% of monthly limit."
  :type 'float
  :group 'protagentic-config)

(defcustom protagentic-config-enable-usage-tracking t
  "Whether to track LLM usage statistics."
  :type 'boolean
  :group 'protagentic-config)

;; Prompt customization
(defcustom protagentic-config-custom-prompts-enabled nil
  "Whether to use custom prompt templates."
  :type 'boolean
  :group 'protagentic-config)

;; Data structures for configuration

(cl-defstruct protagentic-api-config
  "Configuration for API credentials and settings."
  api-key
  model-name
  max-tokens
  temperature
  cost-limit
  usage-warnings)

(cl-defstruct protagentic-usage-stats
  "Usage statistics for cost tracking."
  daily-tokens
  daily-cost
  monthly-tokens
  monthly-cost
  request-count
  last-updated
  current-month)

;; Configuration file paths
(defconst protagentic-config--config-dir
  (expand-file-name "protagentic" user-emacs-directory)
  "Directory for Protagentic configuration files.")

(defconst protagentic-config--usage-stats-file
  (expand-file-name "usage-stats.el" protagentic-config--config-dir)
  "File for storing usage statistics.")

(defconst protagentic-config--custom-prompts-file
  (expand-file-name "custom-prompts.el" protagentic-config--config-dir)
  "File for storing custom prompt templates.")

;; Utility functions

(defun protagentic-config--save-customizations ()
  "Save customizations in a compatible way across Emacs versions."
  (condition-case err
      (cond
       ;; Try customize-save-all first (newer Emacs)
       ((fboundp 'customize-save-all)
        (customize-save-all))
       ;; Fallback to custom-save-all (older Emacs)
       ((fboundp 'custom-save-all)
        (custom-save-all))
       ;; Last resort - just message the user
       (t
        (message "Customizations set but not saved automatically")
        (message "Use M-x customize-save-all to save permanently")))
    (error
     ;; Handle specific error cases
     (let ((error-msg (error-message-string err)))
       (cond
        ((string-match-p "would overwrite existing customizations" error-msg)
         (message "Customizations updated in current session")
         (message "Use M-x customize-save-all to save permanently"))
        (t
         (message "Warning: Could not save customizations: %s" error-msg)
         (message "You may need to manually save your configuration")))))))

;; Secure credential management

(defun protagentic-config--store-in-environment (api-key)
  "Store API-KEY as an environment variable."
  (setenv "OPENAI_API_KEY" api-key)
  (message "API key set as environment variable OPENAI_API_KEY")
  (message "To make this permanent, add this to your shell profile:")
  (message "  export OPENAI_API_KEY=\"%s\"" api-key))

(defun protagentic-config--store-in-authinfo (api-key)
  "Store API-KEY in ~/.authinfo file."
  (let ((authinfo-file (expand-file-name "~/.authinfo")))
    (with-temp-buffer
      ;; Read existing content if file exists
      (when (file-exists-p authinfo-file)
        (insert-file-contents authinfo-file))
      
      ;; Remove any existing protagentic entry
      (goto-char (point-min))
      (while (re-search-forward "^machine api\\.openai\\.com.*login protagentic.*" nil t)
        (delete-region (line-beginning-position) (1+ (line-end-position))))
      
      ;; Add new entry
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "machine api.openai.com login protagentic password %s port https\n" api-key))
      
      ;; Write file
      (write-region (point-min) (point-max) authinfo-file)
      (set-file-modes authinfo-file #o600)) ; Make file readable only by user
    
    (message "API key stored in %s" authinfo-file)
    (message "File permissions set to 600 for security")))

(defun protagentic-config--store-as-custom-variable (api-key)
  "Store API-KEY as a custom variable (less secure)."
  (customize-set-variable 'protagentic-llm-api-key api-key)
  (protagentic-config--save-customizations)
  (message "API key stored as custom variable")
  (message "Warning: This method stores the key in plain text in your Emacs configuration"))

(defun protagentic-config-set-api-key (api-key)
  "Store API-KEY securely.
The key can be stored as an environment variable or in a secure file."
  (interactive "sEnter OpenAI API key: ")
  (when (string-empty-p (string-trim api-key))
    (error "API key cannot be empty"))
  
  ;; Validate API key format (basic check)
  (unless (string-match-p "^sk-[A-Za-z0-9]+" api-key)
    (unless (y-or-n-p "API key doesn't match expected format (sk-...). Continue anyway? ")
      (error "API key setup cancelled")))
  
  ;; Offer storage options
  (let ((storage-method 
         (completing-read 
          "How would you like to store the API key? "
          '(("environment" . "Set as environment variable (recommended)")
            ("authinfo" . "Store in ~/.authinfo file"))
          nil t nil nil "environment")))
    
    (pcase storage-method
      ("environment"
       (protagentic-config--store-in-environment api-key))
      ("authinfo"
       (protagentic-config--store-in-authinfo api-key))
      (_ 
       (error "Unknown storage method: %s" storage-method))))
  
  ;; Test the API key
  (when (y-or-n-p "Test API key connectivity now? ")
    (protagentic-config-validate-api-key)))

(defun protagentic-config-get-api-key ()
  "Retrieve API key from various storage locations.
Returns the API key string or nil if not found."
  (or 
   ;; Try environment variable first (most common)
   (getenv "OPENAI_API_KEY")
   
   ;; Try auth-source (if available and working)
   (condition-case nil
       (let ((auth-info (auth-source-search :host "api.openai.com"
                                           :user "protagentic"
                                           :port "https"
                                           :max 1)))
         (when auth-info
           (let ((secret (plist-get (car auth-info) :secret)))
             (if (functionp secret)
                 (funcall secret)
               secret))))
     (error nil))
   
   ;; Try legacy custom variable (for backward compatibility)
   (when (boundp 'protagentic-llm-api-key)
     protagentic-llm-api-key)))

(defun protagentic-config-remove-api-key ()
  "Remove stored API key from all storage locations."
  (interactive)
  (when (y-or-n-p "Remove stored OpenAI API key from all locations? ")
    (let ((removed-from '()))
      
      ;; Remove from environment variable
      (when (getenv "OPENAI_API_KEY")
        (setenv "OPENAI_API_KEY" nil)
        (push "environment variable" removed-from))
      
      ;; Remove from auth-source (if available)
      (condition-case nil
          (let ((auth-info (auth-source-search :host "api.openai.com"
                                              :user "protagentic"
                                              :port "https"
                                              :max 1)))
            (when auth-info
              (auth-source-delete (car auth-info))
              (push "auth-source" removed-from)))
        (error nil))
      
      ;; Remove from custom variable
      (when (boundp 'protagentic-llm-api-key)
        (customize-set-variable 'protagentic-llm-api-key nil)
        (push "custom variable" removed-from))
      
      (if removed-from
          (message "API key removed from: %s" (string-join removed-from ", "))
        (message "No stored API key found")))))

(defun protagentic-config-validate-api-key ()
  "Validate the stored API key by testing connectivity.
Returns t if valid, nil otherwise. Displays results to user."
  (interactive)
  (let ((api-key (protagentic-config-get-api-key)))
    (if (not api-key)
        (progn
          (message "No API key configured. Use M-x protagentic-config-setup to configure.")
          nil)
      (message "Testing API key connectivity...")
      (if (fboundp 'protagentic-llm-validate-credentials)
          (let ((valid (protagentic-llm-validate-credentials)))
            (if valid
                (message "✓ API key is valid and connectivity confirmed")
              (message "✗ API key validation failed. Check key and network connection"))
            valid)
        (progn
          (message "LLM module not loaded - cannot validate credentials")
          nil)))))

;; Configuration setup and management

(defun protagentic-config-setup ()
  "Interactive setup for Protagentic configuration."
  (interactive)
  (with-output-to-temp-buffer "*Protagentic Configuration*"
    (princ "Protagentic Configuration Setup\n")
    (princ "==============================\n\n")
    (princ "This wizard will help you configure Protagentic for optimal use.\n\n"))
  
  ;; API Key setup
  (when (or (not (protagentic-config-get-api-key))
            (y-or-n-p "Configure OpenAI API key? "))
    (let ((api-key (read-string "Enter your OpenAI API key: ")))
      (protagentic-config-set-api-key api-key)))
  
  ;; Generation mode preference
  (let ((mode (completing-read 
               "Default generation mode: "
               '(("template" . "Fast, offline template-based generation")
                 ("llm" . "High-quality LLM-powered generation")
                 ("hybrid" . "LLM with template fallback"))
               nil t nil nil "hybrid")))
    (customize-set-variable 'protagentic-config-default-generation-mode (intern mode))
    (message "Default generation mode set to: %s" mode))
  
  ;; Cost management
  (when (eq protagentic-config-default-generation-mode 'llm)
    (let ((cost-limit (read-number "Monthly cost limit (USD, 0 for no limit): " 50.0)))
      (customize-set-variable 'protagentic-config-monthly-cost-limit cost-limit)))
  
  ;; Model selection
  (when (memq protagentic-config-default-generation-mode '(llm hybrid))
    (let ((model (completing-read 
                  "Preferred LLM model: "
                  '("gpt-4" "gpt-4-turbo" "gpt-3.5-turbo")
                  nil t nil nil "gpt-4")))
      (customize-set-variable 'protagentic-config-llm-model model)))
  
  ;; Save configuration
  (protagentic-config--save-customizations)
  (message "Configuration saved successfully!"))

(defun protagentic-config-show-status ()
  "Display current configuration status."
  (interactive)
  (let ((api-key-configured (not (null (protagentic-config-get-api-key))))
        (usage-stats (protagentic-config-load-usage-stats)))
    
    (with-output-to-temp-buffer "*Protagentic Status*"
      (princ "Protagentic Configuration Status\n")
      (princ "===============================\n\n")
      
      ;; API Configuration
      (princ "API Configuration:\n")
      (princ (format "  API Key: %s\n" 
                     (if api-key-configured "✓ Configured" "✗ Not configured")))
      (princ (format "  Default Model: %s\n" protagentic-config-llm-model))
      (princ (format "  Max Tokens: %d\n" protagentic-config-llm-max-tokens))
      (princ (format "  Temperature: %.1f\n\n" protagentic-config-llm-temperature))
      
      ;; Generation Settings
      (princ "Generation Settings:\n")
      (princ (format "  Default Mode: %s\n" protagentic-config-default-generation-mode))
      (princ (format "  Prompt for Mode: %s\n\n" 
                     (if protagentic-config-prompt-for-mode "Yes" "No")))
      
      ;; Cost Management
      (princ "Cost Management:\n")
      (princ (format "  Monthly Limit: $%.2f\n" protagentic-config-monthly-cost-limit))
      (princ (format "  Warning Threshold: %.0f%%\n" 
                     (* protagentic-config-cost-warning-threshold 100)))
      (princ (format "  Usage Tracking: %s\n\n" 
                     (if protagentic-config-enable-usage-tracking "Enabled" "Disabled")))
      
      ;; Usage Statistics
      (when (and protagentic-config-enable-usage-tracking usage-stats)
        (princ "Current Usage:\n")
        (princ (format "  This Month: $%.2f (%.0f tokens)\n" 
                       (protagentic-usage-stats-monthly-cost usage-stats)
                       (protagentic-usage-stats-monthly-tokens usage-stats)))
        (princ (format "  Today: $%.2f (%.0f tokens)\n" 
                       (protagentic-usage-stats-daily-cost usage-stats)
                       (protagentic-usage-stats-daily-tokens usage-stats)))
        (princ (format "  Total Requests: %d\n\n" 
                       (protagentic-usage-stats-request-count usage-stats))))
      
      ;; Quick Actions
      (princ "Quick Actions:\n")
      (princ "  M-x protagentic-config-setup - Run configuration wizard\n")
      (princ "  M-x protagentic-config-validate-api-key - Test API connectivity\n")
      (princ "  M-x protagentic-config-reset-usage-stats - Reset usage tracking\n"))))

;; Usage tracking

(defun protagentic-config-ensure-config-dir ()
  "Ensure the configuration directory exists."
  (unless (file-directory-p protagentic-config--config-dir)
    (make-directory protagentic-config--config-dir t)))

(defun protagentic-config-load-usage-stats ()
  "Load usage statistics from file.
Returns a protagentic-usage-stats structure or nil if not found."
  (when (file-exists-p protagentic-config--usage-stats-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents protagentic-config--usage-stats-file)
          (read (current-buffer)))
      (error
       (message "Warning: Could not load usage statistics: %s" (error-message-string err))
       nil))))

(defun protagentic-config-save-usage-stats (stats)
  "Save usage STATS to file."
  (protagentic-config-ensure-config-dir)
  (condition-case err
      (with-temp-file protagentic-config--usage-stats-file
        (prin1 stats (current-buffer)))
    (error
     (message "Warning: Could not save usage statistics: %s" (error-message-string err)))))

(defun protagentic-config-track-usage (tokens cost)
  "Track usage of TOKENS and COST.
Updates daily and monthly statistics."
  (when protagentic-config-enable-usage-tracking
    (let* ((stats (or (protagentic-config-load-usage-stats)
                      (make-protagentic-usage-stats
                       :daily-tokens 0 :daily-cost 0.0
                       :monthly-tokens 0 :monthly-cost 0.0
                       :request-count 0
                       :last-updated (current-time)
                       :current-month (format-time-string "%Y-%m"))))
           (current-month (format-time-string "%Y-%m"))
           (today (format-time-string "%Y-%m-%d"))
           (last-update-day (format-time-string "%Y-%m-%d" 
                                               (protagentic-usage-stats-last-updated stats))))
      
      ;; Reset daily stats if it's a new day
      (unless (string= today last-update-day)
        (setf (protagentic-usage-stats-daily-tokens stats) 0)
        (setf (protagentic-usage-stats-daily-cost stats) 0.0))
      
      ;; Reset monthly stats if it's a new month
      (unless (string= current-month (protagentic-usage-stats-current-month stats))
        (setf (protagentic-usage-stats-monthly-tokens stats) 0)
        (setf (protagentic-usage-stats-monthly-cost stats) 0.0)
        (setf (protagentic-usage-stats-current-month stats) current-month))
      
      ;; Update statistics
      (cl-incf (protagentic-usage-stats-daily-tokens stats) tokens)
      (cl-incf (protagentic-usage-stats-daily-cost stats) cost)
      (cl-incf (protagentic-usage-stats-monthly-tokens stats) tokens)
      (cl-incf (protagentic-usage-stats-monthly-cost stats) cost)
      (cl-incf (protagentic-usage-stats-request-count stats))
      (setf (protagentic-usage-stats-last-updated stats) (current-time))
      
      ;; Check cost limits
      (protagentic-config-check-cost-limits stats)
      
      ;; Save updated statistics
      (protagentic-config-save-usage-stats stats))))

(defun protagentic-config-check-cost-limits (stats)
  "Check if usage STATS exceed configured limits and warn user."
  (let ((monthly-cost (protagentic-usage-stats-monthly-cost stats))
        (cost-limit protagentic-config-monthly-cost-limit)
        (warning-threshold protagentic-config-cost-warning-threshold))
    
    (when (and (> cost-limit 0) (> monthly-cost 0))
      (let ((usage-ratio (/ monthly-cost cost-limit)))
        (cond
         ((>= usage-ratio 1.0)
          (message "⚠️  Monthly cost limit exceeded: $%.2f / $%.2f" monthly-cost cost-limit)
          (when (y-or-n-p "Cost limit exceeded. Switch to template-based generation? ")
            (customize-set-variable 'protagentic-config-default-generation-mode 'template)))
         
         ((>= usage-ratio warning-threshold)
          (message "⚠️  Approaching monthly cost limit: $%.2f / $%.2f (%.0f%%)" 
                   monthly-cost cost-limit (* usage-ratio 100))))))))

(defun protagentic-config-reset-usage-stats ()
  "Reset usage statistics."
  (interactive)
  (when (y-or-n-p "Reset all usage statistics? ")
    (let ((stats (make-protagentic-usage-stats
                  :daily-tokens 0 :daily-cost 0.0
                  :monthly-tokens 0 :monthly-cost 0.0
                  :request-count 0
                  :last-updated (current-time)
                  :current-month (format-time-string "%Y-%m"))))
      (protagentic-config-save-usage-stats stats)
      (message "Usage statistics reset"))))

;; Generation mode management

(defun protagentic-config-get-generation-mode ()
  "Get the current generation mode preference.
Returns 'template, 'llm, or 'hybrid."
  protagentic-config-default-generation-mode)

(defun protagentic-config-set-generation-mode (mode)
  "Set the default generation MODE.
MODE should be 'template, 'llm, or 'hybrid."
  (interactive (list (intern (completing-read 
                              "Generation mode: "
                              '("template" "llm" "hybrid")
                              nil t))))
  (customize-set-variable 'protagentic-config-default-generation-mode mode)
  (message "Default generation mode set to: %s" mode))

(defun protagentic-config-prompt-for-generation-mode ()
  "Prompt user to select generation mode for current operation.
Returns the selected mode symbol."
  (if protagentic-config-prompt-for-mode
      (intern (completing-read 
               "Generation mode for this operation: "
               '(("template" . "Fast template-based generation")
                 ("llm" . "High-quality LLM generation")
                 ("hybrid" . "LLM with template fallback"))
               nil t nil nil (symbol-name protagentic-config-default-generation-mode)))
    protagentic-config-default-generation-mode))

;; Code quality and execution configuration
(defcustom protagentic-code-quality-standard "clean-code"
  "Code quality standard to enforce during generation.
Options: 'clean-code', 'google', 'airbnb', 'custom'."
  :type '(choice (const :tag "Clean Code" "clean-code")
                 (const :tag "Google Style" "google")
                 (const :tag "Airbnb Style" "airbnb")
                 (const :tag "Custom" "custom"))
  :group 'protagentic-config)

(defcustom protagentic-enforce-test-coverage t
  "Whether to enforce test coverage requirements."
  :type 'boolean
  :group 'protagentic-config)

(defcustom protagentic-min-test-coverage 90
  "Minimum test coverage percentage required."
  :type 'integer
  :group 'protagentic-config)

(defcustom protagentic-auto-validate-quality t
  "Whether to automatically validate code quality after generation."
  :type 'boolean
  :group 'protagentic-config)

(defcustom protagentic-max-function-lines 30
  "Maximum lines allowed in a single function."
  :type 'integer
  :group 'protagentic-config)

(defcustom protagentic-enforce-naming-conventions t
  "Whether to enforce naming conventions."
  :type 'boolean
  :group 'protagentic-config)

;; Configuration getters for new features
(defun protagentic-config-get-code-quality-standard ()
  "Get the configured code quality standard."
  protagentic-code-quality-standard)

(defun protagentic-config-get-test-coverage-requirement ()
  "Get the minimum test coverage requirement."
  protagentic-min-test-coverage)

(defun protagentic-config-should-auto-validate-quality ()
  "Check if automatic quality validation is enabled."
  protagentic-auto-validate-quality)

(defun protagentic-config-should-enforce-test-coverage ()
  "Check if test coverage enforcement is enabled."
  protagentic-enforce-test-coverage)

(provide 'protagentic-config)

;;; protagentic-config.el ends here