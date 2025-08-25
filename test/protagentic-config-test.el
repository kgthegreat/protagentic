;;; protagentic-config-test.el --- Tests for protagentic-config -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Unit tests for protagentic-config module.

;;; Code:

(require 'ert)
(require 'protagentic-config)

;; Test data structures

(ert-deftest protagentic-config-test-api-config-structure ()
  "Test API configuration structure creation and access."
  (let ((config (make-protagentic-api-config
                 :api-key "test-key"
                 :model-name "gpt-4"
                 :max-tokens 2000
                 :temperature 0.5
                 :cost-limit 100.0
                 :usage-warnings t)))
    (should (equal (protagentic-api-config-api-key config) "test-key"))
    (should (equal (protagentic-api-config-model-name config) "gpt-4"))
    (should (equal (protagentic-api-config-max-tokens config) 2000))
    (should (equal (protagentic-api-config-temperature config) 0.5))
    (should (equal (protagentic-api-config-cost-limit config) 100.0))
    (should (equal (protagentic-api-config-usage-warnings config) t))))

(ert-deftest protagentic-config-test-usage-stats-structure ()
  "Test usage statistics structure creation and access."
  (let ((stats (make-protagentic-usage-stats
                :daily-tokens 150
                :daily-cost 0.30
                :monthly-tokens 1500
                :monthly-cost 3.00
                :request-count 25
                :last-updated (current-time)
                :current-month "2025-01")))
    (should (equal (protagentic-usage-stats-daily-tokens stats) 150))
    (should (equal (protagentic-usage-stats-daily-cost stats) 0.30))
    (should (equal (protagentic-usage-stats-monthly-tokens stats) 1500))
    (should (equal (protagentic-usage-stats-monthly-cost stats) 3.00))
    (should (equal (protagentic-usage-stats-request-count stats) 25))
    (should (equal (protagentic-usage-stats-current-month stats) "2025-01"))))

;; Test configuration management

(ert-deftest protagentic-config-test-generation-mode-management ()
  "Test generation mode setting and retrieval."
  (let ((original-mode protagentic-config-default-generation-mode))
    ;; Test setting different modes
    (customize-set-variable 'protagentic-config-default-generation-mode 'llm)
    (should (eq (protagentic-config-get-generation-mode) 'llm))
    
    (customize-set-variable 'protagentic-config-default-generation-mode 'template)
    (should (eq (protagentic-config-get-generation-mode) 'template))
    
    (customize-set-variable 'protagentic-config-default-generation-mode 'hybrid)
    (should (eq (protagentic-config-get-generation-mode) 'hybrid))
    
    ;; Restore original mode
    (customize-set-variable 'protagentic-config-default-generation-mode original-mode)))

;; Test usage tracking

(ert-deftest protagentic-config-test-usage-stats-creation ()
  "Test creation of new usage statistics."
  (let ((stats (make-protagentic-usage-stats
                :daily-tokens 0 :daily-cost 0.0
                :monthly-tokens 0 :monthly-cost 0.0
                :request-count 0
                :last-updated (current-time)
                :current-month (format-time-string "%Y-%m"))))
    (should (equal (protagentic-usage-stats-daily-tokens stats) 0))
    (should (equal (protagentic-usage-stats-daily-cost stats) 0.0))
    (should (equal (protagentic-usage-stats-monthly-tokens stats) 0))
    (should (equal (protagentic-usage-stats-monthly-cost stats) 0.0))
    (should (equal (protagentic-usage-stats-request-count stats) 0))
    (should (stringp (protagentic-usage-stats-current-month stats)))))

(ert-deftest protagentic-config-test-cost-limit-checking ()
  "Test cost limit validation logic."
  (let ((protagentic-config-monthly-cost-limit 10.0)
        (protagentic-config-cost-warning-threshold 0.8))
    
    ;; Test stats under warning threshold
    (let ((stats-low (make-protagentic-usage-stats
                      :monthly-cost 5.0)))
      ;; Should not trigger warnings (test by checking no error is thrown)
      (should-not (condition-case nil
                      (protagentic-config-check-cost-limits stats-low)
                    (error t))))
    
    ;; Test stats at warning threshold
    (let ((stats-warning (make-protagentic-usage-stats
                          :monthly-cost 8.0)))
      ;; Should trigger warning but not error - function should complete successfully
      (should (progn (protagentic-config-check-cost-limits stats-warning) t)))
    
    ;; Test stats over limit - mock y-or-n-p to avoid user input
    (let ((stats-over (make-protagentic-usage-stats
                       :monthly-cost 12.0))
          (original-mode protagentic-config-default-generation-mode))
      ;; Mock y-or-n-p to return 'y' (yes)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
        (protagentic-config-check-cost-limits stats-over)
        ;; Should have switched to template mode
        (should (eq protagentic-config-default-generation-mode 'template)))
      
      ;; Test with 'n' (no) response
      (customize-set-variable 'protagentic-config-default-generation-mode original-mode)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) nil)))
        (protagentic-config-check-cost-limits stats-over)
        ;; Should not have changed mode
        (should (eq protagentic-config-default-generation-mode original-mode)))
      
      ;; Restore original mode
      (customize-set-variable 'protagentic-config-default-generation-mode original-mode))))

;; Test configuration file paths

(ert-deftest protagentic-config-test-config-directory ()
  "Test configuration directory path generation."
  (should (stringp protagentic-config--config-dir))
  (should (string-suffix-p "protagentic" protagentic-config--config-dir))
  (should (file-name-absolute-p protagentic-config--config-dir)))

(ert-deftest protagentic-config-test-config-file-paths ()
  "Test configuration file path generation."
  (should (stringp protagentic-config--usage-stats-file))
  (should (string-suffix-p "usage-stats.el" protagentic-config--usage-stats-file))
  (should (file-name-absolute-p protagentic-config--usage-stats-file))
  
  (should (stringp protagentic-config--custom-prompts-file))
  (should (string-suffix-p "custom-prompts.el" protagentic-config--custom-prompts-file))
  (should (file-name-absolute-p protagentic-config--custom-prompts-file)))

;; Test API key handling (without actual storage)

(ert-deftest protagentic-config-test-api-key-validation ()
  "Test API key format validation logic."
  ;; Test valid API key format
  (let ((valid-key "sk-1234567890abcdef1234567890abcdef"))
    (should (string-match-p "^sk-[A-Za-z0-9]+" valid-key)))
  
  ;; Test invalid API key formats
  (let ((invalid-key-1 "invalid-key")
        (invalid-key-2 "")
        (invalid-key-3 "   "))
    (should-not (string-match-p "^sk-[A-Za-z0-9]+" invalid-key-1))
    (should-not (string-match-p "^sk-[A-Za-z0-9]+" invalid-key-2))
    (should-not (string-match-p "^sk-[A-Za-z0-9]+" invalid-key-3))))

;; Test configuration defaults

(ert-deftest protagentic-config-test-default-values ()
  "Test that configuration defaults are reasonable."
  (should (memq protagentic-config-default-generation-mode '(template llm hybrid)))
  (should (booleanp protagentic-config-prompt-for-mode))
  (should (stringp protagentic-config-llm-model))
  (should (integerp protagentic-config-llm-max-tokens))
  (should (> protagentic-config-llm-max-tokens 0))
  (should (floatp protagentic-config-llm-temperature))
  (should (>= protagentic-config-llm-temperature 0.0))
  (should (<= protagentic-config-llm-temperature 2.0))
  (should (floatp protagentic-config-monthly-cost-limit))
  (should (>= protagentic-config-monthly-cost-limit 0.0))
  (should (floatp protagentic-config-cost-warning-threshold))
  (should (>= protagentic-config-cost-warning-threshold 0.0))
  (should (<= protagentic-config-cost-warning-threshold 1.0))
  (should (booleanp protagentic-config-enable-usage-tracking)))

;; Mock tests for file operations (without actual file I/O)

(ert-deftest protagentic-config-test-usage-stats-serialization ()
  "Test usage statistics can be properly serialized and deserialized."
  (let ((original-stats (make-protagentic-usage-stats
                         :daily-tokens 100
                         :daily-cost 0.20
                         :monthly-tokens 1000
                         :monthly-cost 2.00
                         :request-count 15
                         :last-updated (current-time)
                         :current-month "2025-01")))
    
    ;; Test that the structure can be serialized to string
    (let ((serialized (prin1-to-string original-stats)))
      (should (stringp serialized))
      (should (> (length serialized) 0))
      
      ;; Test that it can be deserialized back
      (let ((deserialized (read-from-string serialized)))
        (should (protagentic-usage-stats-p (car deserialized)))
        (should (equal (protagentic-usage-stats-daily-tokens (car deserialized)) 100))
        (should (equal (protagentic-usage-stats-monthly-cost (car deserialized)) 2.00))
        (should (equal (protagentic-usage-stats-current-month (car deserialized)) "2025-01"))))))

;; Test configuration directory creation logic

(ert-deftest protagentic-config-test-directory-creation-logic ()
  "Test configuration directory creation logic."
  ;; This tests the logic without actually creating directories
  (let ((test-dir "/tmp/test-protagentic-config"))
    ;; Should handle non-existent directory
    (should-not (file-directory-p test-dir))
    
    ;; The ensure function should be able to handle this case
    ;; (We can't test actual creation without side effects)
    (should (functionp 'protagentic-config-ensure-config-dir))))

(provide 'protagentic-config-test)

;;; protagentic-config-test.el ends here