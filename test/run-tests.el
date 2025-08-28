#!/usr/bin/env emacs --script
;;; run-tests.el --- Test runner for protagentic-executor -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Test runner script for protagentic-executor tests.
;; Can be run from command line or within Emacs.

;;; Code:

;; Add current directory to load path
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

;; Load required modules
(require 'ert)
(require 'cl-lib)

;; Mock the protagentic modules that we don't have
(provide 'protagentic-core)
(provide 'protagentic-llm)
(provide 'protagentic-config)
(provide 'protagentic-utils)

;; Define mock functions for missing dependencies
(defun protagentic--find-current-spec () nil)
(defun protagentic--read-file-content (file) nil)
(defun protagentic--detect-project-root () "/tmp/test")
(defun protagentic-llm-available-p () t)
(defun protagentic-llm-generate-content (prompt type context) nil)
(defun protagentic-spec-requirements-file (spec) (plist-get spec :requirements-file))
(defun protagentic-spec-design-file (spec) (plist-get spec :design-file))
(defun protagentic-spec-tasks-file (spec) (plist-get spec :tasks-file))

;; Load the module under test
(load-file "protagentic-executor.el")

;; Load the test file
(load-file "test/protagentic-executor-test.el")

;; Function to run tests interactively
(defun run-protagentic-executor-tests ()
  "Run all protagentic-executor tests interactively."
  (interactive)
  (let ((ert-batch-backtrace-right-margin 120))
    (ert-run-tests-interactively "protagentic-executor-test-")))

;; Function to run tests in batch mode
(defun run-protagentic-executor-tests-batch ()
  "Run all protagentic-executor tests in batch mode."
  (let ((ert-batch-backtrace-right-margin 120))
    (ert-run-tests-batch "protagentic-executor-test-")))

;; If running as script, run tests in batch mode
(when noninteractive
  (message "Running protagentic-executor tests...")
  (run-protagentic-executor-tests-batch))

(provide 'run-tests)

;;; run-tests.el ends here