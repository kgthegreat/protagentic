;;; protagentic-quality.el --- Code quality guidelines and validation -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Code quality guidelines, validation, and enforcement for Protagentic.
;; Ensures generated code follows best practices and maintainability standards.

;;; Code:

(require 'cl-lib)

(defgroup protagentic-quality nil
  "Code quality settings for Protagentic."
  :group 'protagentic
  :prefix "protagentic-quality-")

;; Quality standards configuration

(defcustom protagentic-quality-max-function-lines 30
  "Maximum lines allowed in a single function."
  :type 'integer
  :group 'protagentic-quality)

(defcustom protagentic-quality-max-complexity 10
  "Maximum cyclomatic complexity allowed."
  :type 'integer
  :group 'protagentic-quality)

(defcustom protagentic-quality-min-test-coverage 90
  "Minimum test coverage percentage required."
  :type 'integer
  :group 'protagentic-quality)

(defcustom protagentic-quality-enforce-naming-conventions t
  "Whether to enforce naming conventions."
  :type 'boolean
  :group 'protagentic-quality)

;; Language-specific quality rules

(defconst protagentic-quality--language-rules
  '((javascript . ((naming . "camelCase")
                   (test-suffix . ".test.js")
                   (max-params . 4)
                   (requires-jsdoc . t)))
    (python . ((naming . "snake_case")
               (test-prefix . "test_")
               (max-params . 5)
               (requires-docstring . t)))
    (java . ((naming . "camelCase")
             (test-suffix . "Test.java")
             (max-params . 4)
             (requires-javadoc . t)))
    (elisp . ((naming . "kebab-case")
              (test-suffix . "-test.el")
              (max-params . 6)
              (requires-docstring . t))))
  "Language-specific quality rules.")

;; Quality validation framework

(defun protagentic-quality-validate-code (content filename)
  "Validate code CONTENT in FILENAME against quality standards.
Returns validation result with errors and suggestions."
  (let* ((language (protagentic-quality--detect-language filename))
         (rules (protagentic-quality--get-language-rules language))
         (errors '())
         (warnings '())
         (suggestions '()))
    
    ;; Core quality checks
    (let ((function-issues (protagentic-quality--check-function-quality content language)))
      (setq errors (append errors (plist-get function-issues :errors)))
      (setq warnings (append warnings (plist-get function-issues :warnings))))
    
    (let ((naming-issues (protagentic-quality--check-naming-conventions content language rules)))
      (setq warnings (append warnings naming-issues)))
    
    (let ((complexity-issues (protagentic-quality--check-complexity content language)))
      (setq errors (append errors complexity-issues)))
    
    (let ((documentation-issues (protagentic-quality--check-documentation content language rules)))
      (setq warnings (append warnings documentation-issues)))
    
    (let ((security-issues (protagentic-quality--check-security content language)))
      (setq errors (append errors security-issues)))
    
    ;; Generate improvement suggestions
    (setq suggestions (protagentic-quality--generate-suggestions content language))
    
    (list :errors errors
          :warnings warnings
          :suggestions suggestions
          :score (protagentic-quality--calculate-quality-score errors warnings)
          :language language)))

;; Function quality analysis

(defun protagentic-quality--check-function-quality (content language)
  "Check function quality in CONTENT for LANGUAGE.
Returns plist with :errors and :warnings."
  (let ((errors '())
        (warnings '())
        (functions (protagentic-quality--extract-functions content language)))
    
    (dolist (func functions)
      (let ((name (plist-get func :name))
            (lines (plist-get func :lines))
            (params (plist-get func :params)))
        
        ;; Check function length
        (when (> lines protagentic-quality-max-function-lines)
          (push (format "Function '%s' is too long (%d lines, max %d)" 
                        name lines protagentic-quality-max-function-lines)
                errors))
        
        ;; Check parameter count
        (let ((max-params (protagentic-quality--get-max-params language)))
          (when (> params max-params)
            (push (format "Function '%s' has too many parameters (%d, max %d)"
                          name params max-params)
                  warnings)))
        
        ;; Check function naming
        (unless (protagentic-quality--valid-function-name-p name language)
          (push (format "Function '%s' doesn't follow naming conventions" name)
                warnings))))
    
    (list :errors errors :warnings warnings)))

;; Complexity analysis

(defun protagentic-quality--check-complexity (content language)
  "Check cyclomatic complexity in CONTENT for LANGUAGE.
Returns list of complexity errors."
  (let ((errors '())
        (functions (protagentic-quality--extract-functions content language)))
    
    (dolist (func functions)
      (let ((name (plist-get func :name))
            (complexity (protagentic-quality--calculate-complexity 
                        (plist-get func :body) language)))
        (when (> complexity protagentic-quality-max-complexity)
          (push (format "Function '%s' is too complex (complexity %d, max %d)"
                        name complexity protagentic-quality-max-complexity)
                errors))))
    
    errors))

(defun protagentic-quality--calculate-complexity (function-body language)
  "Calculate cyclomatic complexity of FUNCTION-BODY for LANGUAGE.
Returns complexity score."
  (let ((complexity 1)) ; Base complexity
    
    ;; Count decision points based on language
    (pcase language
      ('javascript
       (setq complexity (+ complexity
                          (protagentic-quality--count-patterns 
                           function-body '("if" "else if" "while" "for" "switch" "case" "catch" "&&" "||" "?")))))
      ('python
       (setq complexity (+ complexity
                          (protagentic-quality--count-patterns 
                           function-body '("if" "elif" "while" "for" "except" "and" "or")))))
      ('java
       (setq complexity (+ complexity
                          (protagentic-quality--count-patterns 
                           function-body '("if" "else if" "while" "for" "switch" "case" "catch" "&&" "||" "?")))))
      ('elisp
       (setq complexity (+ complexity
                          (protagentic-quality--count-patterns 
                           function-body '("if" "when" "unless" "cond" "while" "dolist" "dotimes" "and" "or"))))))
    
    complexity))

;; Security checks

(defun protagentic-quality--check-security (content language)
  "Check for security issues in CONTENT for LANGUAGE.
Returns list of security errors."
  (let ((errors '()))
    
    ;; SQL injection checks
    (when (string-match-p "\\+.*['\"].*SELECT\\|\\+.*['\"].*INSERT\\|\\+.*['\"].*UPDATE" content)
      (push "Potential SQL injection vulnerability - use parameterized queries" errors))
    
    ;; XSS checks for web languages
    (when (memq language '(javascript java))
      (when (string-match-p "innerHTML\\s*=\\|document\\.write" content)
        (push "Potential XSS vulnerability - sanitize user input" errors)))
    
    ;; Hardcoded secrets
    (when (string-match-p "password\\s*=\\s*['\"][^'\"]+['\"]\\|api[_-]?key\\s*=\\s*['\"][^'\"]+['\"]" content)
      (push "Hardcoded credentials detected - use environment variables" errors))
    
    ;; Unsafe file operations
    (when (string-match-p "\\.\\./" content)
      (push "Path traversal vulnerability - validate file paths" errors))
    
    errors))

;; Documentation checks

(defun protagentic-quality--check-documentation (content language rules)
  "Check documentation quality in CONTENT for LANGUAGE with RULES.
Returns list of documentation warnings."
  (let ((warnings '())
        (functions (protagentic-quality--extract-functions content language)))
    
    (when (plist-get rules 'requires-docstring)
      (dolist (func functions)
        (let ((name (plist-get func :name))
              (has-doc (plist-get func :has-documentation)))
          (unless has-doc
            (push (format "Function '%s' missing documentation" name) warnings)))))
    
    warnings))

;; Naming convention validation

(defun protagentic-quality--check-naming-conventions (content language rules)
  "Check naming conventions in CONTENT for LANGUAGE with RULES.
Returns list of naming warnings."
  (let ((warnings '())
        (naming-style (plist-get rules 'naming)))
    
    (when protagentic-quality-enforce-naming-conventions
      ;; Check variable names
      (let ((variables (protagentic-quality--extract-variables content language)))
        (dolist (var variables)
          (unless (protagentic-quality--valid-name-p var naming-style)
            (push (format "Variable '%s' doesn't follow %s convention" var naming-style)
                  warnings))))
      
      ;; Check function names
      (let ((functions (protagentic-quality--extract-functions content language)))
        (dolist (func functions)
          (let ((name (plist-get func :name)))
            (unless (protagentic-quality--valid-name-p name naming-style)
              (push (format "Function '%s' doesn't follow %s convention" name naming-style)
                    warnings))))))
    
    warnings))

;; Helper functions for quality analysis

(defun protagentic-quality--find-long-functions (content language)
  "Find functions longer than allowed in CONTENT for LANGUAGE.
Returns list of long function names."
  (let ((functions (protagentic-quality--extract-functions content language))
        (long-functions '()))
    (dolist (func functions)
      (when (> (plist-get func :lines) protagentic-quality-max-function-lines)
        (push (plist-get func :name) long-functions)))
    long-functions))

(defun protagentic-quality--has-comprehensive-error-handling-p (content language)
  "Check if CONTENT has comprehensive error handling for LANGUAGE."
  (pcase language
    ('javascript (and (string-match-p "try\\s-*{" content)
                      (string-match-p "catch\\s-*(" content)))
    ('python (and (string-match-p "try:" content)
                  (string-match-p "except" content)))
    ('java (and (string-match-p "try\\s-*{" content)
                (string-match-p "catch\\s-*(" content)))
    ('elisp (string-match-p "condition-case" content))
    (_ nil)))

(defun protagentic-quality--has-performance-issues-p (content language)
  "Check if CONTENT has potential performance issues for LANGUAGE."
  ;; Simple heuristics for performance issues
  (or (string-match-p "for.*for.*for" content) ; Nested loops
      (string-match-p "while.*while" content)   ; Nested while loops
      (string-match-p "\\.length" content)))    ; Repeated length calls

(defun protagentic-quality--has-edge-case-tests-p (content language)
  "Check if CONTENT appears to have edge case tests for LANGUAGE."
  (and (string-match-p "test\\|it\\|describe" content)
       (or (string-match-p "null\\|undefined\\|empty" content)
           (string-match-p "error\\|exception\\|throw" content))))

;; Quality improvement suggestions

(defun protagentic-quality--generate-suggestions (content language)
  "Generate improvement suggestions for CONTENT in LANGUAGE.
Returns list of actionable suggestions."
  (let ((suggestions '()))
    
    ;; Suggest refactoring for long functions
    (let ((long-functions (protagentic-quality--find-long-functions content language)))
      (when long-functions
        (push "Consider breaking down long functions into smaller, focused functions" suggestions)))
    
    ;; Suggest error handling improvements
    (unless (protagentic-quality--has-comprehensive-error-handling-p content language)
      (push "Add comprehensive error handling and input validation" suggestions))
    
    ;; Suggest performance improvements
    (when (protagentic-quality--has-performance-issues-p content language)
      (push "Consider optimizing loops and data structures for better performance" suggestions))
    
    ;; Suggest test improvements
    (unless (protagentic-quality--has-edge-case-tests-p content language)
      (push "Add tests for edge cases and error conditions" suggestions))
    
    suggestions))

;; Language detection and rules

(defun protagentic-quality--detect-language (filename)
  "Detect programming language from FILENAME.
Returns language symbol."
  (let ((extension (file-name-extension filename)))
    (cond
     ((member extension '("js" "jsx" "ts" "tsx")) 'javascript)
     ((member extension '("py" "pyw")) 'python)
     ((member extension '("java")) 'java)
     ((member extension '("el")) 'elisp)
     ((member extension '("c" "cpp" "cc" "cxx")) 'cpp)
     ((member extension '("rs")) 'rust)
     ((member extension '("go")) 'go)
     (t 'unknown))))

(defun protagentic-quality--get-language-rules (language)
  "Get quality rules for LANGUAGE."
  (cdr (assq language protagentic-quality--language-rules)))

;; Code analysis helpers

(defun protagentic-quality--extract-variables (content language)
  "Extract variable names from CONTENT for LANGUAGE.
Returns list of variable names."
  (let ((variables '()))
    (pcase language
      ('javascript
       (with-temp-buffer
         (insert content)
         (goto-char (point-min))
         ;; Match var, let, const declarations
         (while (re-search-forward "\\b\\(var\\|let\\|const\\)\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" nil t)
           (push (match-string 2) variables))))
      ('python
       (with-temp-buffer
         (insert content)
         (goto-char (point-min))
         ;; Match variable assignments
         (while (re-search-forward "^\\s-*\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*=" nil t)
           (push (match-string 1) variables))))
      ('java
       (with-temp-buffer
         (insert content)
         (goto-char (point-min))
         ;; Match field declarations and local variables
         (while (re-search-forward "\\b\\(int\\|String\\|boolean\\|double\\|float\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" nil t)
           (push (match-string 2) variables))))
      ('elisp
       (with-temp-buffer
         (insert content)
         (goto-char (point-min))
         ;; Match let bindings and defvar
         (while (re-search-forward "\\b\\(let\\|defvar\\|defcustom\\)\\s-*(?\\s-*\\([a-zA-Z-][a-zA-Z0-9-]*\\)" nil t)
           (push (match-string 2) variables)))))
    (reverse variables)))

(defun protagentic-quality--extract-functions (content language)
  "Extract function information from CONTENT for LANGUAGE.
Returns list of function plists with :name, :lines, :params, :body, :has-documentation."
  (let ((functions '()))
    (pcase language
      ('javascript (protagentic-quality--extract-js-functions content))
      ('python (protagentic-quality--extract-python-functions content))
      ('java (protagentic-quality--extract-java-functions content))
      ('elisp (protagentic-quality--extract-elisp-functions content))
      (_ '()))))

(defun protagentic-quality--extract-js-functions (content)
  "Extract JavaScript functions from CONTENT."
  (let ((functions '()))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      
      ;; Match function declarations and expressions
      (while (re-search-forward "\\(function\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)\\|\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)\\s-*=\\s-*function\\|\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)\\s-*:\\s-*function\\)" nil t)
        (let* ((name (or (match-string 2) (match-string 3) (match-string 4)))
               (start-line (line-number-at-pos))
               (params (protagentic-quality--count-js-params))
               (body-start (point))
               (body-end (protagentic-quality--find-js-function-end))
               (end-line (line-number-at-pos body-end))
               (body (buffer-substring-no-properties body-start body-end))
               (has-doc (protagentic-quality--has-js-documentation-p)))
          
          (push (list :name name
                      :lines (- end-line start-line)
                      :params params
                      :body body
                      :has-documentation has-doc)
                functions))))
    
    functions))

;; Quality scoring

(defun protagentic-quality--calculate-quality-score (errors warnings)
  "Calculate quality score based on ERRORS and WARNINGS.
Returns score from 0-100."
  (let ((error-penalty (* (length errors) 10))
        (warning-penalty (* (length warnings) 2)))
    (max 0 (- 100 error-penalty warning-penalty))))

;; Validation helpers

(defun protagentic-quality--valid-name-p (name style)
  "Check if NAME follows STYLE convention."
  (pcase style
    ("camelCase" (string-match-p "^[a-z][a-zA-Z0-9]*$" name))
    ("snake_case" (string-match-p "^[a-z][a-z0-9_]*$" name))
    ("kebab-case" (string-match-p "^[a-z][a-z0-9-]*$" name))
    ("PascalCase" (string-match-p "^[A-Z][a-zA-Z0-9]*$" name))
    (_ t)))

(defun protagentic-quality--count-patterns (text patterns)
  "Count occurrences of PATTERNS in TEXT."
  (let ((count 0))
    (dolist (pattern patterns)
      (let ((case-fold-search nil))
        (setq count (+ count (how-many (regexp-quote pattern) text)))))
    count))

;; Public API

(defun protagentic-quality-check-file (filename)
  "Check quality of file at FILENAME.
Returns validation result."
  (when (file-exists-p filename)
    (let ((content (with-temp-buffer
                     (insert-file-contents filename)
                     (buffer-string))))
      (protagentic-quality-validate-code content filename))))

(defun protagentic-quality-get-guidelines (language)
  "Get quality guidelines for LANGUAGE.
Returns formatted guidelines string."
  (let ((rules (protagentic-quality--get-language-rules language)))
    (format "Quality Guidelines for %s:

1. NAMING CONVENTIONS:
   - Use %s for variables and functions
   - Use descriptive, intention-revealing names
   - Avoid abbreviations and single-letter variables

2. FUNCTION DESIGN:
   - Maximum %d lines per function
   - Maximum %d parameters per function
   - Single responsibility principle
   - Pure functions when possible

3. COMPLEXITY:
   - Maximum cyclomatic complexity: %d
   - Avoid deep nesting (max 3 levels)
   - Use early returns to reduce complexity

4. DOCUMENTATION:
   - %s
   - Document complex algorithms
   - Include usage examples

5. ERROR HANDLING:
   - Validate all inputs
   - Use appropriate exception types
   - Provide meaningful error messages
   - Handle edge cases gracefully

6. TESTING:
   - Minimum %d%% test coverage
   - Test edge cases and error conditions
   - Use descriptive test names
   - Follow AAA pattern (Arrange, Act, Assert)"
            (capitalize (symbol-name language))
            (or (plist-get rules 'naming) "descriptive names")
            protagentic-quality-max-function-lines
            (protagentic-quality--get-max-params language)
            protagentic-quality-max-complexity
            (if (plist-get rules 'requires-docstring) 
                "Document all public functions" 
                "Document complex functions")
            protagentic-quality-min-test-coverage)))

(provide 'protagentic-quality)

;;; protagentic-quality.el ends here