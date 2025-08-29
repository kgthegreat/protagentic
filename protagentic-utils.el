;;; protagentic-utils.el --- Utility functions for Protagentic -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Utility functions for file system operations, project detection,
;; and spec directory management.

;;; Code:

(require 'cl-lib)

;; Try to load project management packages if available
(require 'project nil t)
(require 'projectile nil t)

(defun protagentic--detect-project-root ()
  "Detect the project root directory using available project management tools.
Returns the project root path or nil if not in a project."
  (cond
   ;; Try project.el first (built into Emacs 25+)
   ((and (fboundp 'project-current)
         (project-current))
    (if (fboundp 'project-root)
        (project-root (project-current))
      ;; Fallback for older project.el versions
      (car (project-roots (project-current)))))
   
   ;; Try projectile if available
   ((and (fboundp 'projectile-project-root)
         (projectile-project-p))
    (projectile-project-root))
   
   ;; Fallback: look for common project indicators
   (t (protagentic--find-project-root-fallback))))

(defun protagentic--find-project-root-fallback ()
  "Fallback method to find project root by looking for common indicators.
Searches upward from current directory for .git, package.json, etc."
  (let ((indicators '(".git" ".hg" ".svn" "package.json" "Cargo.toml" 
                      "pyproject.toml" "pom.xml" "Makefile" "README.md"))
        (current-dir (file-name-directory (or buffer-file-name default-directory))))
    (cl-loop for dir = current-dir then (file-name-directory (directory-file-name dir))
             while (and dir (not (string= dir "/")))
             when (cl-some (lambda (indicator)
                             (file-exists-p (expand-file-name indicator dir)))
                           indicators)
             return dir)))

(defun protagentic--get-spec-directory (spec-name)
  "Get the full path to the spec directory for SPEC-NAME.
Returns nil if no project root can be detected."
  (let ((project-root (protagentic--detect-project-root)))
    (when project-root
      (expand-file-name 
       (file-name-as-directory spec-name)
       (expand-file-name protagentic-spec-directory project-root)))))

(defun protagentic--ensure-spec-directory (spec-path)
  "Ensure that the spec directory at SPEC-PATH exists.
Creates the directory and any necessary parent directories.
Returns t on success, signals error on failure."
  (condition-case err
      (progn
        (make-directory spec-path t)
        t)
    (error
     (error "Failed to create spec directory %s: %s" 
            spec-path (error-message-string err)))))

(defun protagentic--create-file-with-content (file-path content)
  "Create a file at FILE-PATH with CONTENT.
Creates parent directories if necessary.
Returns t on success, signals error on failure."
  (condition-case err
      (progn
        ;; Ensure parent directory exists
        (let ((parent-dir (file-name-directory file-path)))
          (when parent-dir
            (make-directory parent-dir t)))
        
        ;; Write content to file
        (with-temp-file file-path
          (insert content))
        t)
    (error
     (error "Failed to create file %s: %s" 
            file-path (error-message-string err)))))

(defun protagentic--read-file-content (file-path)
  "Read and return the content of FILE-PATH.
Returns nil if file doesn't exist or can't be read."
  (when (file-readable-p file-path)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file-path)
          (buffer-string))
      (error nil))))

(defun protagentic--file-exists-p (file-path)
  "Check if FILE-PATH exists and is readable.
Returns t if file exists and is readable, nil otherwise."
  (and file-path
       (file-exists-p file-path)
       (file-readable-p file-path)))

(defun protagentic--get-project-specs ()
  "Get a list of all spec names in the current project.
Returns a list of spec directory names, or nil if no project root found."
  (let* ((project-root (protagentic--detect-project-root))
         (specs-dir (when project-root
                      (expand-file-name protagentic-spec-directory project-root))))
    (when (and specs-dir (file-directory-p specs-dir))
      (cl-remove-if-not 
       (lambda (name)
         (and (not (string-prefix-p "." name))
              (file-directory-p (expand-file-name name specs-dir))))
       (directory-files specs-dir)))))

(defun protagentic--sanitize-spec-name (spec-name)
  "Sanitize SPEC-NAME to be safe for use as a directory name.
Converts to lowercase, replaces spaces with hyphens, removes invalid characters."
  (let ((sanitized (downcase (string-trim spec-name))))
    ;; Replace spaces and underscores with hyphens
    (setq sanitized (replace-regexp-in-string "[ _]+" "-" sanitized))
    ;; Remove invalid characters for directory names
    (setq sanitized (replace-regexp-in-string "[^a-z0-9-]" "" sanitized))
    ;; Remove multiple consecutive hyphens
    (setq sanitized (replace-regexp-in-string "-+" "-" sanitized))
    ;; Remove leading/trailing hyphens
    (string-trim sanitized "-+" "-+")))

(defun protagentic--spec-file-path (spec-name file-type)
  "Get the full path for a spec file of FILE-TYPE for SPEC-NAME.
FILE-TYPE should be one of 'requirements', 'design', or 'tasks'."
  (let ((spec-dir (protagentic--get-spec-directory spec-name)))
    (when spec-dir
      (expand-file-name 
       (format "%s.md" (symbol-name file-type))
       spec-dir))))

(defun protagentic--validate-project-context ()
  "Validate that we're in a valid project context.
Returns project root path or provides helpful guidance on failure."
  (let ((project-root (protagentic--detect-project-root)))
    (unless project-root
      (protagentic--handle-project-root-error))
    project-root))

;; Enhanced error handling functions

(defun protagentic--safe-create-directory (directory-path)
  "Safely create DIRECTORY-PATH with comprehensive error handling.
Returns t on success, provides helpful error messages on failure."
  (condition-case err
      (progn
        (make-directory directory-path t)
        t)
    (file-already-exists
     (if (file-directory-p directory-path)
         t  ; Directory already exists, that's fine
       (error "Cannot create directory %s: a file with that name already exists" directory-path)))
    (file-error
     (let ((error-type (cadr err)))
       (cond
        ((string-match-p "permission denied\\|not permitted" (format "%s" err))
         (error "Permission denied creating directory %s. Check file permissions and try again" directory-path))
        ((string-match-p "no space\\|disk full" (format "%s" err))
         (error "Insufficient disk space to create directory %s" directory-path))
        ((string-match-p "read-only" (format "%s" err))
         (error "Cannot create directory %s: filesystem is read-only" directory-path))
        (t
         (error "Failed to create directory %s: %s" directory-path (error-message-string err))))))
    (error
     (error "Unexpected error creating directory %s: %s" directory-path (error-message-string err)))))

(defun protagentic--safe-write-file (file-path content)
  "Safely write CONTENT to FILE-PATH with comprehensive error handling.
Returns t on success, provides helpful error messages on failure."
  (condition-case err
      (progn
        ;; Ensure parent directory exists
        (let ((parent-dir (file-name-directory file-path)))
          (when parent-dir
            (protagentic--safe-create-directory parent-dir)))
        
        ;; Check if we can write to the location
        (when (file-exists-p file-path)
          (unless (file-writable-p file-path)
            (error "Cannot write to %s: file is not writable" file-path)))
        
        ;; Write content to file
        (with-temp-file file-path
          (insert content))
        
        ;; Verify the file was written correctly
        (unless (file-exists-p file-path)
          (error "File %s was not created successfully" file-path))
        
        t)
    (file-error
     (let ((error-msg (format "%s" err)))
       (cond
        ((string-match-p "permission denied\\|not permitted" error-msg)
         (error "Permission denied writing to %s. Check file permissions" file-path))
        ((string-match-p "no space\\|disk full" error-msg)
         (error "Insufficient disk space to write file %s" file-path))
        ((string-match-p "read-only" error-msg)
         (error "Cannot write to %s: filesystem is read-only" file-path))
        ((string-match-p "directory.*not exist" error-msg)
         (error "Cannot write to %s: parent directory does not exist" file-path))
        (t
         (error "Failed to write file %s: %s" file-path (error-message-string err))))))
    (error
     (error "Unexpected error writing file %s: %s" file-path (error-message-string err)))))

(defun protagentic--safe-read-file (file-path)
  "Safely read content from FILE-PATH with comprehensive error handling.
Returns file content on success, nil if file doesn't exist, signals error on other failures."
  (cond
   ((not (file-exists-p file-path))
    nil)  ; File doesn't exist, return nil
   
   ((not (file-readable-p file-path))
    (error "Cannot read %s: file is not readable (check permissions)" file-path))
   
   (t
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file-path)
          (buffer-string))
      (file-error
       (let ((error-msg (format "%s" err)))
         (cond
          ((string-match-p "permission denied\\|not permitted" error-msg)
           (error "Permission denied reading %s. Check file permissions" file-path))
          ((string-match-p "no such file" error-msg)
           nil)  ; File was deleted between existence check and read
          ((string-match-p "is a directory" error-msg)
           (error "Cannot read %s: it is a directory, not a file" file-path))
          (t
           (error "Failed to read file %s: %s" file-path (error-message-string err))))))
      (error
       (error "Unexpected error reading file %s: %s" file-path (error-message-string err)))))))

(defun protagentic--safe-delete-file (file-path)
  "Safely delete FILE-PATH with comprehensive error handling.
Returns t on success, provides helpful error messages on failure."
  (condition-case err
      (progn
        (unless (file-exists-p file-path)
          (error "Cannot delete %s: file does not exist" file-path))
        
        (delete-file file-path)
        t)
    (file-error
     (let ((error-msg (format "%s" err)))
       (cond
        ((string-match-p "permission denied\\|not permitted" error-msg)
         (error "Permission denied deleting %s. Check file permissions" file-path))
        ((string-match-p "directory not empty" error-msg)
         (error "Cannot delete %s: directory is not empty" file-path))
        ((string-match-p "read-only" error-msg)
         (error "Cannot delete %s: filesystem is read-only" file-path))
        (t
         (error "Failed to delete file %s: %s" file-path (error-message-string err))))))
    (error
     (error "Unexpected error deleting file %s: %s" file-path (error-message-string err)))))

(defun protagentic--safe-delete-directory (directory-path)
  "Safely delete DIRECTORY-PATH and its contents with comprehensive error handling.
Returns t on success, provides helpful error messages on failure."
  (condition-case err
      (progn
        (unless (file-directory-p directory-path)
          (if (file-exists-p directory-path)
              (error "Cannot delete %s as directory: it is a file" directory-path)
            (error "Cannot delete %s: directory does not exist" directory-path)))
        
        (delete-directory directory-path t)
        t)
    (file-error
     (let ((error-msg (format "%s" err)))
       (cond
        ((string-match-p "permission denied\\|not permitted" error-msg)
         (error "Permission denied deleting directory %s. Check permissions on directory and its contents" directory-path))
        ((string-match-p "directory not empty" error-msg)
         (error "Cannot delete %s: directory contains files that cannot be deleted" directory-path))
        ((string-match-p "read-only" error-msg)
         (error "Cannot delete %s: filesystem is read-only" directory-path))
        (t
         (error "Failed to delete directory %s: %s" directory-path (error-message-string err))))))
    (error
     (error "Unexpected error deleting directory %s: %s" directory-path (error-message-string err)))))

(defun protagentic--handle-project-root-error ()
  "Handle missing project root with helpful guidance.
Provides suggestions for resolving project detection issues."
  (let ((current-dir (or default-directory "unknown")))
    (with-output-to-temp-buffer "*Protagentic Help*"
      (princ "Protagentic: Project Root Not Found\n")
      (princ "===================================\n\n")
      (princ (format "Current directory: %s\n\n" current-dir))
      (princ "Protagentic requires a project context to work. Here are some solutions:\n\n")
      (princ "1. Navigate to an existing project:\n")
      (princ "   - Open a file in a Git repository\n")
      (princ "   - Open a file in a directory with package.json, Cargo.toml, etc.\n\n")
      (princ "2. Create a new project:\n")
      (princ "   - Run 'git init' in your desired project directory\n")
      (princ "   - Create a README.md or other project indicator file\n\n")
      (princ "3. Install project management tools:\n")
      (princ "   - Install and configure Projectile: M-x package-install RET projectile\n")
      (princ "   - Use built-in project.el (available in Emacs 25+)\n\n")
      (princ "4. Supported project indicators:\n")
      (princ "   - .git, .hg, .svn (version control)\n")
      (princ "   - package.json (Node.js)\n")
      (princ "   - Cargo.toml (Rust)\n")
      (princ "   - pyproject.toml (Python)\n")
      (princ "   - pom.xml (Java/Maven)\n")
      (princ "   - Makefile\n")
      (princ "   - README.md\n\n")
      (princ "Once you're in a project directory, try the Protagentic command again.\n"))
    
    (error "Not in a project directory. See *Protagentic Help* buffer for solutions")))

;; Update existing functions to use safe operations

(defun protagentic--ensure-spec-directory (spec-path)
  "Ensure that the spec directory at SPEC-PATH exists.
Uses safe directory creation with enhanced error handling."
  (protagentic--safe-create-directory spec-path))

(defun protagentic--create-file-with-content (file-path content)
  "Create a file at FILE-PATH with CONTENT.
Uses safe file writing with enhanced error handling."
  (protagentic--safe-write-file file-path content))

(defun protagentic--read-file-content (file-path)
  "Read and return the content of FILE-PATH.
Uses safe file reading with enhanced error handling."
  (protagentic--safe-read-file file-path))

(defun protagentic--read-spec-name (prompt)
  "Read spec name from user with PROMPT.
Returns spec name string or signals error if no specs available."
  (let ((available-specs (protagentic--get-project-specs)))
    (if available-specs
        (completing-read prompt available-specs nil t)
      (user-error "No specs found in current project"))))

(provide 'protagentic-utils)

;;; protagentic-utils.el ends here