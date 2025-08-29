;;; protagentic.el --- Structured feature planning for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Kumar Gaurav <kgthegreat@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools, project, planning
;; URL: https://github.com/kgthegreat/protagentic

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Protagentic provides a structured workflow for feature planning within Emacs.
;; It guides developers through three phases:
;; 1. Requirements gathering with user stories and acceptance criteria
;; 2. Design documentation with architecture and technical specifications  
;; 3. Task generation with actionable implementation steps
;;
;; The plugin integrates with existing Emacs tools and uses markdown formatting
;; for all generated documents.

;;; Code:

(require 'cl-lib)

(defgroup protagentic nil
  "Structured feature planning for Emacs."
  :group 'tools
  :prefix "protagentic-")

(defcustom protagentic-spec-directory ".protagentic/specs"
  "Directory name for storing spec files relative to project root.
This directory will be created automatically when needed."
  :type 'string
  :group 'protagentic
  :safe 'stringp)

(defcustom protagentic-auto-open-generated-files t
  "Whether to automatically open generated files after creation.
When t, newly generated requirements, design, and task files will
be opened automatically in Emacs buffers."
  :type 'boolean
  :group 'protagentic
  :safe 'booleanp)

(defcustom protagentic-default-requirements-count 3
  "Default number of requirements to include in new spec templates.
This affects the initial template generation for requirements documents."
  :type 'integer
  :group 'protagentic
  :safe 'integerp)

(defcustom protagentic-use-interactive-refinement t
  "Whether to use interactive refinement workflows by default.
When t, refinement commands will use guided prompts.
When nil, refinement will open files for manual editing."
  :type 'boolean
  :group 'protagentic
  :safe 'booleanp)

(defcustom protagentic-show-workflow-guidance t
  "Whether to show contextual workflow guidance messages.
When t, commands will display helpful next-step guidance.
When nil, only basic completion messages are shown."
  :type 'boolean
  :group 'protagentic
  :safe 'booleanp)

(defcustom protagentic-validate-prerequisites-interactively t
  "Whether to provide interactive help for workflow prerequisite errors.
When t, missing prerequisites will show helpful guidance and offer to
complete missing phases automatically.
When nil, prerequisite errors will be shown as simple error messages."
  :type 'boolean
  :group 'protagentic
  :safe 'booleanp)

(defcustom protagentic-backup-before-refinement t
  "Whether to create backups before refinement operations.
When t, original files are backed up before interactive modifications.
Backups are stored with .bak extension in the same directory."
  :type 'boolean
  :group 'protagentic
  :safe 'booleanp)

(defcustom protagentic-template-style 'comprehensive
  "Style of templates to generate for new documents.
- 'minimal: Basic structure with minimal examples
- 'comprehensive: Detailed structure with multiple examples and guidance
- 'custom: Use custom templates (requires additional configuration)"
  :type '(choice (const :tag "Minimal templates" minimal)
                 (const :tag "Comprehensive templates" comprehensive)
                 (const :tag "Custom templates" custom))
  :group 'protagentic
  :safe 'symbolp)

(defcustom protagentic-preferred-project-tools '(project projectile)
  "Preferred project management tools in order of preference.
Protagentic will try these tools in order for project root detection.
Available options: project, projectile, fallback"
  :type '(repeat (choice (const :tag "Built-in project.el" project)
                         (const :tag "Projectile" projectile)
                         (const :tag "Fallback detection" fallback)))
  :group 'protagentic
  :safe (lambda (val) (and (listp val) (cl-every 'symbolp val))))

(defcustom protagentic-file-naming-convention 'kebab-case
  "Naming convention for spec directories and files.
- 'kebab-case: use-hyphens-between-words
- 'snake_case: use_underscores_between_words
- 'camelCase: useCamelCaseNaming"
  :type '(choice (const :tag "kebab-case (recommended)" kebab-case)
                 (const :tag "snake_case" snake_case)
                 (const :tag "camelCase" camelCase))
  :group 'protagentic
  :safe 'symbolp)

(defcustom protagentic-quality-threshold 70
  "Minimum quality score (0-100) for document validation.
Documents below this threshold will show improvement suggestions.
Set to 0 to disable quality checking."
  :type 'integer
  :group 'protagentic
  :safe 'integerp)

(defcustom protagentic-auto-save-refinements t
  "Whether to automatically save refinements during interactive sessions.
When t, changes are saved immediately during refinement.
When nil, user must manually save changes."
  :type 'boolean
  :group 'protagentic
  :safe 'booleanp)

(defcustom protagentic-completion-style 'default
  "Completion style for spec selection and navigation.
- 'default: Use Emacs default completion
- 'ido: Use ido-style completion (if available)
- 'ivy: Use ivy-style completion (if available)
- 'helm: Use helm-style completion (if available)"
  :type '(choice (const :tag "Default Emacs completion" default)
                 (const :tag "Ido completion" ido)
                 (const :tag "Ivy completion" ivy)
                 (const :tag "Helm completion" helm))
  :group 'protagentic
  :safe 'symbolp)

(defcustom protagentic-status-display-format 'detailed
  "Format for status displays and progress information.
- 'minimal: Show only essential information
- 'detailed: Show comprehensive status with guidance
- 'compact: Show status in a compact format"
  :type '(choice (const :tag "Minimal display" minimal)
                 (const :tag "Detailed display" detailed)
                 (const :tag "Compact display" compact))
  :group 'protagentic
  :safe 'symbolp)

(defcustom protagentic-hook-functions nil
  "List of functions to run at various Protagentic workflow points.
Functions should accept a spec object and phase symbol as arguments.
Called at: spec creation, phase completion, workflow completion."
  :type 'hook
  :group 'protagentic)

(defcustom protagentic-external-tools-integration t
  "Whether to integrate with external development tools.
When t, Protagentic will attempt to integrate with:
- Version control systems (Git)
- Issue trackers (if configured)
- Documentation generators"
  :type 'boolean
  :group 'protagentic
  :safe 'booleanp)

;; Integration with existing Emacs packages
(defvar protagentic--markdown-mode-available nil
  "Whether markdown-mode is available for enhanced editing.")

(defvar protagentic--completion-framework nil
  "Detected completion framework (ivy, helm, ido, or default).")

(defun protagentic--detect-available-packages ()
  "Detect available packages for integration."
  ;; Check for markdown-mode
  (setq protagentic--markdown-mode-available 
        (featurep 'markdown-mode))
  
  ;; Detect completion framework
  (setq protagentic--completion-framework
        (cond
         ((and (eq protagentic-completion-style 'ivy) (featurep 'ivy)) 'ivy)
         ((and (eq protagentic-completion-style 'helm) (featurep 'helm)) 'helm)
         ((and (eq protagentic-completion-style 'ido) (featurep 'ido)) 'ido)
         (t 'default))))

(defun protagentic--setup-markdown-integration ()
  "Set up integration with markdown-mode if available."
  (when protagentic--markdown-mode-available
    ;; Add protagentic-specific markdown extensions
    (add-to-list 'auto-mode-alist '("\\.protagentic/specs/.*\\.md\\'" . markdown-mode))
    
    ;; Set up markdown-mode hooks for protagentic files
    (add-hook 'markdown-mode-hook 'protagentic--maybe-enable-spec-features)))

(defun protagentic--maybe-enable-spec-features ()
  "Enable Protagentic-specific features if in a spec file."
  (when (and buffer-file-name
             (string-match-p "\\.protagentic/specs/" buffer-file-name))
    ;; Add spec-specific key bindings
    (local-set-key (kbd "C-c C-n") 'protagentic-navigation-next-phase)
    (local-set-key (kbd "C-c C-s") 'protagentic-navigation-show-status)
    (local-set-key (kbd "C-c C-r") 'protagentic-commands-refine-requirements)
    (local-set-key (kbd "C-c C-d") 'protagentic-commands-refine-design)
    
    ;; Add spec information to mode line
    (protagentic--update-mode-line)))

(defun protagentic--update-mode-line ()
  "Update mode line with spec information."
  (let ((spec (protagentic--find-current-spec)))
    (when spec
      (let* ((status (protagentic--get-spec-status spec))
             (phase (plist-get status :current-phase))
             (progress (format "%d/3" 
                               (+ (if (plist-get status :requirements-complete) 1 0)
                                  (if (plist-get status :design-complete) 1 0)
                                  (if (plist-get status :tasks-complete) 1 0)))))
        (setq mode-line-buffer-identification
              (format "%%12b [Protagentic:%s %s]" 
                      (or phase "new") progress))))))

(defun protagentic--enhanced-completing-read (prompt choices &optional predicate require-match initial-input hist def inherit-input-method)
  "Enhanced completing-read that uses the configured completion framework."
  (pcase protagentic--completion-framework
    ('ivy
     (if (fboundp 'ivy-completing-read)
         (ivy-completing-read prompt choices predicate require-match initial-input hist def inherit-input-method)
       (completing-read prompt choices predicate require-match initial-input hist def inherit-input-method)))
    
    ('helm
     (if (fboundp 'helm-comp-read)
         (helm-comp-read prompt choices)
       (completing-read prompt choices predicate require-match initial-input hist def inherit-input-method)))
    
    ('ido
     (if (fboundp 'ido-completing-read)
         (ido-completing-read prompt choices predicate require-match initial-input hist def inherit-input-method)
       (completing-read prompt choices predicate require-match initial-input hist def inherit-input-method)))
    
    (_ (completing-read prompt choices predicate require-match initial-input hist def inherit-input-method))))

(defun protagentic--setup-project-integration ()
  "Set up integration with project management tools."
  ;; Ensure project tools are loaded in preferred order
  (dolist (tool protagentic-preferred-project-tools)
    (pcase tool
      ('project
       (when (>= emacs-major-version 25)
         (require 'project nil t)))
      ('projectile
       (require 'projectile nil t))
      ('fallback
       ;; Fallback is always available
       t)
      (_ 
       (message "Unknown project tool: %s" tool)))))

(defun protagentic--setup-version-control-integration ()
  "Set up integration with version control systems."
  (when protagentic-external-tools-integration
    ;; Add .protagentic directory to .gitignore if in a Git repo
    (when (and (executable-find "git")
               (file-directory-p ".git"))
      (protagentic--maybe-update-gitignore))))

(defun protagentic--maybe-update-gitignore ()
  "Add .protagentic to .gitignore if not already present."
  (let ((gitignore-file ".gitignore")
        (protagentic-entry ".protagentic/"))
    (when (or (not (file-exists-p gitignore-file))
              (not (with-temp-buffer
                     (insert-file-contents gitignore-file)
                     (search-forward protagentic-entry nil t))))
      (when (y-or-n-p "Add .protagentic/ to .gitignore? ")
        (with-temp-buffer
          (when (file-exists-p gitignore-file)
            (insert-file-contents gitignore-file))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "# Protagentic spec files\n")
          (insert protagentic-entry "\n")
          (write-file gitignore-file))
        (message "Added .protagentic/ to .gitignore")))))

(defun protagentic--create-backup-if-needed (file-path)
  "Create a backup of FILE-PATH if backup is enabled."
  (when (and protagentic-backup-before-refinement
             (file-exists-p file-path))
    (let ((backup-path (concat file-path ".bak")))
      (copy-file file-path backup-path t)
      (message "Created backup: %s" backup-path))))

(defun protagentic--apply-naming-convention (name)
  "Apply the configured naming convention to NAME."
  (pcase protagentic-file-naming-convention
    ('kebab-case
     (protagentic--sanitize-spec-name name))
    
    ('snake_case
     (let ((sanitized (downcase (string-trim name))))
       (setq sanitized (replace-regexp-in-string "[ -]+" "_" sanitized))
       (setq sanitized (replace-regexp-in-string "[^a-z0-9_]" "" sanitized))
       (setq sanitized (replace-regexp-in-string "_+" "_" sanitized))
       (string-trim sanitized "_")))
    
    (camelCase
     (let* ((words (split-string (downcase (string-trim name)) "[ _-]+" t))
            (first-word (car words))
            (rest-words (mapcar 'capitalize (cdr words))))
       (concat first-word (mapconcat 'identity rest-words ""))))
    
    (t (protagentic--sanitize-spec-name name))))

;; Load core submodules
(require 'protagentic-core)
(require 'protagentic-utils)
(require 'protagentic-templates)
(require 'protagentic-commands)
(require 'protagentic-navigation)
(require 'protagentic-config)
(require 'protagentic-llm)
(require 'protagentic-generator)
(require 'protagentic-prompts)

;; Note: protagentic-executor and protagentic-quality are loaded on-demand

;; Menu integration
(defvar protagentic-menu-map
  (let ((map (make-sparse-keymap "Protagentic")))
    (define-key map [protagentic-help]
      '(menu-item "Help" protagentic-help
                  :help "Show Protagentic help and commands"))
    (define-key map [separator-1] '("--"))
    (define-key map [protagentic-create-spec]
      '(menu-item "Create Spec" protagentic-create-spec
                  :help "Create a new feature specification"))
    (define-key map [protagentic-open-spec]
      '(menu-item "Open Spec" protagentic-open-spec
                  :help "Open an existing specification"))
    (define-key map [protagentic-list-specs]
      '(menu-item "List Specs" protagentic-list-specs
                  :help "List all specifications in project"))
    (define-key map [separator-2] '("--"))
    (define-key map [protagentic-generate-design]
      '(menu-item "Generate Design" protagentic-generate-design
                  :help "Generate design from requirements"))
    (define-key map [protagentic-generate-tasks]
      '(menu-item "Generate Tasks" protagentic-generate-tasks
                  :help "Generate implementation tasks"))
    
    (define-key map [protagentic-execute-next-task]
      '(menu-item "Execute Next Task" protagentic-execute-next-task
                  :help "Execute the next pending implementation task"))
    
    (define-key map [protagentic-execute-all-tasks]
      '(menu-item "Execute All Tasks" protagentic-execute-all-tasks
                  :help "Execute all pending implementation tasks"))
    
    (define-key map [protagentic-show-task-status]
      '(menu-item "Show Task Status" protagentic-show-task-status
                  :help "Show task execution status"))
    
    (define-key map [protagentic-validate-code-quality]
      '(menu-item "Validate Code Quality" protagentic-validate-code-quality
                  :help "Validate generated code quality"))
    
    (define-key map [separator-3] '("--"))
    (define-key map [protagentic-show-config]
      '(menu-item "Show Configuration" protagentic-show-config
                  :help "Display current configuration"))
    (define-key map [protagentic-setup-llm]
      '(menu-item "Setup LLM" protagentic-setup-llm
                  :help "Configure LLM integration"))
    map)
  "Keymap for Protagentic menu.")

;; Add to Tools menu
(define-key-after global-map [menu-bar tools protagentic]
  (cons "Protagentic" protagentic-menu-map)
  'games)

;; Initialize integrations
(protagentic--detect-available-packages)
(protagentic--setup-markdown-integration)
(protagentic--setup-project-integration)
(protagentic--setup-version-control-integration)

;;;###autoload
(defun protagentic-create-spec (spec-name)
  "Create a new spec with SPEC-NAME and initialize requirements phase."
  (interactive "sSpec name: ")
  (protagentic-commands-create-spec spec-name))

;;;###autoload
(defun protagentic-generate-design ()
  "Generate design document from existing requirements."
  (interactive)
  (protagentic-commands-generate-design))

;;;###autoload
(defun protagentic-generate-tasks ()
  "Generate task list from existing design document."
  (interactive)
  (protagentic-commands-generate-tasks))

;;;###autoload
(defun protagentic-open-requirements ()
  "Open requirements document for current spec."
  (interactive)
  (protagentic-navigation-open-requirements))

;;;###autoload
(defun protagentic-open-design ()
  "Open design document for current spec."
  (interactive)
  (protagentic-navigation-open-design))

;;;###autoload
(defun protagentic-open-tasks ()
  "Open tasks document for current spec."
  (interactive)
  (protagentic-navigation-open-tasks))

;;;###autoload
(defun protagentic-show-status ()
  "Show status of current spec workflow."
  (interactive)
  (protagentic-navigation-show-status))

;;;###autoload
(defun protagentic-setup-llm ()
  "Setup LLM integration for Protagentic."
  (interactive)
  (protagentic-commands-setup-llm))

;;;###autoload
(defun protagentic-show-config ()
  "Show current Protagentic configuration and status."
  (interactive)
  (protagentic-commands-show-config))

;;;###autoload
(defun protagentic-set-generation-mode ()
  "Set the default content generation mode."
  (interactive)
  (protagentic-commands-set-generation-mode))

;;;###autoload
(defun protagentic-validate-api-key ()
  "Validate the configured OpenAI API key."
  (interactive)
  (protagentic-commands-validate-api-key))

;;;###autoload
(defun protagentic-help ()
  "Show Protagentic help and command overview."
  (interactive)
  (with-help-window "*Protagentic Help*"
    (princ "Protagentic - Structured Feature Planning for Emacs\n")
    (princ "==================================================\n\n")
    
    (princ "WORKFLOW COMMANDS:\n")
    (princ "  M-x protagentic-create-spec          Create new feature spec\n")
    (princ "  M-x protagentic-generate-design      Generate design from requirements\n")
    (princ "  M-x protagentic-generate-tasks       Generate tasks from design\n\n")
    
    (princ "NAVIGATION COMMANDS:\n")
    (princ "  M-x protagentic-open-spec            Open existing spec\n")
    (princ "  M-x protagentic-list-specs           List all specs in project\n")
    (princ "  M-x protagentic-show-status          Show current spec status\n\n")
    
    (princ "REFINEMENT COMMANDS:\n")
    (princ "  M-x protagentic-refine-requirements  Improve requirements interactively\n")
    (princ "  M-x protagentic-refine-design        Improve design interactively\n")
    (princ "  M-x protagentic-refine-tasks         Improve tasks interactively\n\n")
    
    (princ "CONFIGURATION COMMANDS:\n")
    (princ "  M-x protagentic-setup-llm            Configure LLM integration\n")
    (princ "  M-x protagentic-show-config          Display current configuration\n")
    (princ "  M-x protagentic-set-generation-mode  Set default generation mode\n")
    (princ "  M-x protagentic-validate-api-key     Test API connectivity\n\n")
    
    (princ "SUGGESTED KEYBINDINGS:\n")
    (princ "Add these to your Emacs configuration:\n\n")
    (princ "  (global-set-key (kbd \"C-c p c\") 'protagentic-create-spec)\n")
    (princ "  (global-set-key (kbd \"C-c p o\") 'protagentic-open-spec)\n")
    (princ "  (global-set-key (kbd \"C-c p l\") 'protagentic-list-specs)\n")
    (princ "  (global-set-key (kbd \"C-c p s\") 'protagentic-show-status)\n")
    (princ "  (global-set-key (kbd \"C-c p d\") 'protagentic-generate-design)\n")
    (princ "  (global-set-key (kbd \"C-c p t\") 'protagentic-generate-tasks)\n")
    (princ "  (global-set-key (kbd \"C-c p h\") 'protagentic-help)\n\n")
    
    (princ "QUICK START:\n")
    (princ "1. Run M-x protagentic-create-spec to create your first spec\n")
    (princ "2. Describe your feature when prompted\n")
    (princ "3. Review and refine the generated requirements\n")
    (princ "4. Run M-x protagentic-generate-design to create technical design\n")
    (princ "5. Run M-x protagentic-generate-tasks to create implementation plan\n\n")
    
    (princ "DOCUMENTATION:\n")
    (princ "  README.md     - Installation and basic usage\n")
    (princ "  EXAMPLES.md   - Detailed examples and workflows\n")
    (princ "  LLM-SETUP.md  - LLM configuration guide\n\n")
    
    (princ "For more information, visit:\n")
    (princ "https://github.com/kgthegreat/protagentic\n")))

;;;###autoload
(defun protagentic-regenerate-requirements ()
  "Regenerate requirements for the current spec with mode selection."
  (interactive)
  (protagentic-commands-regenerate-requirements))

;;;###autoload
(defun protagentic-regenerate-design ()
  "Regenerate design document for the current spec with mode selection."
  (interactive)
  (protagentic-commands-regenerate-design))

;;;###autoload
(defun protagentic-regenerate-tasks ()
  "Regenerate tasks document for the current spec with mode selection."
  (interactive)
  (protagentic-commands-regenerate-tasks))

;; New commands are defined in protagentic-commands.el and autoloaded from there

(provide 'protagentic)

;;; protagentic.el ends here