# Protagentic

**Structured Feature Planning for Emacs**

Protagentic is an Emacs plugin that guides developers through systematic feature planning using a three-phase workflow: requirements gathering, design documentation, and implementation task generation. Transform rough ideas into actionable development plans without leaving your editor.

## Features

- 📝 **Requirements Phase**: Generate user stories and acceptance criteria in EARS format
- 🏗️ **Design Phase**: Create technical architecture and component specifications  
- ✅ **Tasks Phase**: Generate hierarchical implementation checklists with requirement traceability
- 🤖 **LLM Integration**: OpenAI-powered content generation with intelligent context awareness
- 🔄 **Hybrid Generation**: Choose between fast templates or high-quality LLM content with automatic fallback
- 💰 **Cost Management**: Built-in usage tracking, cost estimation, and spending limits
- 🔄 **Interactive Refinement**: Guided improvement workflows for each phase
- 📊 **Progress Tracking**: Visual status indicators and contextual guidance
- 🛠️ **Emacs Integration**: Seamless integration with markdown-mode, projectile, and completion frameworks

## Quick Start

### Installation

```elisp
;; Add to your Emacs configuration
(add-to-list 'load-path "/path/to/protagentic")
(require 'protagentic)

;; Optional: Add convenient keybindings
(global-set-key (kbd "C-c p c") 'protagentic-create-spec)
(global-set-key (kbd "C-c p o") 'protagentic-open-spec)
(global-set-key (kbd "C-c p l") 'protagentic-list-specs)
(global-set-key (kbd "C-c p s") 'protagentic-show-status)
(global-set-key (kbd "C-c p d") 'protagentic-generate-design)
(global-set-key (kbd "C-c p t") 'protagentic-generate-tasks)
(global-set-key (kbd "C-c p h") 'protagentic-help)
```

### Basic Workflow

1. **Create a Spec**: `M-x protagentic-create-spec`
   - Enter a feature name (e.g., "user-authentication")
   - Describe what the feature should do
   - Review and refine the generated requirements

2. **Generate Design**: `M-x protagentic-generate-design`
   - Automatically creates technical architecture from requirements
   - Review and enhance components, data models, and error handling

3. **Generate Tasks**: `M-x protagentic-generate-tasks`
   - Creates actionable implementation checklist
   - Tasks reference specific requirements for traceability

### Example Session

```
M-x protagentic-create-spec RET
Spec name: user-authentication RET
Describe the 'user-authentication' feature: A secure login system with registration and password reset RET

M-x protagentic-generate-design RET
M-x protagentic-generate-tasks RET
```

This creates a complete spec in `.protagentic/specs/user-authentication/` with:
- `requirements.md` - User stories and acceptance criteria
- `design.md` - Technical architecture and components
- `tasks.md` - Implementation checklist

## Commands

### Core Workflow
- `protagentic-create-spec` - Create new spec with requirements
- `protagentic-generate-design` - Generate design from requirements  
- `protagentic-generate-tasks` - Generate tasks from design

### Navigation
- `protagentic-open-requirements` - Open requirements document
- `protagentic-open-design` - Open design document
- `protagentic-open-tasks` - Open tasks document
- `protagentic-show-status` - Display spec progress and guidance

### Management
- `protagentic-list-specs` - List all specs with status
- `protagentic-delete-spec` - Delete spec and all files
- `protagentic-refine-requirements` - Interactive requirements improvement
- `protagentic-refine-design` - Interactive design enhancement

### LLM Integration
- `protagentic-setup-llm` - Configure OpenAI API integration
- `protagentic-show-config` - Display current configuration and usage stats
- `protagentic-set-generation-mode` - Set default generation mode (template/LLM/hybrid)
- `protagentic-validate-api-key` - Test API connectivity
- `protagentic-help` - Show command overview and keybinding suggestions
- `protagentic-regenerate-requirements` - Regenerate with mode selection
- `protagentic-regenerate-design` - Regenerate design with mode selection
- `protagentic-regenerate-tasks` - Regenerate tasks with mode selection

### Key Bindings (in spec files)
- `C-c C-n` - Next phase in workflow
- `C-c C-s` - Show status
- `C-c C-r` - Refine requirements
- `C-c C-d` - Refine design

## Configuration

### Basic Settings

```elisp
;; Customize spec directory location
(setq protagentic-spec-directory ".specs")

;; Disable auto-opening of generated files
(setq protagentic-auto-open-generated-files nil)

;; Use minimal templates instead of comprehensive ones
(setq protagentic-template-style 'minimal)

;; Set quality threshold for document validation
(setq protagentic-quality-threshold 80)
```

### Integration Settings

```elisp
;; Preferred project management tools (in order)
(setq protagentic-preferred-project-tools '(projectile project fallback))

;; Completion framework preference
(setq protagentic-completion-style 'ivy)  ; or 'helm, 'ido, 'default

;; File naming convention
(setq protagentic-file-naming-convention 'snake_case)  ; or 'camelCase, 'kebab-case
```

### Workflow Behavior

```elisp
;; Disable interactive refinement (use manual editing)
(setq protagentic-use-interactive-refinement nil)

;; Disable workflow guidance messages
(setq protagentic-show-workflow-guidance nil)

;; Disable automatic backups before refinement
(setq protagentic-backup-before-refinement nil)
```

### LLM Integration Setup

#### Quick Setup
```elisp
;; Run the interactive setup wizard
M-x protagentic-setup-llm
```

#### Manual Configuration
```elisp
;; Set default generation mode
(setq protagentic-config-default-generation-mode 'hybrid)  ; 'template, 'llm, or 'hybrid

;; Configure LLM settings
(setq protagentic-config-llm-model "gpt-4")               ; or "gpt-3.5-turbo"
(setq protagentic-config-llm-max-tokens 4000)
(setq protagentic-config-llm-temperature 0.7)

;; Cost management
(setq protagentic-config-monthly-cost-limit 50.0)         ; USD limit
(setq protagentic-config-cost-warning-threshold 0.8)      ; Warn at 80%
(setq protagentic-config-enable-usage-tracking t)

;; Prompt behavior
(setq protagentic-config-prompt-for-mode t)               ; Ask for mode each time
```

#### API Key Setup
```elisp
;; Option 1: Environment variable (recommended)
export OPENAI_API_KEY="sk-your-api-key-here"

;; Option 2: Interactive setup (stores securely)
M-x protagentic-setup-llm

;; Option 3: Manual secure storage
M-x protagentic-config-set-api-key
```

## Document Structure

### Requirements Document
```markdown
# Requirements Document

## Introduction
[Feature description and context]

## Requirements

### Requirement 1
**User Story:** As a [role], I want [feature], so that [benefit]

#### Acceptance Criteria
1. WHEN [event] THEN system SHALL [response]
2. IF [condition] THEN system SHALL [behavior]
```

### Design Document
```markdown
# Design Document

## Overview
[System overview and approach]

## Architecture
[System architecture and technology stack]

## Components and Interfaces
[Component breakdown and responsibilities]

## Data Models
[Data structures and relationships]

## Error Handling
[Error scenarios and handling strategies]

## Testing Strategy
[Testing approach and coverage]
```

### Tasks Document
```markdown
# Implementation Plan

- [ ] 1. Set up project structure
  - Create directory structure and dependencies
  - _Requirements: 1.1-1.3_

- [ ] 2. Implement core data models
  - [ ] 2.1 Create User model with validation
    - Write User class with validation methods
    - _Requirements: 2.1, 2.2_
```

## Examples

### E-commerce Product Catalog

**Requirements Phase:**
```markdown
### Requirement 1
**User Story:** As a customer, I want to browse products by category, so that I can find items I'm interested in

#### Acceptance Criteria
1. WHEN customer selects a category THEN system SHALL display all products in that category
2. WHEN category has no products THEN system SHALL display "No products found" message
3. WHEN customer applies filters THEN system SHALL update results in real-time
```

**Design Phase:**
```markdown
#### Product Catalog Component
Manages product display, filtering, and search functionality

**Key Responsibilities:**
- Product listing and pagination
- Category-based filtering
- Search query processing
- Sort order management

**Interfaces:**
- ProductService for data retrieval
- FilterService for search criteria
- UIComponent for display rendering
```

**Tasks Phase:**
```markdown
- [ ] 1. Implement Product data model
  - Create Product class with validation
  - Add category relationships and indexing
  - Write unit tests for Product operations
  - _Requirements: 1.1, 1.2_

- [ ] 2. Build ProductCatalog component
  - [ ] 2.1 Create product listing functionality
    - Implement pagination and sorting
    - Add category filtering logic
    - _Requirements: 1.1, 1.3_
```

### API Rate Limiting System

**Requirements Phase:**
```markdown
### Requirement 1
**User Story:** As an API provider, I want to limit request rates per user, so that I can prevent abuse and ensure fair usage

#### Acceptance Criteria
1. WHEN user exceeds rate limit THEN system SHALL return 429 status code
2. WHEN user is within limits THEN system SHALL process request normally
3. WHEN rate limit resets THEN system SHALL allow new requests
```

**Generated Tasks:**
```markdown
- [ ] 1. Implement rate limiting middleware
  - Create RateLimiter class with configurable limits
  - Add Redis integration for distributed rate limiting
  - Write comprehensive unit tests
  - _Requirements: 1.1-1.3_

- [ ] 2. Add rate limit headers to responses
  - Include X-RateLimit-Remaining header
  - Add X-RateLimit-Reset timestamp
  - _Requirements: 1.2_
```

## LLM-Powered Generation

### Generation Modes

**Template Mode** (Fast, Offline)
- Uses built-in templates with pattern matching
- Works offline, no API costs
- Consistent structure, basic content

**LLM Mode** (High Quality)
- Uses OpenAI API for intelligent content generation
- Context-aware prompts based on your project
- Higher quality, more comprehensive content
- Requires API key and internet connection

**Hybrid Mode** (Recommended)
- Attempts LLM generation first
- Automatically falls back to templates if LLM fails
- Best of both worlds with reliability

### Usage Examples

#### Basic LLM Generation
```
M-x protagentic-create-spec RET
Spec name: payment-processing RET
Generation mode: llm RET
Describe the feature: Secure payment processing with multiple payment methods and fraud detection RET
```

The LLM will generate comprehensive requirements including:
- Multiple user stories for different payment scenarios
- Detailed acceptance criteria with edge cases
- Security and compliance considerations
- Error handling requirements

#### Context-Aware Generation

Protagentic analyzes your project to provide better prompts:

```elisp
;; In a Python project with these files:
;; - app.py, requirements.txt, models.py
;; 
;; LLM prompts will include:
;; - "Detected Python project with Flask/Django patterns"
;; - "Consider existing models.py structure"
;; - "Follow Python naming conventions"
```

#### Cost Management

```elisp
;; Check usage and costs
M-x protagentic-show-config

;; Output:
;; Current Usage:
;;   This Month: $12.50 (2,500 tokens)
;;   Today: $2.30 (460 tokens)
;;   Total Requests: 15
;;
;; Cost Management:
;;   Monthly Limit: $50.00
;;   Warning Threshold: 80%
```

### LLM Prompt Customization

#### Built-in Context Enhancement

Protagentic automatically enhances prompts with:

- **Project Analysis**: Detects technology stack from files
- **Complexity Assessment**: Adjusts suggestions based on project size
- **Existing Context**: Uses previous requirements/design for consistency

#### Custom Prompt Templates

```elisp
;; Enable custom prompts
(setq protagentic-prompts-use-custom-templates t)

;; Create custom prompt (advanced)
(defun my-custom-requirements-prompt (context)
  "Custom requirements prompt with company-specific guidelines."
  (format "Generate requirements following our company standards...
Project context: %s
Technology stack: %s"
          (protagentic-generation-context-feature-description context)
          (protagentic-prompts--infer-technology-stack 
           (protagentic-generation-context-project-files context))))
```

### Quality and Validation

#### Automatic Content Improvement

LLM responses are automatically:
- **Formatted**: Fixed heading hierarchy, consistent structure
- **Enhanced**: EARS format for acceptance criteria, proper user story format
- **Validated**: Checked against document requirements
- **Repaired**: Common formatting issues automatically corrected

#### Example Improvements

**Raw LLM Output:**
```
Here's your requirements document:
```markdown
### Requirements Document
user story: as a user i want login
acceptance criteria:
when user clicks login then system should validate
```

**After Processing:**
```markdown
# Requirements Document

## Introduction
[Generated introduction]

## Requirements

### Requirement 1
**User Story:** As a user, I want to log in, so that I can access my account

#### Acceptance Criteria
1. WHEN user clicks login THEN system SHALL validate credentials
2. IF credentials are invalid THEN system SHALL display error message
```

## Advanced Usage

### Custom Templates

Create custom templates by setting `protagentic-template-style` to `'custom` and defining template functions:

```elisp
(setq protagentic-template-style 'custom)

(defun my-custom-requirements-template (feature-description)
  "Custom requirements template."
  (format "# Custom Requirements\n\n%s\n\n## User Stories\n..." feature-description))
```

### Workflow Hooks

Add custom functions to run at workflow milestones:

```elisp
(add-hook 'protagentic-hook-functions 
          (lambda (spec phase)
            (message "Completed %s phase for %s" phase (protagentic-spec-name spec))))
```

### Integration with External Tools

```elisp
;; Enable external tool integration
(setq protagentic-external-tools-integration t)

;; This enables:
;; - Automatic .gitignore updates
;; - Integration with issue trackers (if configured)
;; - Documentation generator hooks
```

## Troubleshooting

### Common Issues

**"Not in a project directory" error:**
- Ensure you're in a directory with `.git`, `package.json`, or other project indicators
- Install projectile: `M-x package-install RET projectile RET`
- Create a project marker: `touch README.md` or `git init`

**"Permission denied" errors:**
- Check file permissions in your project directory
- Ensure `.protagentic` directory is writable
- On Windows, run Emacs as administrator if needed

**Templates not generating correctly:**
- Check `protagentic-template-style` setting
- Verify all required modules are loaded
- Try `M-x protagentic-show-status` for diagnostic information

### LLM Integration Issues

**"No API key configured" error:**
```elisp
;; Check API key setup
M-x protagentic-validate-api-key

;; If not configured, run setup
M-x protagentic-setup-llm

;; Or set environment variable
export OPENAI_API_KEY="sk-your-key-here"
```

**"API validation failed" error:**
- Verify API key is correct and active
- Check internet connection
- Ensure you have OpenAI API credits
- Try a different model: `(setq protagentic-config-llm-model "gpt-3.5-turbo")`

**"LLM generation failed" with automatic fallback:**
- This is normal behavior - templates are used as backup
- Check `M-x protagentic-show-config` for error details
- Verify monthly cost limits haven't been exceeded

**High API costs:**
```elisp
;; Check current usage
M-x protagentic-show-config

;; Adjust cost limits
(setq protagentic-config-monthly-cost-limit 25.0)  ; Lower limit

;; Use more efficient model
(setq protagentic-config-llm-model "gpt-3.5-turbo")

;; Reduce token usage
(setq protagentic-config-llm-max-tokens 2000)

;; Switch to hybrid mode for automatic cost control
(setq protagentic-config-default-generation-mode 'hybrid)
```

**Poor LLM output quality:**
```elisp
;; Increase creativity
(setq protagentic-config-llm-temperature 0.8)

;; Use more powerful model
(setq protagentic-config-llm-model "gpt-4")

;; Increase token limit for longer responses
(setq protagentic-config-llm-max-tokens 6000)

;; Regenerate with different mode
M-x protagentic-regenerate-requirements
```

### Getting Help

**Status and Diagnostics:**
```
M-x protagentic-show-status        ; Show current spec status
M-x protagentic-list-specs         ; List all specs in project
```

**Debug Information:**
```elisp
;; Enable debug messages
(setq debug-on-error t)

;; Check loaded modules
(featurep 'protagentic)
(featurep 'protagentic-core)
```

## Testing

Run the test suite to verify installation:

```elisp
;; Load test files
(load-file "test/protagentic-test.el")
(load-file "test/protagentic-integration-test.el")

;; Run all tests
M-x protagentic-run-all-tests

;; Run specific test categories
M-x protagentic-run-tests              ; Unit tests only
M-x protagentic-run-integration-tests  ; Integration tests only
```

## Contributing

### Development Setup

1. Clone the repository
2. Add to Emacs load path
3. Run tests to verify setup
4. Make changes and add tests
5. Ensure all tests pass

### Code Style

- Follow standard Emacs Lisp conventions
- Use `lexical-binding: t` in all files
- Add comprehensive docstrings
- Include error handling for user-facing functions
- Write tests for new functionality

## License

GPL-3.0 License. See LICENSE file for details.

## Changelog

### Version 0.1.0 (Initial Release)
- Core three-phase workflow (requirements → design → tasks)
- Interactive refinement capabilities
- Comprehensive error handling and user guidance
- Integration with markdown-mode, projectile, and completion frameworks
- Extensive test suite with unit and integration tests
- Customizable templates and workflow behavior