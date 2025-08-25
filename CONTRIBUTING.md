# Contributing to Protagentic

Thank you for your interest in contributing to Protagentic! This document provides guidelines for contributing to the project.

## Getting Started

### Prerequisites

- Emacs 26.1 or later
- Basic knowledge of Emacs Lisp
- Git for version control

### Development Setup

1. **Clone the repository:**
   ```bash
   git clone https://github.com/kgthegreat/protagentic.git
   cd protagentic
   ```

2. **Load in Emacs:**
   ```elisp
   (add-to-list 'load-path "/path/to/protagentic")
   (require 'protagentic)
   ```

3. **Run tests:**
   ```bash
   make test
   ```

## How to Contribute

### Reporting Issues

- Use the [GitHub issue tracker](https://github.com/kgthegreat/protagentic/issues)
- Search existing issues before creating new ones
- Include Emacs version, OS, and steps to reproduce
- Provide error messages and relevant configuration

### Suggesting Features

- Open an issue with the "enhancement" label
- Describe the use case and expected behavior
- Consider if the feature fits Protagentic's core mission

### Code Contributions

1. **Fork the repository**
2. **Create a feature branch:**
   ```bash
   git checkout -b feature/your-feature-name
   ```
3. **Make your changes**
4. **Add tests** for new functionality
5. **Run the test suite:**
   ```bash
   make test
   ```
6. **Update documentation** if needed
7. **Commit with descriptive messages**
8. **Push and create a pull request**

## Development Guidelines

### Code Style

- Follow standard Emacs Lisp conventions
- Use `lexical-binding: t` in all files
- Prefix all functions with `protagentic-`
- Include docstrings for all public functions
- Use `defcustom` for user-configurable options

### Testing

- Write tests for all new functionality
- Use ERT (Emacs Lisp Regression Testing)
- Place tests in the `test/` directory
- Mock external dependencies (LLM API calls)
- Ensure tests pass on Emacs 26.1+

### Documentation

- Update README.md for user-facing changes
- Add examples to EXAMPLES.md when appropriate
- Update docstrings for modified functions
- Consider updating LLM-SETUP.md for API changes

### Commit Messages

Use conventional commit format:
```
type(scope): description

- feat: new feature
- fix: bug fix
- docs: documentation changes
- test: test additions/changes
- refactor: code refactoring
- style: formatting changes
```

Examples:
```
feat(llm): add support for custom API endpoints
fix(config): resolve API key validation issue
docs(readme): update installation instructions
```

## Project Structure

```
protagentic/
├── protagentic.el              # Main package file
├── protagentic-core.el         # Core data structures
├── protagentic-commands.el     # Interactive commands
├── protagentic-llm.el          # LLM integration
├── protagentic-config.el       # Configuration management
├── protagentic-generator.el    # Content generation
├── protagentic-templates.el    # Template system
├── protagentic-prompts.el      # LLM prompts
├── protagentic-navigation.el   # Navigation commands
├── protagentic-utils.el        # Utility functions
├── test/                       # Test files
├── README.md                   # Main documentation
├── EXAMPLES.md                 # Usage examples
└── LLM-SETUP.md               # LLM configuration guide
```

## Areas for Contribution

### High Priority
- Bug fixes and stability improvements
- Performance optimizations
- Better error messages and user guidance
- Documentation improvements

### Medium Priority
- Additional LLM provider support (Anthropic, etc.)
- Integration with more project management tools
- Enhanced template customization
- Improved validation and quality checking

### Low Priority
- UI/UX enhancements
- Additional export formats
- Integration with external tools
- Advanced workflow features

## Testing

### Running Tests

```bash
# Run all tests
make test

# Run specific test file
emacs -batch -l test/protagentic-test.el -f ert-run-tests-batch-and-exit

# Run with coverage (if available)
make test-coverage
```

### Writing Tests

- Test files should end with `-test.el`
- Use descriptive test names: `protagentic-test-feature-behavior`
- Mock external dependencies
- Test both success and error cases
- Include integration tests for workflows

Example test:
```elisp
(ert-deftest protagentic-test-spec-creation ()
  "Test basic spec creation functionality."
  (let ((temp-dir (make-temp-file "protagentic-test" t)))
    (unwind-protect
        (progn
          ;; Test setup
          (setq protagentic-spec-directory temp-dir)
          ;; Test assertions
          (should (protagentic-create-spec "test-spec"))
          (should (file-exists-p (expand-file-name "test-spec/requirements.md" temp-dir))))
      ;; Cleanup
      (delete-directory temp-dir t))))
```

## Release Process

1. Update version in `protagentic.el` and `protagentic-pkg.el`
2. Update CHANGELOG.md
3. Run full test suite
4. Create git tag: `git tag -a v1.0.0 -m "Release 1.0.0"`
5. Push tag: `git push origin v1.0.0`
6. Create GitHub release

## Questions?

- Open an issue for questions about contributing
- Check existing documentation in README.md and EXAMPLES.md
- Look at existing code for patterns and conventions

## License

By contributing to Protagentic, you agree that your contributions will be licensed under the same GPL-3.0+ license as the project.

Thank you for contributing! 🎉