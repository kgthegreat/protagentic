# Protagentic Code Generation Guide

## Overview

Protagentic now supports complete end-to-end development workflow from requirements to production-ready code. This guide explains how to use the enhanced workflow for robust, maintainable code generation.

## Complete Workflow

### Phase 1: Requirements & Design (Existing)
1. **Create Spec**: `M-x protagentic-create-spec`
2. **Generate Design**: `M-x protagentic-generate-design`  
3. **Generate Tasks**: `M-x protagentic-generate-tasks`

### Phase 2: Code Generation (New)
4. **Execute Tasks**: `M-x protagentic-execute-next-task` or `M-x protagentic-execute-all-tasks`
5. **Validate Quality**: `M-x protagentic-validate-code-quality`
6. **Monitor Progress**: `M-x protagentic-show-task-status`

## Code Quality Standards

### Built-in Quality Guidelines

Protagentic enforces robust code quality through multiple standards:

#### Clean Code (Default)
- **Naming**: Intention-revealing names, no abbreviations
- **Functions**: Single responsibility, max 30 lines
- **Structure**: Minimal nesting (max 3 levels), early returns
- **Documentation**: Self-documenting code, minimal comments

#### Google Style
- **Formatting**: 80-100 character lines, consistent indentation
- **Documentation**: Comprehensive API documentation
- **Conventions**: Language-specific Google style guides

#### Airbnb Style (JavaScript focus)
- **Naming**: camelCase variables, PascalCase classes
- **Syntax**: Arrow functions, const over let, semicolons
- **Structure**: Destructuring, single quotes

### Quality Validation

Every generated file is automatically validated for:

1. **Function Quality**
   - Length limits (max 30 lines)
   - Parameter count (language-specific limits)
   - Naming conventions
   - Single responsibility

2. **Code Complexity**
   - Cyclomatic complexity (max 10)
   - Nesting depth (max 3 levels)
   - Decision point counting

3. **Security**
   - SQL injection prevention
   - XSS vulnerability checks
   - Hardcoded credential detection
   - Path traversal protection

4. **Documentation**
   - Function documentation requirements
   - Complex algorithm explanations
   - API usage examples

5. **Testing**
   - Test file generation for all implementation files
   - Edge case coverage
   - Error condition testing
   - Minimum 90% coverage target

## Configuration

### Quality Standards
```elisp
;; Set code quality standard
(setq protagentic-code-quality-standard "clean-code") ; or "google", "airbnb", "custom"

;; Configure quality enforcement
(setq protagentic-enforce-test-coverage t)
(setq protagentic-min-test-coverage 90)
(setq protagentic-auto-validate-quality t)
(setq protagentic-max-function-lines 30)
```

### Generation Preferences
```elisp
;; Use LLM for high-quality code generation
(setq protagentic-config-default-generation-mode 'llm)

;; Automatic quality validation after generation
(setq protagentic-auto-validate-quality t)
```

## Example Workflow

### 1. Create Feature Spec
```
M-x protagentic-create-spec
Feature: User authentication system
```

### 2. Generate Implementation Plan
```
M-x protagentic-generate-design
M-x protagentic-generate-tasks
```

### 3. Execute Tasks
```
M-x protagentic-execute-all-tasks
```

This generates:
- **Implementation files**: Core functionality with proper error handling
- **Test files**: Comprehensive test suites with edge cases
- **Documentation**: API docs and usage examples
- **Configuration**: Setup and deployment files

### 4. Validate Quality
```
M-x protagentic-validate-code-quality
```

Quality report shows:
- **Errors**: Critical issues requiring fixes
- **Warnings**: Style and best practice suggestions  
- **Score**: Overall quality score (0-100)
- **Suggestions**: Actionable improvement recommendations

## Generated Code Structure

### Implementation Files
```javascript
// user-auth.js - Clean, maintainable implementation
class UserAuth {
  constructor(config) {
    this.validateConfig(config);
    this.config = config;
  }

  async authenticate(credentials) {
    try {
      this.validateCredentials(credentials);
      const user = await this.verifyUser(credentials);
      return this.generateToken(user);
    } catch (error) {
      this.logError('Authentication failed', error);
      throw new AuthenticationError(error.message);
    }
  }

  validateCredentials(credentials) {
    if (!credentials?.username || !credentials?.password) {
      throw new ValidationError('Username and password required');
    }
  }
}
```

### Test Files
```javascript
// user-auth.test.js - Comprehensive test coverage
describe('UserAuth', () => {
  describe('authenticate', () => {
    it('should authenticate valid user credentials', async () => {
      // Arrange
      const auth = new UserAuth(validConfig);
      const credentials = { username: 'user', password: 'pass' };
      
      // Act
      const result = await auth.authenticate(credentials);
      
      // Assert
      expect(result).toHaveProperty('token');
      expect(result.token).toBeTruthy();
    });

    it('should throw ValidationError for missing credentials', async () => {
      // Arrange
      const auth = new UserAuth(validConfig);
      
      // Act & Assert
      await expect(auth.authenticate({}))
        .rejects.toThrow(ValidationError);
    });
  });
});
```

## Best Practices

### 1. Iterative Development
- Execute one task at a time for complex features
- Review and refine generated code before proceeding
- Use quality validation to catch issues early

### 2. Quality First
- Always run quality validation after generation
- Address errors before warnings
- Aim for 95+ quality scores

### 3. Test-Driven Approach
- Generated tests serve as specifications
- Run tests immediately after generation
- Add additional edge cases as needed

### 4. Security Mindset
- Review security warnings carefully
- Validate all input handling
- Check authentication and authorization logic

### 5. Maintainability Focus
- Keep functions small and focused
- Use descriptive names throughout
- Document complex business logic

## Troubleshooting

### Common Issues

#### Task Execution Fails
```
Error: Task validation failed: Missing error handling
```
**Solution**: The LLM generated code without proper error handling. The system will retry with enhanced prompts.

#### Quality Validation Errors
```
Error: Function 'processData' is too long (45 lines, max 30)
```
**Solution**: The generated function needs refactoring. Re-run the task with more specific requirements.

#### Missing Test Coverage
```
Warning: Missing test file for: user-service.js
```
**Solution**: Ensure test generation is enabled in the task prompt. Re-execute the task.

### Performance Tips

1. **Use Hybrid Mode**: Combines LLM quality with template reliability
2. **Batch Execution**: Use `execute-all-tasks` for complete features
3. **Quality Caching**: Validation results are cached for faster re-runs

## Advanced Features

### Custom Quality Rules
Create custom quality guidelines in `.protagentic/quality-rules.el`:

```elisp
(defun my-custom-quality-rules ()
  "Custom quality rules for my project."
  '((max-function-lines . 25)
    (require-jsdoc . t)
    (enforce-immutability . t)))
```

### Language-Specific Configuration
```elisp
;; JavaScript projects
(setq protagentic-js-test-framework "jest")
(setq protagentic-js-style-guide "airbnb")

;; Python projects  
(setq protagentic-python-test-framework "pytest")
(setq protagentic-python-style-guide "pep8")
```

### Integration with External Tools
- **ESLint**: Automatic linting integration
- **Prettier**: Code formatting
- **SonarQube**: Advanced quality metrics
- **Coverage.py**: Test coverage reporting

## Next Steps

1. **Try the Workflow**: Start with a simple feature to learn the process
2. **Customize Standards**: Adapt quality rules to your project needs
3. **Integrate Tools**: Connect with your existing development pipeline
4. **Share Feedback**: Help improve the code generation quality

The enhanced Protagentic workflow transforms your requirements into production-ready, maintainable code while enforcing industry best practices throughout the development process.