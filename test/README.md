# Protagentic Executor Tests

This directory contains comprehensive tests for the `protagentic-executor` module, covering the entire task execution flow from parsing to file generation.

## Test Coverage

The test suite covers the following areas:

### 1. Task Parsing (`protagentic-executor-test-parse-tasks`)
- Parsing markdown task lists into structured data
- Extracting task details, requirements references, and status
- Handling both completed and pending tasks

### 2. Task Selection (`protagentic-executor-test-find-next-task`)
- Finding the next pending task in a task list
- Skipping completed tasks

### 3. Prompt Generation (`protagentic-executor-test-build-prompt`)
- Building comprehensive code generation prompts
- Including task details, requirements, and design context
- Formatting instructions for LLM responses

### 4. File Parsing (`protagentic-executor-test-parse-generated-files-*`)
- Parsing files from standard LLM response formats (`**filename.ext**`)
- Parsing files from alternative formats (`Create file:`, `File:`)
- Handling multiple file extraction patterns
- Extracting tool calls and structured responses

### 5. Code Validation (`protagentic-executor-test-validate-generated-code`)
- Validating generated files for basic requirements
- Checking for empty filenames or content
- Error reporting and validation results

### 6. File Writing (`protagentic-executor-test-file-writing`)
- Creating directory structures as needed
- Writing files to the correct locations
- Handling file system operations

### 7. Single Task Execution (`protagentic-executor-test-execute-single-task`)
- End-to-end execution of a single task
- Success and failure scenarios
- Task status updates

### 8. Integration Testing (`protagentic-executor-test-integration-full-flow`)
- Complete workflow from task parsing to file generation
- Mocked external dependencies
- Realistic execution scenarios

## Running Tests

### Prerequisites
- Emacs 25.1 or later
- ERT (Emacs Lisp Regression Testing) - included with Emacs

### Command Line Options

```bash
# Run all tests
make test

# Run tests with verbose output
make test-verbose

# Run tests interactively in Emacs
make test-interactive

# Run a specific test
make test-single TEST=protagentic-executor-test-parse-tasks

# Clean up test artifacts
make clean
```

### Manual Execution

```bash
# Run tests directly with Emacs
emacs --batch --load test/run-tests.el

# Run tests interactively
emacs --load test/run-tests.el --eval "(run-protagentic-executor-tests)"
```

### Within Emacs

```elisp
;; Load the test runner
(load-file "test/run-tests.el")

;; Run all tests interactively
(run-protagentic-executor-tests)

;; Run specific test
(ert-run-tests-interactively "protagentic-executor-test-parse-tasks")
```

## Test Data

The tests use realistic sample data:

- **Task Content**: Sample markdown with various task formats
- **Requirements**: EARS-formatted requirements with user stories
- **Design**: Architectural design document
- **LLM Responses**: Multiple response formats that LLMs commonly generate

## Mocking Strategy

The tests use comprehensive mocking to isolate the executor logic:

- **File System**: Mocked file reading/writing operations
- **LLM Integration**: Mocked API calls with predefined responses
- **Project Detection**: Mocked project root and file scanning
- **External Dependencies**: All protagentic modules are mocked

## Test Fixtures

### Sample Task Content
```markdown
- [ ] 1. Set up project structure and core interfaces
  - Create directory structure for models, services, and API components
  - _Requirements: 1.1_

- [x] 2. Implement data models and validation
  - Write TypeScript interfaces for all data models
  - _Requirements: 2.1, 3.3, 1.2_
```

### Sample LLM Response
```
**src/models/Task.js**
```javascript
class Task {
  constructor(id, title, description) {
    this.id = id;
    this.title = title;
    this.description = description;
  }
}
```

## Debugging Tests

### Verbose Output
Use `make test-verbose` to see detailed test execution information.

### Interactive Debugging
Use `make test-interactive` to run tests in an Emacs session where you can:
- Set breakpoints with `(debug)`
- Inspect variables
- Step through code execution

### Individual Test Execution
Run specific tests to isolate issues:
```bash
make test-single TEST=protagentic-executor-test-parse-tasks
```

## Adding New Tests

1. Add test functions to `protagentic-executor-test.el`
2. Follow the naming convention: `protagentic-executor-test-<feature>`
3. Use the existing mock functions or create new ones as needed
4. Include both success and failure scenarios
5. Test edge cases and error conditions

### Test Function Template
```elisp
(ert-deftest protagentic-executor-test-new-feature ()
  "Test description for the new feature."
  (let ((test-data "sample data"))
    ;; Setup mocks if needed
    (cl-letf (((symbol-function 'external-function)
               (lambda (args) "mocked result")))
      
      ;; Execute the function under test
      (let ((result (protagentic-executor-new-feature test-data)))
        
        ;; Assertions
        (should result)
        (should (equal result "expected value"))))))
```

## Continuous Integration

The tests are designed to run in CI environments:
- No interactive prompts
- Batch mode execution
- Clear exit codes (0 for success, non-zero for failure)
- Comprehensive error reporting

## Troubleshooting

### Common Issues

1. **Missing Dependencies**: Ensure all required Emacs packages are available
2. **File Permissions**: Make sure test files are readable
3. **Path Issues**: Tests use relative paths from the project root

### Debug Mode
Set `debug-on-error` to `t` to get detailed stack traces:
```elisp
(setq debug-on-error t)
```

## Performance

The test suite is designed to run quickly:
- Mocked external dependencies (no actual LLM calls)
- Minimal file system operations
- Focused unit tests with clear boundaries

Typical execution time: < 5 seconds for the full suite.