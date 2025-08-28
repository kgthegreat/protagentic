# Makefile for protagentic-executor testing

.PHONY: test test-verbose test-interactive clean help

# Default target
help:
	@echo "Available targets:"
	@echo "  test           - Run all tests in batch mode"
	@echo "  test-verbose   - Run tests with verbose output"
	@echo "  test-interactive - Run tests interactively in Emacs"
	@echo "  clean          - Clean up test artifacts"
	@echo "  help           - Show this help message"

# Run tests in batch mode
test:
	@echo "Running protagentic-executor tests..."
	emacs --batch --load test/run-tests.el

# Run tests with verbose output
test-verbose:
	@echo "Running protagentic-executor tests (verbose)..."
	emacs --batch --eval "(setq ert-batch-print-level 10 ert-batch-print-length 120)" --load test/run-tests.el

# Run tests interactively
test-interactive:
	@echo "Starting interactive test session..."
	emacs --load test/run-tests.el --eval "(run-protagentic-executor-tests)"

# Clean up test artifacts
clean:
	@echo "Cleaning up test artifacts..."
	find . -name "*.elc" -delete
	rm -rf /tmp/test-project /tmp/integration-test

# Run specific test
test-single:
	@if [ -z "$(TEST)" ]; then \
		echo "Usage: make test-single TEST=test-name"; \
		echo "Example: make test-single TEST=protagentic-executor-test-parse-tasks"; \
	else \
		echo "Running test: $(TEST)"; \
		emacs --batch --load test/run-tests.el --eval "(ert-run-tests-batch \"$(TEST)\")"; \
	fi