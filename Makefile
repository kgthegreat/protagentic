# Makefile for Protagentic Emacs package

EMACS ?= emacs
PACKAGE_NAME = protagentic
VERSION = 0.1.0

# Source files
SOURCES = protagentic.el \
          protagentic-core.el \
          protagentic-utils.el \
          protagentic-templates.el \
          protagentic-commands.el \
          protagentic-navigation.el

# Test files
TEST_SOURCES = test/protagentic-test.el \
               test/protagentic-config-test.el \
               test/protagentic-generator-test.el \
               test/protagentic-integration-test.el \
               test/protagentic-llm-test.el \
               test/protagentic-prompts-test.el

# Compiled files
OBJECTS = $(SOURCES:.el=.elc)

# Package files
PACKAGE_FILES = $(SOURCES) \
                protagentic-pkg.el \
                README.md \
                EXAMPLES.md

.PHONY: all compile test clean package install uninstall help

all: compile

help:
	@echo "Available targets:"
	@echo "  compile    - Byte-compile all source files"
	@echo "  test       - Run all tests"
	@echo "  clean      - Remove compiled files and temporary directories"
	@echo "  package    - Create distribution package"
	@echo "  install    - Install package locally"
	@echo "  uninstall  - Remove installed package"
	@echo "  lint       - Check code style and potential issues"
	@echo "  docs       - Generate documentation"

compile: $(OBJECTS)

%.elc: %.el
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<

test: compile
	$(EMACS) -batch -Q -L . -L test \
		-l test/protagentic-test.el \
		-l test/protagentic-config-test.el \
		-l test/protagentic-generator-test.el \
		-l test/protagentic-integration-test.el \
		-l test/protagentic-llm-test.el \
		-l test/protagentic-prompts-test.el \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f $(OBJECTS)
	rm -f *.tar
	rm -rf $(PACKAGE_NAME)-$(VERSION)/
	rm -rf .protagentic/

package: clean compile
	mkdir -p $(PACKAGE_NAME)-$(VERSION)
	cp $(PACKAGE_FILES) $(PACKAGE_NAME)-$(VERSION)/
	tar -cf $(PACKAGE_NAME)-$(VERSION).tar $(PACKAGE_NAME)-$(VERSION)/
	rm -rf $(PACKAGE_NAME)-$(VERSION)/
	@echo "Package created: $(PACKAGE_NAME)-$(VERSION).tar"

install: package
	$(EMACS) -batch -Q \
		--eval "(progn (require 'package) \
		               (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\")) \
		               (package-initialize) \
		               (package-install-file \"$(PACKAGE_NAME)-$(VERSION).tar\"))"

uninstall:
	$(EMACS) -batch -Q \
		--eval "(progn (require 'package) \
		               (package-initialize) \
		               (package-delete (cadr (assq '$(PACKAGE_NAME) package-alist))))"

lint: compile
	$(EMACS) -batch -Q -L . \
		--eval "(progn (require 'checkdoc) \
		               (setq checkdoc-diagnostic-buffer \"*checkdoc-errors*\") \
		               (mapcar (lambda (file) \
		                        (checkdoc-file file)) \
		                      '$(SOURCES)))"

docs:
	@echo "Documentation files:"
	@echo "  README.md - Main documentation"
	@echo "  EXAMPLES.md - Usage examples"
	@echo "  Generated docs available via M-x describe-package RET protagentic RET"

# Development targets
dev-setup:
	@echo "Development setup:"
	@echo "1. Add to your Emacs config:"
	@echo "   (add-to-list 'load-path \"$(PWD)\")"
	@echo "   (require 'protagentic)"
	@echo "2. Run tests: make test"
	@echo "3. Try example: M-x protagentic-create-spec"

check-dependencies:
	$(EMACS) -batch -Q \
		--eval "(progn (message \"Emacs version: %s\" emacs-version) \
		               (when (version< emacs-version \"26.1\") \
		                 (error \"Emacs 26.1 or higher required\")))"

# CI targets for automated testing
ci-test: check-dependencies compile test lint

# Release preparation
prepare-release: clean ci-test package
	@echo "Release $(VERSION) prepared successfully"
	@echo "Package file: $(PACKAGE_NAME)-$(VERSION).tar"
	@echo "Next steps:"
	@echo "1. Test installation: make install"
	@echo "2. Update version in protagentic.el and protagentic-pkg.el"
	@echo "3. Tag release: git tag v$(VERSION)"
	@echo "4. Submit to MELPA"