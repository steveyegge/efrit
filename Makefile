# Makefile for Efrit - AI-Powered Emacs Coding Assistant

# Configuration
EMACS = emacs
EMACS_BATCH = $(EMACS) --batch --no-init-file
PACKAGE_NAME = efrit
VERSION = 0.2.0

# Source files
EL_FILES = efrit.el efrit-chat.el efrit-do.el efrit-tools.el efrit-multi-turn.el \
           efrit-debug.el efrit-command.el efrit-agent.el efrit-tests.el
ELC_FILES = $(EL_FILES:.el=.elc)

# Test files
TEST_FILES = efrit-tests.el
TEST_SCRIPTS = efrit-test-simple.sh

# Documentation files
DOC_FILES = README.md CONTRIBUTING.md AUTHORS

# Distribution files
DIST_FILES = $(EL_FILES) $(DOC_FILES) $(TEST_SCRIPTS) Makefile .gitignore

.PHONY: all compile test clean distclean install uninstall check help dist

# Default target
all: compile

# Help target
help:
	@echo "Efrit $(VERSION) - Makefile targets:"
	@echo ""
	@echo "Building:"
	@echo "  compile     - Byte compile all Elisp files"
	@echo "  clean       - Remove compiled files"
	@echo "  distclean   - Remove all generated files"
	@echo ""
	@echo "Testing:"
	@echo "  test        - Run all tests"
	@echo "  test-simple - Run basic tests only"
	@echo "  check       - Check syntax and compilation"
	@echo ""
	@echo "Development:"
	@echo "  lint        - Check code style and conventions"
	@echo "  debug       - Build with debug information"
	@echo ""
	@echo "Distribution:"
	@echo "  dist        - Create distribution tarball"
	@echo "  install     - Install to Emacs site-lisp"
	@echo "  uninstall   - Remove from Emacs site-lisp"

# Compilation
compile: $(ELC_FILES)

%.elc: %.el
	@echo "Compiling $<..."
	@$(EMACS_BATCH) \
		--eval "(add-to-list 'load-path \".\")" \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $<

# Check syntax without full compilation dependencies
check:
	@echo "Checking syntax of Elisp files..."
	@for file in $(EL_FILES); do \
		echo "Checking $$file..."; \
		$(EMACS_BATCH) --eval "(check-parens)" $$file || exit 1; \
	done
	@echo "✅ All syntax checks passed"

# Linting (basic style checks)
lint:
	@echo "Checking code style..."
	@for file in $(EL_FILES); do \
		echo "Linting $$file..."; \
		if grep -n "^(defun [^-]*[^-]p " $$file; then \
			echo "❌ Found predicate functions not ending with -p in $$file"; \
			exit 1; \
		fi; \
		if ! grep -q "lexical-binding: t" $$file; then \
			echo "❌ Missing lexical-binding: t in $$file"; \
			exit 1; \
		fi; \
	done
	@echo "✅ Code style checks passed"

# Testing
test: compile
	@echo "Running test suite..."
	@./efrit-test-simple.sh

test-simple: test

# Debug build (with extra information)
debug:
	@echo "Building with debug information..."
	@$(EMACS_BATCH) \
		--eval "(add-to-list 'load-path \".\")" \
		--eval "(setq byte-compile-debug t)" \
		--eval "(setq byte-compile-verbose t)" \
		-f batch-byte-compile $(EL_FILES)

# Cleaning
clean:
	@echo "Removing compiled files..."
	@rm -f $(ELC_FILES)
	@rm -f *.elc~

distclean: clean
	@echo "Removing all generated files..."
	@rm -f efrit-$(VERSION).tar.gz
	@rm -rf dist/

# Installation to system
EMACS_SITE_LISP = $(shell $(EMACS) --batch --eval "(princ (car site-lisp-directory-list))")

install: compile
	@echo "Installing to $(EMACS_SITE_LISP)/$(PACKAGE_NAME)..."
	@mkdir -p $(EMACS_SITE_LISP)/$(PACKAGE_NAME)
	@cp $(EL_FILES) $(ELC_FILES) $(EMACS_SITE_LISP)/$(PACKAGE_NAME)/
	@echo "✅ Installation complete"
	@echo "Add this to your init.el:"
	@echo "  (require 'efrit)"

uninstall:
	@echo "Removing from $(EMACS_SITE_LISP)/$(PACKAGE_NAME)..."
	@rm -rf $(EMACS_SITE_LISP)/$(PACKAGE_NAME)
	@echo "✅ Uninstallation complete"

# Distribution
dist: distclean
	@echo "Creating distribution tarball..."
	@mkdir -p dist/efrit-$(VERSION)
	@cp -r $(DIST_FILES) dist/efrit-$(VERSION)/
	@cd dist && tar -czf ../efrit-$(VERSION).tar.gz efrit-$(VERSION)/
	@rm -rf dist/
	@echo "✅ Created efrit-$(VERSION).tar.gz"

# Development helpers
dev-setup:
	@echo "Setting up development environment..."
	@echo "Checking prerequisites..."
	@which $(EMACS) > /dev/null || (echo "❌ Emacs not found"; exit 1)
	@$(EMACS_BATCH) --version | head -1
	@echo "✅ Development environment ready"

# Continuous integration target
ci: check lint compile test

# Show current configuration
config:
	@echo "Efrit Build Configuration:"
	@echo "  Version: $(VERSION)"
	@echo "  Emacs: $(EMACS)"
	@echo "  Source files: $(words $(EL_FILES)) files"
	@echo "  Test files: $(words $(shell echo $(TEST_FILES))) files"
	@echo "  Site lisp: $(EMACS_SITE_LISP)"
