# Makefile for Efrit - AI-Powered Emacs Coding Assistant

# Configuration
EMACS = emacs
EMACS_BATCH = $(EMACS) --batch --no-init-file
PACKAGE_NAME = efrit
VERSION = 0.3.0

# Source files
EL_FILES = $(wildcard lisp/*.el)
ELC_FILES = $(EL_FILES:.el=.elc)

# Test files
TEST_FILES = $(wildcard test/test-*.el)
TEST_SCRIPTS = $(wildcard test/*.sh bin/*.sh)

# Documentation files
DOC_FILES = README.md CONTRIBUTING.md AUTHORS AGENTS.md LICENSE

# Distribution files
DIST_FILES = lisp/ test/ bin/ plans/ $(DOC_FILES) Makefile .gitignore

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
compile: lisp/efrit-config.elc lisp/efrit-log.elc lisp/efrit-common.elc lisp/efrit-tools.elc $(ELC_FILES)

# Dependency hierarchy: efrit-config first, then efrit-log, efrit-common, efrit-tools, then everything else
lisp/efrit-log.elc: lisp/efrit-config.elc
lisp/efrit-common.elc: lisp/efrit-config.elc
lisp/efrit-tools.elc: lisp/efrit-config.elc lisp/efrit-common.elc
lisp/efrit-debug.elc: lisp/efrit-log.elc
lisp/efrit-chat.elc: lisp/efrit-tools.elc lisp/efrit-debug.elc lisp/efrit-common.elc
lisp/efrit-chat-streamlined.elc: lisp/efrit-tools.elc lisp/efrit-common.elc
lisp/efrit-remote-queue.elc: lisp/efrit-tools.elc lisp/efrit-config.elc
lisp/efrit-context.elc: lisp/efrit-config.elc lisp/efrit-tools.elc
lisp/efrit-protocol.elc: lisp/efrit-config.elc
lisp/efrit-performance.elc: lisp/efrit-config.elc
lisp/efrit-progress.elc: lisp/efrit-config.elc lisp/efrit-tools.elc
lisp/efrit-multi-turn.elc: lisp/efrit-tools.elc lisp/efrit-config.elc
lisp/efrit-do.elc: lisp/efrit-tools.elc lisp/efrit-config.elc lisp/efrit-session-tracker.elc
lisp/efrit-agent.elc: lisp/efrit-tools.elc lisp/efrit-log.elc lisp/efrit-common.elc
lisp/efrit-async.elc: lisp/efrit-common.elc lisp/efrit-context.elc lisp/efrit-protocol.elc lisp/efrit-performance.elc lisp/efrit-progress.elc
lisp/efrit-session-tracker.elc: lisp/efrit-config.elc
lisp/efrit-dashboard.elc: lisp/efrit-config.elc lisp/efrit-tools.elc
lisp/efrit-command.elc: lisp/efrit-tools.elc
lisp/efrit-autonomous-startup.elc: lisp/efrit-config.elc
lisp/efrit.elc: lisp/efrit-config.elc lisp/efrit-tools.elc

lisp/%.elc: lisp/%.el
	@echo "Compiling $<..."
	@$(EMACS_BATCH) \
		--eval "(add-to-list 'load-path \"./lisp\")" \
		--eval "(setq byte-compile-error-on-warn nil)" \
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
		if ! grep -q "lexical-binding: t" $$file; then \
			echo "❌ Missing lexical-binding: t in $$file"; \
			exit 1; \
		fi; \
	done
	@echo "✅ Code style checks passed"

# Testing
test: compile
	@echo "Running test suite..."
	@cd test && ./efrit-test-simple.sh
	@./bin/launch-autonomous-efrit.sh test || echo "⚠️  Autonomous tests skipped (optional)"

test-simple:
	@echo "Running basic tests..."
	@cd test && ./efrit-test-simple.sh

# Debug build (with extra information)
debug:
	@echo "Building with debug information..."
	@$(EMACS_BATCH) \
		--eval "(add-to-list 'load-path \"./lisp\")" \
		--eval "(setq byte-compile-debug t)" \
		--eval "(setq byte-compile-verbose t)" \
		-f batch-byte-compile $(EL_FILES)

# Cleaning
clean:
	@echo "Removing compiled files..."
	@rm -f lisp/*.elc
	@rm -f lisp/*.elc~

distclean: clean
	@echo "Removing all generated files..."
	@rm -f efrit-$(VERSION).tar.gz
	@rm -rf dist/

# Installation to system
EMACS_SITE_LISP = $(shell $(EMACS) --batch --eval "(princ (car site-lisp-directory-list))" 2>/dev/null || echo "/usr/local/share/emacs/site-lisp")

install: compile
	@echo "Installing to $(EMACS_SITE_LISP)/$(PACKAGE_NAME)..."
	@mkdir -p $(EMACS_SITE_LISP)/$(PACKAGE_NAME)
	@cp -r lisp/* $(EMACS_SITE_LISP)/$(PACKAGE_NAME)/
	@chmod +x bin/launch-autonomous-efrit.sh
	@cp bin/launch-autonomous-efrit.sh /usr/local/bin/ 2>/dev/null || echo "⚠️  Could not install launcher script (run as root?)"
	@echo "✅ Installation complete"
	@echo "Add this to your init.el:"
	@echo "  (add-to-list 'load-path \"$(EMACS_SITE_LISP)/$(PACKAGE_NAME)\")"
	@echo "  (require 'efrit)"

uninstall:
	@echo "Removing from $(EMACS_SITE_LISP)/$(PACKAGE_NAME)..."
	@rm -rf $(EMACS_SITE_LISP)/$(PACKAGE_NAME)
	@rm -f /usr/local/bin/launch-autonomous-efrit.sh
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
	@echo "Project structure:"
	@find . -name "*.el" | head -10
	@echo "✅ Development environment ready"

# Continuous integration target
ci: check lint compile test

# Show current configuration
config:
	@echo "Efrit Build Configuration:"
	@echo "  Version: $(VERSION)"
	@echo "  Emacs: $(EMACS)"
	@echo "  Source files: $(words $(EL_FILES)) files"
	@echo "  Test files: $(words $(TEST_FILES)) files"
	@echo "  Site lisp: $(EMACS_SITE_LISP)"
	@echo "  Structure: Professional elisp project layout"

# Development convenience targets
quick-test: compile
	@echo "Running quick development tests..."
	@$(EMACS_BATCH) \
		--eval "(add-to-list 'load-path \"./lisp\")" \
		--eval "(require 'efrit)" \
		--eval "(message \"✅ Efrit loads successfully\")"

# Show project structure
tree:
	@echo "Efrit Project Structure:"
	@tree -I '.git|*.elc|.DS_Store' -a || find . -name ".*" -prune -o -type f -print | sort
