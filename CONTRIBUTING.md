# Contributing to Efrit

Thank you for your interest in contributing to Efrit! This guide will help you get started with contributing to this AI-powered Emacs coding assistant.

## Table of Contents

- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Code Standards](#code-standards)
- [Testing](#testing)
- [Submitting Changes](#submitting-changes)
- [Bug Reports](#bug-reports)
- [Feature Requests](#feature-requests)

## Getting Started

### Prerequisites

- Emacs 28.1 or later
- Anthropic API key (for Claude API access)
- Basic knowledge of Emacs Lisp

### Development Setup

1. **Clone the repository**:
   ```bash
   git clone https://github.com/steveyegge/efrit.git
   cd efrit
   ```

2. **Set up your API key** in `~/.authinfo`:
   ```
   machine api.anthropic.com login personal password YOUR_API_KEY_HERE
   ```

3. **Load Efrit for development**:
   ```elisp
   (add-to-list 'load-path "/path/to/efrit")
   (require 'efrit)
   ```

4. **Run tests** to ensure everything works:
   ```bash
   ./efrit-test-simple.sh
   ```

## Code Standards

### Emacs Lisp Conventions

- **Use lexical binding**: All files must have `;;; -*- lexical-binding: t -*-`
- **Naming conventions**:
  - Functions: `kebab-case` (e.g., `efrit-send-message`)
  - Variables: `kebab-case` (e.g., `efrit-max-tokens`)
  - Private functions: prefix with `--` (e.g., `efrit--handle-response`)
  - Predicates: suffix with `-p` (e.g., `error-p`, `ready-p`)
- **Package prefix**: Use `efrit-` for all public functions and variables
- **Docstrings**: Required for all functions and variables
- **Line length**: Keep under 100 characters when possible
- **Function length**: Keep individual functions under 100 lines for better readability and ripgrep chunking
- **File length**: No restrictions - long files are normal in Elisp

### Code Quality

- **Error handling**: Use `condition-case` with meaningful error messages
- **Cleanup**: Use `unwind-protect` for resource management
- **Dependencies**: Minimize external dependencies; prefer built-in Emacs functions
- **Comments**: Code should be self-documenting; add comments only for complex logic

### Example Function Structure

```elisp
(defun efrit-example-function (arg1 arg2 &optional optional-arg)
  "Brief description of what this function does.
ARG1 is the first argument and does X.
ARG2 is the second argument and does Y.
Optional OPTIONAL-ARG when provided will Z."
  (condition-case err
      (let ((result (some-operation arg1 arg2)))
        (when optional-arg
          (setq result (modify-result result optional-arg)))
        result)
    (error
     (user-error "Operation failed: %s" (error-message-string err)))))
```

## Testing

### Running Tests

- **Quick tests**: `./efrit-test-simple.sh`
- **Comprehensive tests**: `./efrit-test-comprehensive.sh`
- **Manual testing**: `emacs -q -l efrit.el` then `M-x efrit-chat`

### Writing Tests

- Add test files with descriptive names (e.g., `test-multi-turn-basic.el`)
- Test both success and error cases
- Include integration tests for multi-component features
- Use the existing test patterns from `efrit-tests.el`

### Test Coverage Areas

- Core functionality (chat, commands, tools)
- Multi-turn conversation handling
- Error handling and recovery
- API integration
- Buffer management

## Submitting Changes

### Pull Request Process

1. **Fork the repository** and create a feature branch:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes** following the code standards above

3. **Test your changes**:
   ```bash
   ./efrit-test-simple.sh
   # Fix any issues before proceeding
   ```

4. **Commit with clear messages**:
   ```bash
   git commit -m "Add feature: brief description of what was added"
   ```

5. **Update documentation** if needed (README.md, docstrings, etc.)

6. **Submit a pull request** with:
   - Clear description of changes
   - Rationale for the changes
   - Any breaking changes noted
   - Test results

### Commit Message Format

Use clear, descriptive commit messages:
- `Add feature: multi-turn conversation support`
- `Fix bug: handle empty API responses gracefully`
- `Refactor: simplify error handling in efrit-chat`
- `Docs: update README with new configuration options`

## Bug Reports

### Before Reporting

1. **Search existing issues** to avoid duplicates
2. **Test with minimal config**: `emacs -q -l efrit.el`
3. **Check the debug buffer**: `M-x efrit-debug-show` (if debug is enabled)

### Bug Report Template

```markdown
**Describe the bug**
A clear description of what the bug is.

**To Reproduce**
Steps to reproduce the behavior:
1. Run `M-x efrit-chat`
2. Enter message "..."
3. See error

**Expected behavior**
What you expected to happen.

**Environment**
- Emacs version: [e.g., 29.1]
- Operating system: [e.g., macOS 13.0]
- Efrit version: [e.g., v0.2.0]

**Additional context**
- Error messages from `*Messages*` buffer
- Content of `*efrit-debug*` buffer (if available)
- Minimal reproduction case
```

## Feature Requests

### Before Requesting

1. **Check existing issues** and discussions
2. **Consider if it fits** Efrit's core mission as an Emacs-native AI assistant
3. **Think about implementation** - how would this work with Emacs Lisp?

### Feature Request Template

```markdown
**Feature Description**
Clear description of the proposed feature.

**Use Case**
Why would this feature be useful? What problem does it solve?

**Proposed Implementation**
If you have ideas about how this could be implemented.

**Alternatives Considered**
Other ways you've considered solving this problem.
```

## Development Areas

### High Priority
- Multi-turn conversation improvements
- Better error handling and recovery
- Performance optimizations
- Documentation improvements

### Medium Priority
- Additional tool integrations
- Voice command support
- Image processing capabilities
- Concurrent execution with Emacs threads

### Low Priority
- UI/UX enhancements
- Configuration management
- Integration with other Emacs packages

## Getting Help

- **GitHub Issues**: For bugs and feature requests
- **GitHub Discussions**: For questions and general discussion
- **Code Review**: Tag maintainers for review guidance

## Recognition

Contributors will be recognized in:
- `AUTHORS` file
- Release notes for significant contributions
- GitHub contributor statistics

Thank you for helping make Efrit better!
