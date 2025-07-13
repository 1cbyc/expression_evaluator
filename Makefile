# Makefile for Expression Evaluator
# Provides easy commands for building, testing, and running the project yes

# Variables
SBCL = sbcl
LISP_FILES = main.lisp tests.lisp cli.lisp
TEST_FILE = tests.lisp
CLI_FILE = cli.lisp

# Default target
.PHONY: all
all: test

# Run tests
.PHONY: test
test:
	@echo "Running test suite..."
	$(SBCL) --load $(TEST_FILE) --eval "(run-all-tests)" --quit

# Run specific test
.PHONY: test-%
test-%:
	@echo "Running test: $*"
	$(SBCL) --load $(TEST_FILE) --eval "(test-specific '$*)" --quit

# Start interactive mode
.PHONY: interactive
interactive:
	@echo "Starting interactive mode..."
	$(SBCL) --load main.lisp

# Start CLI
.PHONY: cli
cli:
	@echo "Starting command-line interface..."
	$(SBCL) --load $(CLI_FILE) --interactive

# Run examples
.PHONY: examples
examples:
	@echo "Running examples..."
	$(SBCL) --load main.lisp --eval "(run-examples)" --quit

# Benchmark performance
.PHONY: benchmark
benchmark:
	@echo "Running performance benchmark..."
	$(SBCL) --load main.lisp --eval "(benchmark-evaluation '(+ 1 2 3 4 5) 10000)" --quit

# Clean compiled files
.PHONY: clean
clean:
	@echo "Cleaning compiled files..."
	rm -f *.fasl *.fas *.dfsl *.pfsl *.d64fsl *.p64fsl
	rm -f *.lx64fsl *.lx32fsl *.dx64fsl *.dx32fsl
	rm -f *.fx64fsl *.fx32fsl *.sx64fsl *.sx32fsl
	rm -f *.wx64fsl *.wx32fsl

# Show help
.PHONY: help
help:
	@echo "Expression Evaluator - Available Commands"
	@echo "========================================"
	@echo ""
	@echo "make test          - Run all tests"
	@echo "make test-arithmetic - Run arithmetic tests only"
	@echo "make test-functions - Run function tests only"
	@echo "make interactive   - Start interactive mode"
	@echo "make cli           - Start command-line interface"
	@echo "make examples      - Run example expressions"
	@echo "make benchmark     - Run performance benchmark"
	@echo "make clean         - Clean compiled files"
	@echo "make help          - Show this help"
	@echo ""
	@echo "Examples:"
	@echo "  make test"
	@echo "  make interactive"
	@echo "  make cli"

# Quick evaluation of a single expression
.PHONY: eval
eval:
	@if [ -z "$(EXPR)" ]; then \
		echo "Usage: make eval EXPR='(+ 2 3)'"; \
		exit 1; \
	fi
	@echo "Evaluating: $(EXPR)"
	$(SBCL) --load main.lisp --eval "(format t \"Result: ~A~%\" (evaluate-expression '$(EXPR)))" --quit

# Check if SBCL is installed
.PHONY: check-sbcl
check-sbcl:
	@if ! command -v $(SBCL) > /dev/null 2>&1; then \
		echo "Error: $(SBCL) is not installed or not in PATH"; \
		echo "Please install Steel Bank Common Lisp (SBCL)"; \
		echo "Visit: http://www.sbcl.org/"; \
		exit 1; \
	fi
	@echo "$(SBCL) found: $(shell $(SBCL) --version | head -1)"

# Install dependencies (placeholder for future package management)
.PHONY: install
install: check-sbcl
	@echo "Expression Evaluator dependencies checked."
	@echo "No additional dependencies required."

# Development mode - watch for changes and run tests
.PHONY: dev
dev:
	@echo "Development mode - watching for changes..."
	@echo "Press Ctrl+C to stop"
	@while true; do \
		make test > /dev/null 2>&1 && echo "✓ Tests passed" || echo "✗ Tests failed"; \
		sleep 2; \
	done

# Create a simple executable script
.PHONY: install-script
install-script:
	@echo "#!/bin/bash" > eval-expr
	@echo "sbcl --load cli.lisp \"\$$@\"" >> eval-expr
	@chmod +x eval-expr
	@echo "Created executable script: ./eval-expr"
	@echo "Usage: ./eval-expr --expr \"(+ 2 3)\""

# Show project status
.PHONY: status
status:
	@echo "Expression Evaluator - Project Status"
	@echo "===================================="
	@echo "Files:"
	@ls -la *.lisp
	@echo ""
	@echo "Documentation:"
	@ls -la docs/
	@echo ""
	@echo "Git status:"
	@git status --porcelain 2>/dev/null || echo "Not a git repository" 