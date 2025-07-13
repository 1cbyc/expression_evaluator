# makefile for expression evaluator
# provides easy commands for building, testing, and running the project

# variables
SBCL = sbcl
LISP_FILES = main.lisp tests.lisp cli.lisp
TEST_FILE = tests.lisp
CLI_FILE = cli.lisp

# default target
.PHONY: all
all: test

# run tests
.PHONY: test
test:
	@echo "running test suite..."
	$(SBCL) --load $(TEST_FILE) --eval "(run-all-tests)" --quit

# run specific test
.PHONY: test-%
test-%:
	@echo "running test: $*"
	$(SBCL) --load $(TEST_FILE) --eval "(test-specific '$*)" --quit

# start interactive mode
.PHONY: interactive
interactive:
	@echo "starting interactive mode..."
	$(SBCL) --load main.lisp

# start cli
.PHONY: cli
cli:
	@echo "starting command-line interface..."
	$(SBCL) --load $(CLI_FILE) --interactive

# run examples
.PHONY: examples
examples:
	@echo "running examples..."
	$(SBCL) --load main.lisp --eval "(run-examples)" --quit

# benchmark performance
.PHONY: benchmark
benchmark:
	@echo "running performance benchmark..."
	$(SBCL) --load main.lisp --eval "(benchmark-evaluation '(+ 1 2 3 4 5) 10000)" --quit

# clean compiled files
.PHONY: clean
clean:
	@echo "cleaning compiled files..."
	rm -f *.fasl *.fas *.dfsl *.pfsl *.d64fsl *.p64fsl
	rm -f *.lx64fsl *.lx32fsl *.dx64fsl *.dx32fsl
	rm -f *.fx64fsl *.fx32fsl *.sx64fsl *.sx32fsl
	rm -f *.wx64fsl *.wx32fsl

# show help
.PHONY: help
help:
	@echo "expression evaluator - available commands"
	@echo "========================================"
	@echo ""
	@echo "make test          - run all tests"
	@echo "make test-arithmetic - run arithmetic tests only"
	@echo "make test-functions - run function tests only"
	@echo "make interactive   - start interactive mode"
	@echo "make cli           - start command-line interface"
	@echo "make examples      - run example expressions"
	@echo "make benchmark     - run performance benchmark"
	@echo "make clean         - clean compiled files"
	@echo "make help          - show this help"
	@echo ""
	@echo "examples:"
	@echo "  make test"
	@echo "  make interactive"
	@echo "  make cli"

# quick evaluation of a single expression
.PHONY: eval
eval:
	@if [ -z "$(EXPR)" ]; then \
		echo "usage: make eval EXPR='(+ 2 3)'"; \
		exit 1; \
	fi
	@echo "evaluating: $(EXPR)"
	$(SBCL) --load main.lisp --eval "(format t \"result: ~a~%\" (evaluate-expression '$(EXPR)))" --quit

# check if sbcl is installed
.PHONY: check-sbcl
check-sbcl:
	@if ! command -v $(SBCL) > /dev/null 2>&1; then \
		echo "error: $(SBCL) is not installed or not in path"; \
		echo "please install steel bank common lisp (sbcl)"; \
		echo "visit: http://www.sbcl.org/"; \
		exit 1; \
	fi
	@echo "$(SBCL) found: $(shell $(SBCL) --version | head -1)"

# install dependencies (placeholder for future package management)
.PHONY: install
install: check-sbcl
	@echo "expression evaluator dependencies checked."
	@echo "no additional dependencies required."

# development mode - watch for changes and run tests
.PHONY: dev
dev:
	@echo "development mode - watching for changes..."
	@echo "press ctrl+c to stop"
	@while true; do \
		make test > /dev/null 2>&1 && echo "tests passed" || echo "tests failed"; \
		sleep 2; \
	done

# create a simple executable script
.PHONY: install-script
install-script:
	@echo "#!/bin/bash" > eval-expr
	@echo "sbcl --load cli.lisp \"\$$@\"" >> eval-expr
	@chmod +x eval-expr
	@echo "created executable script: ./eval-expr"
	@echo "usage: ./eval-expr --expr \"(+ 2 3)\""

# show project status
.PHONY: status
status:
	@echo "expression evaluator - project status"
	@echo "===================================="
	@echo "files:"
	@ls -la *.lisp
	@echo ""
	@echo "documentation:"
	@ls -la docs/
	@echo ""
	@echo "git status:"
	@git status --porcelain 2>/dev/null || echo "not a git repository" 