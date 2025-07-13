# Expression Evaluator

A comprehensive mathematical expression evaluator written in Common Lisp. This project demonstrates advanced concepts in functional programming, recursive algorithms, and expression parsing.

## What it does

This program takes mathematical expressions written in Lisp format and calculates their results. It supports basic arithmetic, mathematical functions, variables, and complex nested expressions.

### Examples

```lisp
(+ 2 3)           ; Returns 5
(* 4 (+ 2 3))     ; Returns 20
(/ 10 2)          ; Returns 5
(- 10 3 2)        ; Returns 5
(sin pi)           ; Returns 0
(pow 2 3)         ; Returns 8
(sqrt 16)          ; Returns 4
(set x 10)         ; Sets variable x to 10
(+ x 5)            ; Returns 15 (using variable)
```

## WHat it can do

### Main FUnctions
- **Basic arithmetic**: `+`, `-`, `*`, `/`, `%`
- **Mathematical functions**: `sin`, `cos`, `tan`, `log`, `exp`, `abs`, `floor`, `ceil`
- **Power and roots**: `pow`, `sqrt`
- **Constants**: `pi`, `e`, `inf`
- **Variables**: Dynamic variable assignment and retrieval
- **Error handling**: Comprehensive error messages for invalid expressions

### Advanced Feats
- **Interactive mode**: REPL for testing expressions
- **Command-line interface**: Process expressions from command line
- **File processing**: Evaluate expressions from text files
- **Performance benchmarking**: Measure evaluation speed
- **Test suite**: Comprehensive testing framework
- **Pretty printing**: Formatted expression output

## TO USe this Tool

### Prerequisites
Install Steel Bank Common Lisp (SBCL):
- **Windows**: Download from [sbcl.org](http://www.sbcl.org/)
- **macOS**: `brew install sbcl`
- **Linux**: `sudo apt-get install sbcl`

### Basic Usage (per oga)

1. **Interactive mode**:
   ```bash
   sbcl --load main.lisp
   ```

2. **Command-line interface**:
   ```bash
   sbcl --load cli.lisp --interactive
   ```

3. **Evaluate single expression**:
   ```bash
   sbcl --load cli.lisp --expr "(+ 2 3)"
   ```

4. **Process expressions from file**:
   ```bash
   sbcl --load cli.lisp --file examples/expressions.txt
   ```

5. **Run tests**:
   ```bash
   sbcl --load tests.lisp --eval "(run-all-tests)" --quit
   ```

### Using Makefile (I recommend this)

```bash
make test          # Run all tests
make interactive   # Start interactive mode
make cli           # Start command-line interface
make examples      # Run example expressions
make benchmark     # Performance benchmark
make eval EXPR='(+ 2 3)'  # Quick evaluation
```

## Supported Operations

### Arithmetic Operators
- `+` : Addition (supports multiple arguments)
- `-` : Subtraction (unary or multiple arguments)
- `*` : Multiplication (supports multiple arguments)
- `/` : Division (supports multiple arguments)
- `%` : Modulo (remainder)

### Mathematical Functions
- `sin`, `cos`, `tan` : Trigonometric functions
- `log`, `exp` : Logarithmic and exponential
- `abs` : Absolute value
- `floor`, `ceil` : Floor and ceiling functions

### Power and Roots
- `pow` : Power function (base^exponent)
- `sqrt` : Square root

### Constants
- `pi` : Mathematical constant π
- `e` : Natural logarithm base
- `inf` : Infinity

### Variable Management
- `set` : Assign value to variable
- Variables persist during session
- Use `:clear` command to reset variables

## Interactive Commands

When using interactive mode, you can use these special commands:

- `:help` - Show help information
- `:vars` - List all variables
- `:clear` - Clear all variables
- `:examples` - Run example expressions
- `:test` - Run test suite
- `:benchmark` - Run performance benchmark
- `:quit` - Exit

## How it works

The evaluator uses a recursive algorithm:

1. **Base cases**: Numbers and variables are returned as-is
2. **Recursive evaluation**: For lists, evaluate all arguments recursively
3. **Operator application**: Apply the operator to evaluated arguments
4. **Error handling**: Validate expressions and provide clear error messages

This approach naturally handles nested expressions and maintains a clean, functional design.

## Testing

The project includes a comprehensive test suite covering:

- Basic arithmetic operations
- Mathematical functions
- Error handling
- Variable management
- Performance testing
- Complex nested expressions

Run tests with:
```bash
make test
```

## Performance

The evaluator is designed for efficiency:
- Recursive evaluation with minimal overhead
- Hash table lookups for variables and constants
- Optimized for typical mathematical expressions
- Benchmarking tools included

## Why Lisp?

Lisp's design makes it perfect for this project:
- **Natural syntax**: Lists naturally represent expression trees
- **Recursive evaluation**: Lisp excels at recursive algorithms
- **Functional approach**: Pure functions ensure predictable behavior
- **Extensibility**: Easy to add new operators and functions
- **Interactive development**: REPL enables rapid testing

## Contributing

This project welcomes contributions! Areas for improvement:

- **New mathematical functions**: Add more specialized functions
- **Better parsing**: Support for infix notation
- **Optimization**: Performance improvements
- **Documentation**: More examples and tutorials
- **Testing**: Additional test cases

## Learning Outcomes

This project demonstrates:
- **Recursive algorithms**: Tree traversal and expression evaluation
- **Functional programming**: Pure functions and immutable data
- **Error handling**: Robust error management
- **Testing**: Comprehensive test-driven development
- **CLI development**: Command-line interface design
- **Performance optimization**: Benchmarking and profiling

## License

This project is open source. See the LICENSE file for details.

## Getting Help

- Check the examples in `examples/expressions.txt`
- Use `:help` in interactive mode
- Run `make help` for command overview
- Review the documentation in `docs/`

Perfect for learning Lisp, algorithms, and mathematical computation!
