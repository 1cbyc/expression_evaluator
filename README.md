# Expression Evaluator

This is a simple mathematical expression evaluator I wrote in Common Lisp. This project just shows how to build a recursive expression parser that can handle basic arithmetic operations. Had an exam that made me do this 9 months ago.

## What it does

This program takes mathematical expressions written in Lisp format and calculates their results. For example:

```lisp
(+ 2 3)           ; Returns 5
(* 4 (+ 2 3))     ; Returns 20
(/ 10 2)          ; Returns 5
(- 10 3 2)        ; Returns 5
```

## How to use

1. Make sure you have a Common Lisp implementation installed (like SBCL, CCL, or CLISP)
2. Load the main.lisp file:
   ```bash
   sbcl --load main.lisp
   ```
3. Test expressions in your Lisp REPL:
   ```lisp
   (evaluate-expression '(+ 2 3))
   (evaluate-expression '(* 4 (+ 2 3)))
   ```

## How it works

The evaluator uses recursion to process expressions:
- If it's a number, return the number
- If it's a list, evaluate each argument and apply the operator

This approach naturally handles nested expressions and multiple arguments.

## Supported operations

- `+` : Addition
- `-` : Subtraction  
- `*` : Multiplication
- `/` : Division

## Why Lisp?

I use Lisp, Python and R for my doctorate. Besides, Lisp's list-based syntax makes it perfect for representing mathematical expressions. The recursive nature of the language also makes the evaluation algorithm straightforward to implement.

## Getting started

If you're new to Lisp, this project is a great way to learn about:
- Recursive algorithms
- Tree data structures
- Functional programming
- Expression parsing

### Know better and want to help me? Feel free to add new features like:
- More mathematical functions (sin, cos, etc.)
- Variable support
- Better error handling
- Command-line interface
