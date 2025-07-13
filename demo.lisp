;; Expression Evaluator Demo
;; Demonstrates all features of the comprehensive expression evaluator

;; Load all components
(load "main.lisp")
(load "tests.lisp")

(format t "~%")
(format t "==============================================~%")
(format t "    Expression Evaluator - Full Demo~%")
(format t "==============================================~%")
(format t "~%")

;; Demo 1: Basic Arithmetic
(format t "1. Basic Arithmetic Examples:~%")
(format t "   (+ 2 3) = ~A~%" (evaluate-expression '(+ 2 3)))
(format t "   (* 4 5) = ~A~%" (evaluate-expression '(* 4 5)))
(format t "   (- 10 3 2) = ~A~%" (evaluate-expression '(- 10 3 2)))
(format t "   (/ 20 4) = ~A~%" (evaluate-expression '(/ 20 4)))
(format t "~%")

;; Demo 2: Nested Expressions
(format t "2. Nested Expressions:~%")
(format t "   (* 4 (+ 2 3)) = ~A~%" (evaluate-expression '(* 4 (+ 2 3))))
(format t "   (* (+ 1 2) (- 10 5)) = ~A~%" (evaluate-expression '(* (+ 1 2) (- 10 5))))
(format t "~%")

;; Demo 3: Mathematical Functions
(format t "3. Mathematical Functions:~%")
(format t "   (sin pi) = ~A~%" (evaluate-expression '(sin pi)))
(format t "   (cos pi) = ~A~%" (evaluate-expression '(cos pi)))
(format t "   (abs -5) = ~A~%" (evaluate-expression '(abs -5)))
(format t "   (floor 3.7) = ~A~%" (evaluate-expression '(floor 3.7)))
(format t "   (ceil 3.2) = ~A~%" (evaluate-expression '(ceil 3.2)))
(format t "~%")

;; Demo 4: Power and Roots
(format t "4. Power and Roots:~%")
(format t "   (pow 2 3) = ~A~%" (evaluate-expression '(pow 2 3)))
(format t "   (sqrt 16) = ~A~%" (evaluate-expression '(sqrt 16)))
(format t "   (sqrt 2) = ~A~%" (evaluate-expression '(sqrt 2)))
(format t "~%")

;; Demo 5: Constants
(format t "5. Mathematical Constants:~%")
(format t "   pi = ~A~%" (evaluate-expression 'pi))
(format t "   e = ~A~%" (evaluate-expression 'e))
(format t "~%")

;; Demo 6: Variables
(format t "6. Variable Management:~%")
(format t "   Setting x = 10...~%")
(evaluate-expression '(set x 10))
(format t "   x = ~A~%" (evaluate-expression 'x))
(format t "   Setting y = 20...~%")
(evaluate-expression '(set y 20))
(format t "   (+ x y) = ~A~%" (evaluate-expression '(+ x y)))
(format t "~%")

;; Demo 7: Complex Expressions
(format t "7. Complex Expressions:~%")
(format t "   (sin (+ pi pi)) = ~A~%" (evaluate-expression '(sin (+ pi pi))))
(format t "   (pow 2 (+ 1 2)) = ~A~%" (evaluate-expression '(pow 2 (+ 1 2))))
(format t "   (log (exp 1)) = ~A~%" (evaluate-expression '(log (exp 1))))
(format t "~%")

;; Demo 8: Error Handling
(format t "8. Error Handling:~%")
(format t "   Testing invalid operator...~%")
(handler-case
  (evaluate-expression '(invalid-op 1 2))
  (error (e) (format t "   Error caught: ~A~%" e)))

(format t "   Testing undefined variable...~%")
(handler-case
  (evaluate-expression '(undefined-var))
  (error (e) (format t "   Error caught: ~A~%" e)))
(format t "~%")

;; Demo 9: Performance Benchmark
(format t "9. Performance Benchmark:~%")
(benchmark-evaluation '(+ 1 2 3 4 5) 10000)
(format t "~%")

;; Demo 10: Pretty Printing
(format t "10. Pretty Printing Example:~%")
(format t "Expression: (* (+ 1 2) (- 10 5))~%")
(format t "Pretty printed:~%")
(pretty-print-expression '(* (+ 1 2) (- 10 5)))
(format t "~%")

;; Demo 11: Test Suite
(format t "11. Running Test Suite:~%")
(run-all-tests)
(format t "~%")

;; Demo 12: Available Commands
(format t "12. Available Commands:~%")
(format t "   (run-examples)      - Run example expressions~%")
(format t "   (interactive-mode)  - Start interactive session~%")
(format t "   (list-variables)    - List all variables~%")
(format t "   (clear-variables)   - Clear all variables~%")
(format t "   (run-all-tests)     - Run complete test suite~%")
(format t "   (benchmark-evaluation expr iterations) - Performance test~%")
(format t "~%")

;; Demo 13: File Processing
(format t "13. File Processing Demo:~%")
(format t "   Processing examples/expressions.txt...~%")
(load "cli.lisp")
(process-file "examples/expressions.txt")
(format t "~%")

(format t "==============================================~%")
(format t "    Demo Complete!~%")
(format t "==============================================~%")
(format t "~%")
(format t "Try running: make interactive~%")
(format t "Or: sbcl --load cli.lisp --interactive~%")
(format t "~%") 