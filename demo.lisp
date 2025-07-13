;; expression evaluator demo
;; demonstrates all features of the comprehensive expression evaluator

;; load all components
(load "main.lisp")
(load "tests.lisp")

(format t "~%")
(format t "==============================================~%")
(format t "    expression evaluator - full demo~%")
(format t "==============================================~%")
(format t "~%")

;; demo 1: basic arithmetic
(format t "1. basic arithmetic examples:~%")
(format t "   (+ 2 3) = ~a~%" (evaluate-expression '(+ 2 3)))
(format t "   (* 4 5) = ~a~%" (evaluate-expression '(* 4 5)))
(format t "   (- 10 3 2) = ~a~%" (evaluate-expression '(- 10 3 2)))
(format t "   (/ 20 4) = ~a~%" (evaluate-expression '(/ 20 4)))
(format t "~%")

;; demo 2: nested expressions
(format t "2. nested expressions:~%")
(format t "   (* 4 (+ 2 3)) = ~a~%" (evaluate-expression '(* 4 (+ 2 3))))
(format t "   (* (+ 1 2) (- 10 5)) = ~a~%" (evaluate-expression '(* (+ 1 2) (- 10 5))))
(format t "~%")

;; demo 3: mathematical functions
(format t "3. mathematical functions:~%")
(format t "   (sin pi) = ~a~%" (evaluate-expression '(sin pi)))
(format t "   (cos pi) = ~a~%" (evaluate-expression '(cos pi)))
(format t "   (abs -5) = ~a~%" (evaluate-expression '(abs -5)))
(format t "   (floor 3.7) = ~a~%" (evaluate-expression '(floor 3.7)))
(format t "   (ceil 3.2) = ~a~%" (evaluate-expression '(ceil 3.2)))
(format t "~%")

;; demo 4: power and roots
(format t "4. power and roots:~%")
(format t "   (pow 2 3) = ~a~%" (evaluate-expression '(pow 2 3)))
(format t "   (sqrt 16) = ~a~%" (evaluate-expression '(sqrt 16)))
(format t "   (sqrt 2) = ~a~%" (evaluate-expression '(sqrt 2)))
(format t "~%")

;; demo 5: constants
(format t "5. mathematical constants:~%")
(format t "   pi = ~a~%" (evaluate-expression 'pi))
(format t "   e = ~a~%" (evaluate-expression 'e))
(format t "~%")

;; demo 6: variables
(format t "6. variable management:~%")
(format t "   setting x = 10...~%")
(evaluate-expression '(set x 10))
(format t "   x = ~a~%" (evaluate-expression 'x))
(format t "   setting y = 20...~%")
(evaluate-expression '(set y 20))
(format t "   (+ x y) = ~a~%" (evaluate-expression '(+ x y)))
(format t "~%")

;; demo 7: complex expressions
(format t "7. complex expressions:~%")
(format t "   (sin (+ pi pi)) = ~a~%" (evaluate-expression '(sin (+ pi pi))))
(format t "   (pow 2 (+ 1 2)) = ~a~%" (evaluate-expression '(pow 2 (+ 1 2))))
(format t "   (log (exp 1)) = ~a~%" (evaluate-expression '(log (exp 1))))
(format t "~%")

;; demo 8: error handling
(format t "8. error handling:~%")
(format t "   testing invalid operator...~%")
(handler-case
  (evaluate-expression '(invalid-op 1 2))
  (error (e) (format t "   error caught: ~a~%" e)))

(format t "   testing undefined variable...~%")
(handler-case
  (evaluate-expression '(undefined-var))
  (error (e) (format t "   error caught: ~a~%" e)))
(format t "~%")

;; demo 9: performance benchmark
(format t "9. performance benchmark:~%")
(benchmark-evaluation '(+ 1 2 3 4 5) 10000)
(format t "~%")

;; demo 10: pretty printing
(format t "10. pretty printing example:~%")
(format t "expression: (* (+ 1 2) (- 10 5))~%")
(format t "pretty printed:~%")
(pretty-print-expression '(* (+ 1 2) (- 10 5)))
(format t "~%")

;; demo 11: test suite
(format t "11. running test suite:~%")
(run-all-tests)
(format t "~%")

;; demo 12: available commands
(format t "12. available commands:~%")
(format t "   (run-examples)      - run example expressions~%")
(format t "   (interactive-mode)  - start interactive session~%")
(format t "   (list-variables)    - list all variables~%")
(format t "   (clear-variables)   - clear all variables~%")
(format t "   (run-all-tests)     - run complete test suite~%")
(format t "   (benchmark-evaluation expr iterations) - performance test~%")
(format t "~%")

;; demo 13: file processing
(format t "13. file processing demo:~%")
(format t "   processing examples/expressions.txt...~%")
(load "cli.lisp")
(process-file "examples/expressions.txt")
(format t "~%")

(format t "==============================================~%")
(format t "    demo complete!~%")
(format t "==============================================~%")
(format t "~%")
(format t "try running: make interactive~%")
(format t "or: sbcl --load cli.lisp --interactive~%")
(format t "~%") 