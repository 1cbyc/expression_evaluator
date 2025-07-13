;; Test Suite for Expression Evaluator
;; Comprehensive testing of all features

;; Load the main evaluator
(load "main.lisp")

;; Test utilities
(defun assert-equal (expected actual &optional message)
  "Assert that expected equals actual."
  (unless (equal expected actual)
    (error "Test failed~@[ ~A~]: expected ~A, got ~A" message expected actual)))

(defun assert-approx (expected actual tolerance &optional message)
  "Assert that expected approximately equals actual within tolerance."
  (unless (<= (abs (- expected actual)) tolerance)
    (error "Test failed~@[ ~A~]: expected ~A, got ~A (tolerance: ~A)" 
           message expected actual tolerance)))

(defun run-test (name test-fn)
  "Run a test and report success/failure."
  (format t "Running test: ~A~%" name)
  (handler-case
    (funcall test-fn)
    (error (e) 
      (format t "  ❌ FAILED: ~A~%" e)
      (return-from run-test nil)))
  (format t "  ✅ PASSED~%")
  t)

;; Test categories
(defun test-basic-arithmetic ()
  "Test basic arithmetic operations."
  (assert-equal 5 (evaluate-expression '(+ 2 3)) "Simple addition")
  (assert-equal 6 (evaluate-expression '(* 2 3)) "Simple multiplication")
  (assert-equal 1 (evaluate-expression '(- 4 3)) "Simple subtraction")
  (assert-equal 2 (evaluate-expression '(/ 6 3)) "Simple division")
  (assert-equal 5 (evaluate-expression '(- 10 3 2)) "Multiple arguments")
  (assert-equal 20 (evaluate-expression '(* 4 (+ 2 3))) "Nested expressions"))

(defun test-mathematical-functions ()
  "Test mathematical functions."
  (assert-approx 0.0 (evaluate-expression '(sin pi)) 0.001 "Sine of pi")
  (assert-approx -1.0 (evaluate-expression '(cos pi)) 0.001 "Cosine of pi")
  (assert-approx 1.0 (evaluate-expression '(exp 0)) 0.001 "Exponential of 0")
  (assert-approx 0.0 (evaluate-expression '(log 1)) 0.001 "Log of 1")
  (assert-equal 5 (evaluate-expression '(abs -5)) "Absolute value")
  (assert-equal 3 (evaluate-expression '(floor 3.7)) "Floor function")
  (assert-equal 4 (evaluate-expression '(ceil 3.2)) "Ceiling function"))

(defun test-power-and-root ()
  "Test power and root functions."
  (assert-equal 8 (evaluate-expression '(pow 2 3)) "Power function")
  (assert-equal 4 (evaluate-expression '(sqrt 16)) "Square root")
  (assert-approx 2.0 (evaluate-expression '(sqrt 4)) 0.001 "Square root of 4")
  (assert-approx 1.414 (evaluate-expression '(sqrt 2)) 0.001 "Square root of 2"))

(defun test-constants ()
  "Test mathematical constants."
  (assert-approx 3.14159 (evaluate-expression 'pi) 0.0001 "Pi constant")
  (assert-approx 2.71828 (evaluate-expression 'e) 0.0001 "E constant"))

(defun test-variables ()
  "Test variable assignment and retrieval."
  (clear-variables) ; Start with clean environment
  (assert-equal 10 (evaluate-expression '(set x 10)) "Variable assignment")
  (assert-equal 10 (evaluate-expression 'x) "Variable retrieval")
  (assert-equal 20 (evaluate-expression '(set y 20)) "Second variable")
  (assert-equal 30 (evaluate-expression '(+ x y)) "Variable arithmetic"))

(defun test-error-handling ()
  "Test error handling for invalid expressions."
  (handler-case
    (evaluate-expression '(invalid-op 1 2))
    (error (e) 
      (assert (search "Unknown operator" (format nil "~A" e)) "Unknown operator error")))
  
  (handler-case
    (evaluate-expression '(undefined-var))
    (error (e)
      (assert (search "Undefined variable" (format nil "~A" e)) "Undefined variable error")))
  
  (handler-case
    (evaluate-expression '("not-a-symbol" 1 2))
    (error (e)
      (assert (search "Operator must be a symbol" (format nil "~A" e)) "Invalid operator error"))))

(defun test-complex-expressions ()
  "Test complex nested expressions."
  (assert-equal 25 (evaluate-expression '(* (+ 2 3) (+ 2 3))) "Complex nested")
  (assert-approx 1.0 (evaluate-expression '(sin (+ pi pi))) 0.001 "Trigonometric with constants")
  (assert-equal 8 (evaluate-expression '(pow 2 (+ 1 2))) "Power with nested addition"))

(defun test-performance ()
  "Test performance with repeated evaluations."
  (let ((expr '(+ 1 2 3 4 5)))
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 1000)
        (evaluate-expression expr))
      (let ((end-time (get-internal-real-time))
            (total-time (/ (- end-time start-time) internal-time-units-per-second)))
        (assert (< total-time 1.0) "Performance test: should complete 1000 evaluations in under 1 second")))))

;; Main test runner
(defun run-all-tests ()
  "Run all tests and report results."
  (format t "~%=== Running Expression Evaluator Tests ===~%")
  
  (let ((tests '(
    ("Basic Arithmetic" test-basic-arithmetic)
    ("Mathematical Functions" test-mathematical-functions)
    ("Power and Root" test-power-and-root)
    ("Constants" test-constants)
    ("Variables" test-variables)
    ("Error Handling" test-error-handling)
    ("Complex Expressions" test-complex-expressions)
    ("Performance" test-performance)
  ))
        (passed 0)
        (total 0))
    
    (dolist (test tests)
      (incf total)
      (if (run-test (car test) (cadr test))
          (incf passed)))
    
    (format t "~%=== Test Results ===~%")
    (format t "Passed: ~A/~A tests~%" passed total)
    (if (= passed total)
        (format t "🎉 All tests passed!~%")
        (format t "❌ ~A test(s) failed~%" (- total passed)))))

;; Individual test runners for debugging
(defun test-specific (test-name)
  "Run a specific test by name."
  (case test-name
    (arithmetic (test-basic-arithmetic))
    (functions (test-mathematical-functions))
    (power (test-power-and-root))
    (constants (test-constants))
    (variables (test-variables))
    (errors (test-error-handling))
    (complex (test-complex-expressions))
    (performance (test-performance))
    (t (format t "Unknown test: ~A~%" test-name))))

;; Load-time message
(format t "Test suite loaded. Use (run-all-tests) to run all tests.~%") 