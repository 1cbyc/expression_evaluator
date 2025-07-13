;; test suite for expression evaluator
;; comprehensive testing of all features

;; load the main evaluator
(load "main.lisp")

;; test utilities
(defun assert-equal (expected actual &optional message)
  "assert that expected equals actual."
  (unless (equal expected actual)
    (error "test failed~@[ ~a~]: expected ~a, got ~a" message expected actual)))

(defun assert-approx (expected actual tolerance &optional message)
  "assert that expected approximately equals actual within tolerance."
  (unless (<= (abs (- expected actual)) tolerance)
    (error "test failed~@[ ~a~]: expected ~a, got ~a (tolerance: ~a)" 
           message expected actual tolerance)))

(defun run-test (name test-fn)
  "run a test and report success/failure."
  (format t "running test: ~a~%" name)
  (handler-case
    (funcall test-fn)
    (error (e) 
      (format t "  failed: ~a~%" e)
      (return-from run-test nil)))
  (format t "  passed~%")
  t)

;; test categories
(defun test-basic-arithmetic ()
  "test basic arithmetic operations."
  (assert-equal 5 (evaluate-expression '(+ 2 3)) "simple addition")
  (assert-equal 6 (evaluate-expression '(* 2 3)) "simple multiplication")
  (assert-equal 1 (evaluate-expression '(- 4 3)) "simple subtraction")
  (assert-equal 2 (evaluate-expression '(/ 6 3)) "simple division")
  (assert-equal 5 (evaluate-expression '(- 10 3 2)) "multiple arguments")
  (assert-equal 20 (evaluate-expression '(* 4 (+ 2 3))) "nested expressions"))

(defun test-mathematical-functions ()
  "test mathematical functions."
  (assert-approx 0.0 (evaluate-expression '(sin pi)) 0.001 "sine of pi")
  (assert-approx -1.0 (evaluate-expression '(cos pi)) 0.001 "cosine of pi")
  (assert-approx 1.0 (evaluate-expression '(exp 0)) 0.001 "exponential of 0")
  (assert-approx 0.0 (evaluate-expression '(log 1)) 0.001 "log of 1")
  (assert-equal 5 (evaluate-expression '(abs -5)) "absolute value")
  (assert-equal 3 (evaluate-expression '(floor 3.7)) "floor function")
  (assert-equal 4 (evaluate-expression '(ceil 3.2)) "ceiling function"))

(defun test-power-and-root ()
  "test power and root functions."
  (assert-equal 8 (evaluate-expression '(pow 2 3)) "power function")
  (assert-equal 4 (evaluate-expression '(sqrt 16)) "square root")
  (assert-approx 2.0 (evaluate-expression '(sqrt 4)) 0.001 "square root of 4")
  (assert-approx 1.414 (evaluate-expression '(sqrt 2)) 0.001 "square root of 2"))

(defun test-constants ()
  "test mathematical constants."
  (assert-approx 3.14159 (evaluate-expression 'pi) 0.0001 "pi constant")
  (assert-approx 2.71828 (evaluate-expression 'e) 0.0001 "e constant"))

(defun test-variables ()
  "test variable assignment and retrieval."
  (clear-variables) ; start with clean environment
  (assert-equal 10 (evaluate-expression '(set x 10)) "variable assignment")
  (assert-equal 10 (evaluate-expression 'x) "variable retrieval")
  (assert-equal 20 (evaluate-expression '(set y 20)) "second variable")
  (assert-equal 30 (evaluate-expression '(+ x y)) "variable arithmetic"))

(defun test-error-handling ()
  "test error handling for invalid expressions."
  (handler-case
    (evaluate-expression '(invalid-op 1 2))
    (error (e) 
      (assert (search "unknown operator" (format nil "~a" e)) "unknown operator error")))
  
  (handler-case
    (evaluate-expression '(undefined-var))
    (error (e)
      (assert (search "undefined variable" (format nil "~a" e)) "undefined variable error")))
  
  (handler-case
    (evaluate-expression '("not-a-symbol" 1 2))
    (error (e)
      (assert (search "operator must be a symbol" (format nil "~a" e)) "invalid operator error"))))

(defun test-complex-expressions ()
  "test complex nested expressions."
  (assert-equal 25 (evaluate-expression '(* (+ 2 3) (+ 2 3))) "complex nested")
  (assert-approx 1.0 (evaluate-expression '(sin (+ pi pi))) 0.001 "trigonometric with constants")
  (assert-equal 8 (evaluate-expression '(pow 2 (+ 1 2))) "power with nested addition"))

(defun test-performance ()
  "test performance with repeated evaluations."
  (let ((expr '(+ 1 2 3 4 5)))
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 1000)
        (evaluate-expression expr))
      (let ((end-time (get-internal-real-time))
            (total-time (/ (- end-time start-time) internal-time-units-per-second)))
        (assert (< total-time 1.0) "performance test: should complete 1000 evaluations in under 1 second")))))

;; main test runner
(defun run-all-tests ()
  "run all tests and report results."
  (format t "~%=== running expression evaluator tests ===~%")
  
  (let ((tests '(
    ("basic arithmetic" test-basic-arithmetic)
    ("mathematical functions" test-mathematical-functions)
    ("power and root" test-power-and-root)
    ("constants" test-constants)
    ("variables" test-variables)
    ("error handling" test-error-handling)
    ("complex expressions" test-complex-expressions)
    ("performance" test-performance)
  ))
        (passed 0)
        (total 0))
    
    (dolist (test tests)
      (incf total)
      (if (run-test (car test) (cadr test))
          (incf passed)))
    
    (format t "~%=== test results ===~%")
    (format t "passed: ~a/~a tests~%" passed total)
    (if (= passed total)
        (format t "all tests passed!~%")
        (format t "~a test(s) failed~%" (- total passed)))))

;; individual test runners for debugging
(defun test-specific (test-name)
  "run a specific test by name."
  (case test-name
    (arithmetic (test-basic-arithmetic))
    (functions (test-mathematical-functions))
    (power (test-power-and-root))
    (constants (test-constants))
    (variables (test-variables))
    (errors (test-error-handling))
    (complex (test-complex-expressions))
    (performance (test-performance))
    (t (format t "unknown test: ~a~%" test-name))))

;; load-time message
(format t "test suite loaded. use (run-all-tests) to run all tests.~%") 