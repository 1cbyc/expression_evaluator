;; Expression Evaluator
;; A comprehensive mathematical expression evaluator in Common Lisp
;;
;; This program demonstrates recursive expression evaluation
;; using Lisp's natural list-based syntax for representing
;; mathematical expressions.

;; Global environment for variables
(defvar *environment* (make-hash-table :test 'equal)
  "Global environment for storing variables and functions.")

;; Mathematical constants
(defvar *constants* 
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash 'pi table) 3.14159265359)
    (setf (gethash 'e table) 2.71828182846)
    (setf (gethash 'inf table) most-positive-double-float)
    table)
  "Mathematical constants.")

;; Error handling utilities
(defun expression-error (format-string &rest args)
  "Signal a custom error for expression evaluation."
  (error "Expression Error: ~?" format-string args))

(defun validate-expression (expr)
  "Validate that an expression is well-formed."
  (cond
    ((numberp expr) t)
    ((symbolp expr) t)
    ((listp expr)
     (and (not (null expr))
          (symbolp (car expr))
          (every #'validate-expression (cdr expr))))
    (t nil)))

;; Enhanced expression evaluator with more features
(defun evaluate-expression (expr)
  "Evaluates a mathematical expression represented as a Lisp list.
   
   Examples:
   (evaluate-expression '(+ 2 3))     ; Returns 5
   (evaluate-expression '(* 4 (+ 2 3))) ; Returns 20
   (evaluate-expression '(/ 10 2))    ; Returns 5
   (evaluate-expression '(- 10 3 2))  ; Returns 5
   (evaluate-expression '(sin pi))     ; Returns 0
   (evaluate-expression '(pow 2 3))   ; Returns 8
   
   Supported operators: +, -, *, /, %, pow, sqrt
   Supported functions: sin, cos, tan, log, exp, abs, floor, ceil
   Supported constants: pi, e, inf
   "
  (cond
    ;; Base cases
    ((numberp expr) expr)
    ((symbolp expr) 
     (cond
       ((gethash expr *environment*) (gethash expr *environment*))
       ((gethash expr *constants*) (gethash expr *constants*))
       (t (expression-error "Undefined variable: ~A" expr))))
    
    ;; List expressions
    ((listp expr)
     (let ((op (car expr))
           (args (cdr expr)))
       
       ;; Validate expression structure
       (unless (symbolp op)
         (expression-error "Operator must be a symbol: ~A" op))
       
       ;; Handle different operators
       (case op
         ;; Basic arithmetic
         (+ (reduce #'+ (mapcar #'evaluate-expression args)))
         (- (if (= (length args) 1)
                (- (evaluate-expression (car args)))
                (reduce #'- (mapcar #'evaluate-expression args))))
         (* (reduce #'* (mapcar #'evaluate-expression args)))
         (/ (reduce #'/ (mapcar #'evaluate-expression args)))
         (% (mod (evaluate-expression (car args)) 
                 (evaluate-expression (cadr args))))
         
         ;; Power and root
         (pow (expt (evaluate-expression (car args))
                    (evaluate-expression (cadr args))))
         (sqrt (sqrt (evaluate-expression (car args))))
         
         ;; Trigonometric functions
         (sin (sin (evaluate-expression (car args))))
         (cos (cos (evaluate-expression (car args))))
         (tan (tan (evaluate-expression (car args))))
         
         ;; Logarithmic and exponential
         (log (log (evaluate-expression (car args))))
         (exp (exp (evaluate-expression (car args))))
         
         ;; Other mathematical functions
         (abs (abs (evaluate-expression (car args))))
         (floor (floor (evaluate-expression (car args))))
         (ceil (ceiling (evaluate-expression (car args))))
         
         ;; Variable assignment
         (set (let ((var (car args))
                   (val (evaluate-expression (cadr args))))
               (setf (gethash var *environment*) val)
               val))
         
         ;; Unknown operator
         (t (expression-error "Unknown operator: ~A" op)))))
    
    ;; Invalid expression
    (t (expression-error "Invalid expression: ~A" expr))))

;; Variable management
(defun set-variable (name value)
  "Set a variable in the global environment."
  (setf (gethash name *environment*) value)
  value)

(defun get-variable (name)
  "Get a variable from the global environment."
  (gethash name *environment*))

(defun list-variables ()
  "List all variables in the environment."
  (loop for key being the hash-keys of *environment*
        collect key))

(defun clear-variables ()
  "Clear all variables from the environment."
  (clrhash *environment*))

;; Expression parsing utilities
(defun parse-infix (infix-expr)
  "Convert infix notation to prefix (Lisp) notation.
   Basic implementation - can be extended for complex expressions."
  (cond
    ((atom infix-expr) infix-expr)
    ((= (length infix-expr) 1) (parse-infix (car infix-expr)))
    ((= (length infix-expr) 3)
     (let ((left (car infix-expr))
           (op (cadr infix-expr))
           (right (caddr infix-expr)))
       (list op (parse-infix left) (parse-infix right))))
    (t infix-expr)))

;; Pretty printing
(defun pretty-print-expression (expr &optional (depth 0))
  "Pretty print an expression with proper indentation."
  (let ((indent (make-string (* depth 2) :initial-element #\space)))
    (cond
      ((atom expr) (format t "~A~A~%" indent expr))
      (t (format t "~A(~A~%" indent (car expr))
         (dolist (arg (cdr expr))
           (pretty-print-expression arg (+ depth 1)))
         (format t "~A)~%" indent)))))

;; Example usage and testing
(defun run-examples ()
  "Run some example expressions to demonstrate the evaluator."
  (format t "~%=== Expression Evaluator Examples ===~%")
  
  (let ((examples '(
    ("Simple addition" (+ 2 3))
    ("Simple multiplication" (* 4 5))
    ("Nested expression" (* 4 (+ 2 3)))
    ("Multiple arguments" (- 10 3 2))
    ("Division" (/ 20 4))
    ("Complex nested" (* (+ 1 2) (- 10 5)))
    ("Power function" (pow 2 3))
    ("Square root" (sqrt 16))
    ("Trigonometric" (sin pi))
    ("Logarithm" (log (exp 1)))
    ("Absolute value" (abs -5))
    ("Floor function" (floor 3.7))
    ("Ceiling function" (ceil 3.2))
  )))
    
    (dolist (example examples)
      (let ((description (car example))
            (expr (cadr example)))
        (format t "~%~A: ~S~%" description expr)
        (handler-case
          (format t "Result: ~A~%" (evaluate-expression expr))
          (error (e) (format t "Error: ~A~%" e)))))))

;; Interactive mode with enhanced features
(defun interactive-mode ()
  "Start an interactive session for evaluating expressions."
  (format t "~%=== Interactive Expression Evaluator ===~%")
  (format t "Enter expressions in Lisp format (e.g., (+ 2 3))~%")
  (format t "Special commands:~%")
  (format t "  :vars    - List all variables~%")
  (format t "  :clear   - Clear all variables~%")
  (format t "  :examples - Run examples~%")
  (format t "  :quit    - Exit~%~%")
  
  (loop
    (format t "> ")
    (force-output)
    (let ((input (read)))
      (cond
        ((eq input 'quit) (return (format t "Goodbye!~%")))
        ((eq input ':vars) 
         (format t "Variables: ~A~%" (list-variables)))
        ((eq input ':clear)
         (clear-variables)
         (format t "All variables cleared.~%"))
        ((eq input ':examples)
         (run-examples))
        (t (handler-case
             (format t "Result: ~A~%" (evaluate-expression input))
             (error (e) (format t "Error: ~A~%" e))))))))

;; Performance testing
(defun benchmark-evaluation (expr iterations)
  "Benchmark expression evaluation performance."
  (let ((start-time (get-internal-real-time)))
    (dotimes (i iterations)
      (evaluate-expression expr))
    (let ((end-time (get-internal-real-time))
          (total-time (/ (- end-time start-time) internal-time-units-per-second)))
      (format t "~%Benchmark Results:~%")
      (format t "Expression: ~S~%" expr)
      (format t "Iterations: ~A~%" iterations)
      (format t "Total time: ~,3F seconds~%" total-time)
      (format t "Average time: ~,6F seconds per evaluation~%" (/ total-time iterations)))))

;; Load-time message
(format t "Expression Evaluator loaded successfully!~%")
(format t "Use (run-examples) to see examples~%")
(format t "Use (interactive-mode) to start interactive session~%")
(format t "Use (benchmark-evaluation expr iterations) for performance testing~%")