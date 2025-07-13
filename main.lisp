;; expression evaluator
;; a comprehensive mathematical expression evaluator in common lisp
;;
;; this program demonstrates recursive expression evaluation
;; using lisp's natural list-based syntax for representing
;; mathematical expressions.

;; global environment for variables
(defvar *environment* (make-hash-table :test 'equal)
  "global environment for storing variables and functions.")

;; mathematical constants
(defvar *constants* 
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash 'pi table) 3.14159265359)
    (setf (gethash 'e table) 2.71828182846)
    (setf (gethash 'inf table) most-positive-double-float)
    table)
  "mathematical constants.")

;; error handling utilities
(defun expression-error (format-string &rest args)
  "signal a custom error for expression evaluation."
  (error "expression error: ~?" format-string args))

(defun validate-expression (expr)
  "validate that an expression is well-formed."
  (cond
    ((numberp expr) t)
    ((symbolp expr) t)
    ((listp expr)
     (and (not (null expr))
          (symbolp (car expr))
          (every #'validate-expression (cdr expr))))
    (t nil)))

;; enhanced expression evaluator with more features
(defun evaluate-expression (expr)
  "evaluates a mathematical expression represented as a lisp list.
   
   examples:
   (evaluate-expression '(+ 2 3))     ; returns 5
   (evaluate-expression '(* 4 (+ 2 3))) ; returns 20
   (evaluate-expression '(/ 10 2))    ; returns 5
   (evaluate-expression '(- 10 3 2))  ; returns 5
   (evaluate-expression '(sin pi))     ; returns 0
   (evaluate-expression '(pow 2 3))   ; returns 8
   
   supported operators: +, -, *, /, %, pow, sqrt
   supported functions: sin, cos, tan, log, exp, abs, floor, ceil
   supported constants: pi, e, inf
   "
  (cond
    ;; base cases
    ((numberp expr) expr)
    ((symbolp expr) 
     (cond
       ((gethash expr *environment*) (gethash expr *environment*))
       ((gethash expr *constants*) (gethash expr *constants*))
       (t (expression-error "undefined variable: ~a" expr))))
    
    ;; list expressions
    ((listp expr)
     (let ((op (car expr))
           (args (cdr expr)))
       
       ;; validate expression structure
       (unless (symbolp op)
         (expression-error "operator must be a symbol: ~a" op))
       
       ;; handle different operators
       (case op
         ;; basic arithmetic
         (+ (reduce #'+ (mapcar #'evaluate-expression args)))
         (- (if (= (length args) 1)
                (- (evaluate-expression (car args)))
                (reduce #'- (mapcar #'evaluate-expression args))))
         (* (reduce #'* (mapcar #'evaluate-expression args)))
         (/ (reduce #'/ (mapcar #'evaluate-expression args)))
         (% (mod (evaluate-expression (car args)) 
                 (evaluate-expression (cadr args))))
         
         ;; power and root
         (pow (expt (evaluate-expression (car args))
                    (evaluate-expression (cadr args))))
         (sqrt (sqrt (evaluate-expression (car args))))
         
         ;; trigonometric functions
         (sin (sin (evaluate-expression (car args))))
         (cos (cos (evaluate-expression (car args))))
         (tan (tan (evaluate-expression (car args))))
         
         ;; logarithmic and exponential
         (log (log (evaluate-expression (car args))))
         (exp (exp (evaluate-expression (car args))))
         
         ;; other mathematical functions
         (abs (abs (evaluate-expression (car args))))
         (floor (floor (evaluate-expression (car args))))
         (ceil (ceiling (evaluate-expression (car args))))
         
         ;; variable assignment
         (set (let ((var (car args))
                   (val (evaluate-expression (cadr args))))
               (setf (gethash var *environment*) val)
               val))
         
         ;; unknown operator
         (t (expression-error "unknown operator: ~a" op)))))
    
    ;; invalid expression
    (t (expression-error "invalid expression: ~a" expr))))

;; variable management
(defun set-variable (name value)
  "set a variable in the global environment."
  (setf (gethash name *environment*) value)
  value)

(defun get-variable (name)
  "get a variable from the global environment."
  (gethash name *environment*))

(defun list-variables ()
  "list all variables in the environment."
  (loop for key being the hash-keys of *environment*
        collect key))

(defun clear-variables ()
  "clear all variables from the environment."
  (clrhash *environment*))

;; expression parsing utilities
(defun parse-infix (infix-expr)
  "convert infix notation to prefix (lisp) notation.
   basic implementation - can be extended for complex expressions."
  (cond
    ((atom infix-expr) infix-expr)
    ((= (length infix-expr) 1) (parse-infix (car infix-expr)))
    ((= (length infix-expr) 3)
     (let ((left (car infix-expr))
           (op (cadr infix-expr))
           (right (caddr infix-expr)))
       (list op (parse-infix left) (parse-infix right))))
    (t infix-expr)))

;; pretty printing
(defun pretty-print-expression (expr &optional (depth 0))
  "pretty print an expression with proper indentation."
  (let ((indent (make-string (* depth 2) :initial-element #\space)))
    (cond
      ((atom expr) (format t "~a~a~%" indent expr))
      (t (format t "~a(~a~%" indent (car expr))
         (dolist (arg (cdr expr))
           (pretty-print-expression arg (+ depth 1)))
         (format t "~a)~%" indent)))))

;; example usage and testing
(defun run-examples ()
  "run some example expressions to demonstrate the evaluator."
  (format t "~%=== expression evaluator examples ===~%")
  
  (let ((examples '(
    ("simple addition" (+ 2 3))
    ("simple multiplication" (* 4 5))
    ("nested expression" (* 4 (+ 2 3)))
    ("multiple arguments" (- 10 3 2))
    ("division" (/ 20 4))
    ("complex nested" (* (+ 1 2) (- 10 5)))
    ("power function" (pow 2 3))
    ("square root" (sqrt 16))
    ("trigonometric" (sin pi))
    ("logarithm" (log (exp 1)))
    ("absolute value" (abs -5))
    ("floor function" (floor 3.7))
    ("ceiling function" (ceil 3.2))
  )))
    
    (dolist (example examples)
      (let ((description (car example))
            (expr (cadr example)))
        (format t "~%~a: ~s~%" description expr)
        (handler-case
          (format t "result: ~a~%" (evaluate-expression expr))
          (error (e) (format t "error: ~a~%" e)))))))

;; interactive mode with enhanced features
(defun interactive-mode ()
  "start an interactive session for evaluating expressions."
  (format t "~%=== interactive expression evaluator ===~%")
  (format t "enter expressions in lisp format (e.g., (+ 2 3))~%")
  (format t "special commands:~%")
  (format t "  :vars    - list all variables~%")
  (format t "  :clear   - clear all variables~%")
  (format t "  :examples - run examples~%")
  (format t "  :quit    - exit~%~%")
  
  (loop
    (format t "> ")
    (force-output)
    (let ((input (read)))
      (cond
        ((eq input 'quit) (return (format t "goodbye!~%")))
        ((eq input ':vars) 
         (format t "variables: ~a~%" (list-variables)))
        ((eq input ':clear)
         (clear-variables)
         (format t "all variables cleared.~%"))
        ((eq input ':examples)
         (run-examples))
        (t (handler-case
             (format t "result: ~a~%" (evaluate-expression input))
             (error (e) (format t "error: ~a~%" e))))))))

;; performance testing
(defun benchmark-evaluation (expr iterations)
  "benchmark expression evaluation performance."
  (let ((start-time (get-internal-real-time)))
    (dotimes (i iterations)
      (evaluate-expression expr))
    (let ((end-time (get-internal-real-time))
          (total-time (/ (- end-time start-time) internal-time-units-per-second)))
      (format t "~%benchmark results:~%")
      (format t "expression: ~s~%" expr)
      (format t "iterations: ~a~%" iterations)
      (format t "total time: ~,3f seconds~%" total-time)
      (format t "average time: ~,6f seconds per evaluation~%" (/ total-time iterations)))))

;; load-time message
(format t "expression evaluator loaded successfully!~%")
(format t "use (run-examples) to see examples~%")
(format t "use (interactive-mode) to start interactive session~%")
(format t "use (benchmark-evaluation expr iterations) for performance testing~%")