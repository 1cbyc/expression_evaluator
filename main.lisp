;; Expression Evaluator
;; A simple mathematical expression evaluator in Common Lisp
;;
;; This program demonstrates recursive expression evaluation
;; using Lisp's natural list-based syntax for representing
;; mathematical expressions.

(defun evaluate-expression (expr)
  "Evaluates a mathematical expression represented as a Lisp list.
   
   Examples:
   (evaluate-expression '(+ 2 3))     ; Returns 5
   (evaluate-expression '(* 4 (+ 2 3))) ; Returns 20
   (evaluate-expression '(/ 10 2))    ; Returns 5
   (evaluate-expression '(- 10 3 2))  ; Returns 5
   
   Supported operators: +, -, *, /
   "
  (cond
    ((numberp expr) expr)  ; Base case: if it's a number, return it
    ((listp expr)          ; If it's a list, process recursively
     (let ((op (car expr))
           (args (cdr expr)))
       (case op
         (+ (reduce #'+ (mapcar #'evaluate-expression args)))
         (- (reduce #'- (mapcar #'evaluate-expression args)))
         (* (reduce #'* (mapcar #'evaluate-expression args)))
         (/ (reduce #'/ (mapcar #'evaluate-expression args)))
         (t (error "Unknown operator: ~A" op)))))
    (t (error "Invalid expression: ~A" expr))))

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
  )))
    
    (dolist (example examples)
      (let ((description (car example))
            (expr (cadr example)))
        (format t "~%~A: ~S~%" description expr)
        (format t "Result: ~A~%" (evaluate-expression expr)))))

;; Interactive mode
(defun interactive-mode ()
  "Start an interactive session for evaluating expressions."
  (format t "~%=== Interactive Expression Evaluator ===~%")
  (format t "Enter expressions in Lisp format (e.g., (+ 2 3))~%")
  (format t "Type 'quit' to exit~%~%")
  
  (loop
    (format t "> ")
    (force-output)
    (let ((input (read)))
      (cond
        ((eq input 'quit) (return (format t "Goodbye!~%")))
        (t (handler-case
             (format t "Result: ~A~%" (evaluate-expression input))
             (error (e) (format t "Error: ~A~%" e))))))))

;; Load-time message
(format t "Expression Evaluator loaded successfully!~%")
(format t "Use (run-examples) to see examples~%")
(format t "Use (interactive-mode) to start interactive session~%")