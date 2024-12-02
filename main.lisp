(defun evaluate-expression (expr)
  "Evaluates a mathematical expression represented as a Lisp list."
  (cond
    ((numberp expr) expr)  ; Base case: if it's a number, return it.
    ((listp expr)          ; If it's a list, process recursively.
     (let ((op (car expr))
           (args (cdr expr)))
       (case op
         (+ (reduce #'+ (mapcar #'evaluate-expression args)))
         (- (reduce #'- (mapcar #'evaluate-expression args)))
         (* (reduce #'* (mapcar #'evaluate-expression args)))
         (/ (reduce #'/ (mapcar #'evaluate-expression args)))
         (t (error "Unknown operator: ~A" op)))))
    (t (error "Invalid expression: ~A" expr))))

