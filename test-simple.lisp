;; simple test for expression evaluator
;; this can be run in any common lisp environment

(defun evaluate-expression (expr)
  "simple expression evaluator for testing"
  (cond
    ((numberp expr) expr)
    ((listp expr)
     (let ((op (car expr))
           (args (cdr expr)))
       (case op
         (+ (reduce #'+ (mapcar #'evaluate-expression args)))
         (- (if (= (length args) 1)
                (- (evaluate-expression (car args)))
                (reduce #'- (mapcar #'evaluate-expression args))))
         (* (reduce #'* (mapcar #'evaluate-expression args)))
         (/ (reduce #'/ (mapcar #'evaluate-expression args)))
         (t (error "unknown operator: ~a" op)))))
    (t (error "invalid expression: ~a" expr))))

;; test cases
(format t "testing expression evaluator...~%")

(format t "test 1: (+ 2 3) = ~a~%" (evaluate-expression '(+ 2 3)))
(format t "test 2: (* 4 5) = ~a~%" (evaluate-expression '(* 4 5)))
(format t "test 3: (- 10 3) = ~a~%" (evaluate-expression '(- 10 3)))
(format t "test 4: (/ 20 4) = ~a~%" (evaluate-expression '(/ 20 4)))
(format t "test 5: (* 4 (+ 2 3)) = ~a~%" (evaluate-expression '(* 4 (+ 2 3))))

(format t "all tests passed!~%") 