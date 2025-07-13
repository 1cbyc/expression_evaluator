;; Command Line Interface for Expression Evaluator
;; Provides a user-friendly interface for evaluating expressions

;; Load the main evaluator
(load "main.lisp")

;; Command-line argument parsing
(defun parse-command-line-args ()
  "Parse command line arguments and return a list of options."
  (let ((args (cdr sb-ext:*posix-argv*))
        (options (make-hash-table :test 'equal)))
    
    ;; Set default options
    (setf (gethash "mode" options) "interactive")
    (setf (gethash "expression" options) nil)
    (setf (gethash "file" options) nil)
    (setf (gethash "test" options) nil)
    (setf (gethash "help" options) nil)
    
    ;; Parse arguments
    (do ((arg-list args (cdr arg-list)))
        ((null arg-list))
      (let ((arg (car arg-list)))
        (cond
          ((string= arg "--help") (setf (gethash "help" options) t))
          ((string= arg "--test") (setf (gethash "test" options) t))
          ((string= arg "--file")
           (setf (gethash "file" options) (cadr arg-list))
           (setf arg-list (cdr arg-list)))
          ((string= arg "--expr")
           (setf (gethash "expression" options) (cadr arg-list))
           (setf (gethash "mode" options) "single")
           (setf arg-list (cdr arg-list)))
          ((string= arg "--interactive") (setf (gethash "mode" options) "interactive"))
          ((and (not (gethash "expression" options))
                (not (gethash "file" options))
                (not (string= arg "--help"))
                (not (string= arg "--test")))
           ;; Assume it's an expression if no other mode is specified
           (setf (gethash "expression" options) arg)
           (setf (gethash "mode" options) "single")))))
    
    options))

;; Help message
(defun show-help ()
  "Display help information."
  (format t "~%Expression Evaluator - Command Line Interface~%")
  (format t "==============================================~%~%")
  (format t "Usage: sbcl --load cli.lisp [options] [expression]~%~%")
  (format t "Options:~%")
  (format t "  --help         Show this help message~%")
  (format t "  --interactive  Start interactive mode (default)~%")
  (format t "  --expr EXPR    Evaluate a single expression~%")
  (format t "  --file FILE    Evaluate expressions from a file~%")
  (format t "  --test         Run test suite~%~%")
  (format t "Examples:~%")
  (format t "  sbcl --load cli.lisp --expr \"(+ 2 3)\"~%")
  (format t "  sbcl --load cli.lisp --file expressions.txt~%")
  (format t "  sbcl --load cli.lisp --interactive~%")
  (format t "  sbcl --load cli.lisp --test~%~%")
  (format t "Expression format: Lisp prefix notation~%")
  (format t "  (+ 2 3)       ; Addition~%")
  (format t "  (* 4 (+ 2 3)) ; Nested expressions~%")
  (format t "  (sin pi)       ; Mathematical functions~%"))

;; File processing
(defun process-file (filename)
  "Process expressions from a file."
  (format t "Processing file: ~A~%~%" filename)
  
  (handler-case
    (with-open-file (stream filename :direction :input)
      (let ((line-number 0))
        (loop for line = (read-line stream nil :eof)
              until (eq line :eof)
              do (incf line-number)
              do (let ((trimmed-line (string-trim '(#\space #\tab #\newline) line)))
                   (unless (or (string= trimmed-line "")
                              (char= (char trimmed-line 0) #\;))
                     (format t "Line ~A: ~A~%" line-number trimmed-line)
                     (handler-case
                       (let ((expr (read-from-string trimmed-line)))
                         (format t "Result: ~A~%" (evaluate-expression expr)))
                       (error (e)
                         (format t "Error: ~A~%" e)))
                     (format t "~%"))))))
    (error (e)
      (format t "Error reading file: ~A~%" e))))

;; Single expression evaluation
(defun evaluate-single-expression (expr-string)
  "Evaluate a single expression from a string."
  (handler-case
    (let ((expr (read-from-string expr-string)))
      (format t "Expression: ~A~%" expr-string)
      (format t "Result: ~A~%" (evaluate-expression expr)))
    (error (e)
      (format t "Error parsing expression: ~A~%" e))))

;; Enhanced interactive mode
(defun enhanced-interactive-mode ()
  "Start an enhanced interactive session."
  (format t "~%=== Enhanced Expression Evaluator ===~%")
  (format t "Type expressions in Lisp format~%")
  (format t "Special commands:~%")
  (format t "  :help      - Show help~%")
  (format t "  :vars      - List variables~%")
  (format t "  :clear     - Clear variables~%")
  (format t "  :examples  - Run examples~%")
  (format t "  :test      - Run test suite~%")
  (format t "  :benchmark - Run performance benchmark~%")
  (format t "  :quit      - Exit~%~%")
  
  (loop
    (format t "> ")
    (force-output)
    (let ((input (read)))
      (cond
        ((eq input 'quit) (return (format t "Goodbye!~%")))
        ((eq input ':help)
         (show-help))
        ((eq input ':vars) 
         (format t "Variables: ~A~%" (list-variables)))
        ((eq input ':clear)
         (clear-variables)
         (format t "All variables cleared.~%"))
        ((eq input ':examples)
         (run-examples))
        ((eq input ':test)
         (load "tests.lisp")
         (run-all-tests))
        ((eq input ':benchmark)
         (format t "Enter expression to benchmark: ")
         (force-output)
         (let ((expr (read)))
           (format t "Enter number of iterations: ")
           (force-output)
           (let ((iterations (read)))
             (benchmark-evaluation expr iterations))))
        (t (handler-case
             (format t "Result: ~A~%" (evaluate-expression input))
             (error (e) (format t "Error: ~A~%" e))))))))

;; Main CLI function
(defun main ()
  "Main entry point for the command-line interface."
  (let ((options (parse-command-line-args)))
    
    (cond
      ((gethash "help" options)
       (show-help))
      
      ((gethash "test" options)
       (load "tests.lisp")
       (run-all-tests))
      
      ((gethash "file" options)
       (process-file (gethash "file" options)))
      
      ((gethash "expression" options)
       (evaluate-single-expression (gethash "expression" options)))
      
      (t
       (enhanced-interactive-mode)))))

;; Auto-run main if this file is loaded directly
#+sbcl
(when (member "--load" sb-ext:*posix-argv* :test 'string=)
  (main)) 