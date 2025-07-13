;; command line interface for expression evaluator
;; provides a user-friendly interface for evaluating expressions

;; load the main evaluator
(load "main.lisp")

;; command-line argument parsing
(defun parse-command-line-args ()
  "parse command line arguments and return a list of options."
  (let ((args (cdr sb-ext:*posix-argv*))
        (options (make-hash-table :test 'equal)))
    
    ;; set default options
    (setf (gethash "mode" options) "interactive")
    (setf (gethash "expression" options) nil)
    (setf (gethash "file" options) nil)
    (setf (gethash "test" options) nil)
    (setf (gethash "help" options) nil)
    
    ;; parse arguments
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
           ;; assume it's an expression if no other mode is specified
           (setf (gethash "expression" options) arg)
           (setf (gethash "mode" options) "single")))))
    
    options))

;; help message
(defun show-help ()
  "display help information."
  (format t "~%expression evaluator - command line interface~%")
  (format t "==============================================~%~%")
  (format t "usage: sbcl --load cli.lisp [options] [expression]~%~%")
  (format t "options:~%")
  (format t "  --help         show this help message~%")
  (format t "  --interactive  start interactive mode (default)~%")
  (format t "  --expr expr    evaluate a single expression~%")
  (format t "  --file file    evaluate expressions from a file~%")
  (format t "  --test         run test suite~%~%")
  (format t "examples:~%")
  (format t "  sbcl --load cli.lisp --expr \"(+ 2 3)\"~%")
  (format t "  sbcl --load cli.lisp --file expressions.txt~%")
  (format t "  sbcl --load cli.lisp --interactive~%")
  (format t "  sbcl --load cli.lisp --test~%~%")
  (format t "expression format: lisp prefix notation~%")
  (format t "  (+ 2 3)       ; addition~%")
  (format t "  (* 4 (+ 2 3)) ; nested expressions~%")
  (format t "  (sin pi)       ; mathematical functions~%"))

;; file processing
(defun process-file (filename)
  "process expressions from a file."
  (format t "processing file: ~a~%~%" filename)
  
  (handler-case
    (with-open-file (stream filename :direction :input)
      (let ((line-number 0))
        (loop for line = (read-line stream nil :eof)
              until (eq line :eof)
              do (incf line-number)
              do (let ((trimmed-line (string-trim '(#\space #\tab #\newline) line)))
                   (unless (or (string= trimmed-line "")
                              (char= (char trimmed-line 0) #\;))
                     (format t "line ~a: ~a~%" line-number trimmed-line)
                     (handler-case
                       (let ((expr (read-from-string trimmed-line)))
                         (format t "result: ~a~%" (evaluate-expression expr)))
                       (error (e)
                         (format t "error: ~a~%" e)))
                     (format t "~%"))))))
    (error (e)
      (format t "error reading file: ~a~%" e))))

;; single expression evaluation
(defun evaluate-single-expression (expr-string)
  "evaluate a single expression from a string."
  (handler-case
    (let ((expr (read-from-string expr-string)))
      (format t "expression: ~a~%" expr-string)
      (format t "result: ~a~%" (evaluate-expression expr)))
    (error (e)
      (format t "error parsing expression: ~a~%" e))))

;; enhanced interactive mode
(defun enhanced-interactive-mode ()
  "start an enhanced interactive session."
  (format t "~%=== enhanced expression evaluator ===~%")
  (format t "type expressions in lisp format~%")
  (format t "special commands:~%")
  (format t "  :help      - show help~%")
  (format t "  :vars      - list variables~%")
  (format t "  :clear     - clear variables~%")
  (format t "  :examples  - run examples~%")
  (format t "  :test      - run test suite~%")
  (format t "  :benchmark - run performance benchmark~%")
  (format t "  :quit      - exit~%~%")
  
  (loop
    (format t "> ")
    (force-output)
    (let ((input (read)))
      (cond
        ((eq input 'quit) (return (format t "goodbye!~%")))
        ((eq input ':help)
         (show-help))
        ((eq input ':vars) 
         (format t "variables: ~a~%" (list-variables)))
        ((eq input ':clear)
         (clear-variables)
         (format t "all variables cleared.~%"))
        ((eq input ':examples)
         (run-examples))
        ((eq input ':test)
         (load "tests.lisp")
         (run-all-tests))
        ((eq input ':benchmark)
         (format t "enter expression to benchmark: ")
         (force-output)
         (let ((expr (read)))
           (format t "enter number of iterations: ")
           (force-output)
           (let ((iterations (read)))
             (benchmark-evaluation expr iterations))))
        (t (handler-case
             (format t "result: ~a~%" (evaluate-expression input))
             (error (e) (format t "error: ~a~%" e))))))))

;; main cli function
(defun main ()
  "main entry point for the command-line interface."
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

;; auto-run main if this file is loaded directly
#+sbcl
(when (member "--load" sb-ext:*posix-argv* :test 'string=)
  (main)) 