(in-package :uncommon-lisp)

;; i could make the prompt here display information about the environment
;; can i rewrite this with the loop macro?
(defun uncommon-lisp-repl
    (&optional
       (environment (make-instance 'environment))
       (in-stream *standard-input*)
       (out-stream *standard-output*)
       (prompt "> "))
  "A basic read-eval-print-loop in Uncommon Lisp."
  (let ((value))
    (handler-case
	(loop
	   (progn
	     (when (and out-stream prompt)
	       (format out-stream "~%~A" prompt))
	     (setf value
		   (uncommon-lisp-eval nil
		    (uncommon-lisp-read nil in-stream environment)
		    environment))
	     (when out-stream
	       (uncommon-lisp-print nil value out-stream environment))))
      (end-of-file () value))))

(defun uncommon-lisp-load
    (&optional
       (environment (make-instance 'environment))
       (in-stream *standard-input*)
       (out-stream nil))
  "Load and evaluate Uncommon Lisp code from a stream."
  (uncommon-lisp-repl environment in-stream out-stream nil))

(defun uncommon-lisp-load-string
    (string
     &optional
       (environment (make-instance 'environment))
       (out-stream nil))
  "Load and evaluate Uncommon Lisp code from a string."
  (uncommon-lisp-load environment
		      (make-string-input-stream string)
		      out-stream))

(defun uncommon-lisp-load-file
    (filespec
     &optional
       (environment (make-instance 'environment))
       (out-stream nil))
  "Load and evaluate Uncommon Lisp code from a file."
  (uncommon-lisp-load environment
		      (open filespec)
		      out-stream))
