(in-package :uncommon-bot.core)

(defun uncommon-bot-repl
    (&optional
       (environment (make-instance 'bot-environment))
       (in-stream *standard-input*)
       (out-stream *standard-output*)
       (prompt "> "))
  "A basic read-eval-print-loop for bot expressions."
  (uncommon-lisp-repl environment in-stream out-stream prompt))

(defun uncommon-bot-load
    (&optional
       (environment (make-instance 'bot-environment))
       (in-stream *standard-input*)
       (out-stream nil))
  "Load and evaluate bot expressions from a stream."
  (uncommon-lisp-load environment in-stream out-stream))

(defun uncommon-bot-load-string
    (string
     &optional
       (environment (make-instance 'bot-environment))
       (out-stream nil))
  "Load and evaluate bot expressions from a string."
  (uncommon-lisp-load-string string environment out-stream))

(defun uncommon-bot-load-file
    (filespec
     &optional
       (environment (make-instance 'bot-environment))
       (out-stream nil))
  "Load and evaluate bot expressions from a file."
  (uncommon-lisp-load-file filespec environment out-stream))
