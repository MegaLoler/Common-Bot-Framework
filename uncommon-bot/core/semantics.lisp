(in-package :uncommon-bot.core)

(defclass bot-semantics (semantics)
  ()
  (:documentation "Semantics for evaluating bot expressions."))

(defun falsep (value)
  "Whether a value is falsey or not."
  (and (not (numberp value))
       (reduce (lambda (a b)
	    (or a b))
	  (mapcar (lambda (s)
		    (string-equal s value))
		  '(false nil null no not nah naw nee nay never no-way wrong incorrect untrue nope nothing void))
	  :initial-value nil)))

(defun truep (value)
  "Whether a value is truthy or not."
  (not (falsep value)))

(defun truth-wrap (value)
  "Return a truth value in the bot language."
  (if (truep value)
      'true
      'false))

(defmethod uncommon-lisp-eval
    ((semantics bot-semantics)
     object
     (environment bot-environment))
  "Generic evaluate."
  object)

(defmethod uncommon-lisp-eval
    ((semantics bot-semantics)
     (expression cons)
     (environment bot-environment))
  "Evaluate a cons."
  (let ((command (and (symbolp (car expression))
		      (command (car expression) environment)))
	(argument-expressions (cdr expression)))
    (if command
	(let ((arguments
	       (if (eval-args command)
		   (mapcar
		    (lambda (expression)
		      (uncommon-lisp-eval
		       semantics
		       expression
		       environment))
		    argument-expressions)
		   argument-expressions)))
	  (invoke command arguments environment))
	(error "Invalid command designator!"))))
