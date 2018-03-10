(in-package :common-bot)

(deftype bot-expression ()
  "An expression to be evaluated by a bot."
  `cons)

(defun bot-eval (bot message expression)
  "Evaluate a bot expression in a context."
  (cond ((typep expression 'bot-expression)
	 (command-eval (car expression)
		       bot
		       message
		       (mapcar (lambda (argument)
				 (localize (bot-eval bot message argument)
					   nil nil))
			       (cdr expression))))
	(t expression)))
