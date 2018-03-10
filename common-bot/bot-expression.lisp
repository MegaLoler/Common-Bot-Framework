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
				 (bot-eval bot message argument))
			       (cdr expression))))
	 (t expression)))
