(in-package :common-bot)

(deftype bot-expression ()
  "An expression to be evaluated by a bot."
  `cons)

;; this could totally be expanded on to support variables and all kinds of things!!
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

(defun bot-expression-invoke (bot message expression stream)
  "Evaluate a bot expression in a context and present the result."
  (command-invoke (car expression)
		  bot
		  message
		  (cdr expression)
		  stream))
