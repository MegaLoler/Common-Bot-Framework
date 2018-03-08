(in-package :bot)

(deftype bot-expression ()
  "An expression to be evaluated by a bot."
  `(and list (not null)))

(defun bot-expression (bot expression)
  "Return the bot expression designated by `expression'."
  (cond ((typep expression 'bot-expression)
	 expression)
	((stringp expression)
	 (parse-bot-expression bot expression))
	(t (error "Invalid bot expression!"))))

(defun bot-eval (bot message expression)
  "Evaluate a bot expression."
  (let ((expression (bot-expression bot expression)))
    (apply-command (car expression) bot message (cdr expression))))
