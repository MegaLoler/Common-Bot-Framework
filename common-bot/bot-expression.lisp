(in-package :bot)

(deftype bot-expression ()
  "An expression to be evaluated by a bot."
  `(and list (not null)))

(defun bot-expression (bot expression)
  "Return the bot expression designated by `expression'."
  (cond ((and (listp expression)
	      (not (null expression)))
	 expression)
	((stringp expression)
	 (parse-bot-expression bot expression))
	(t (error "Invalid bot expression!"))))

(defun bot-eval (bot message expression)
  "Evaluate a bot expression."
  (let ((expression (bot-expression bot expression)))
    (apply-command (car expression) bot message (cdr expression))))

(defun parse-bot-expression (bot string)
  "Return a parsed bot expression from a string."
  (read-bot-expression bot (make-string-input-stream string)))

(defun read-bot-expression (bot stream)
  "Parse a bot expression from a stream with the reader function of a bot."
  (funcall (bot-expression-reader bot) bot stream))

(defun print-bot-expression (bot expression &optional stream)
  "Format and print a bot expression to a stream with the printer function of a bot."
  (funcall (bot-expression-printer bot) expression stream))

(defun read-bot-command (bot stream)
  "Read the command name from a bot expression from a stream."
  (let ((symbol (read)))
    (assert (starts-with-any-p
	     (string symbol)
	     (bot-aliases bot)))))

(defun default-bot-expression-reader (bot stream)
  "A default bot expression reader."
  (car (read-bot-command bot stream)
       (loop
	  :for form := (read stream nil nil)
	  :while form
	  :collect form)))

(defun default-bot-expression-printer
    (bot expression &optional stream)
  "A default bot expression printer."
  (let* ((expression (bot-expression expression))
	 (command (car expression))
	 (arguments (cdr expression)))
  (format stream "~A ~{~A~^ ~}"
	  (command-print-reasonable-alias command bot)
	  arguments)))
