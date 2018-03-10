(in-package :common-bot)

(defstruct (command (:print-object command-print))
  "A bot command."
  ;; the recognized names for this command
  (aliases nil :type cons :read-only t)
  ;; a description of this command
  (documentation nil :type string :read-only t)
  ;; example bot expressions using this command
  (examples nil :type list :read-only t)
  ;; predicate indicating whether this command can be invoked in a context
  (permitted #'default-command-permitted :type function :read-only t)
  ;; function used to evaluate this command
  (evaluator #'default-command-evaluator :type function :read-only t)
  ;; function used to present the result of evaluating this command to users
  (presenter #'default-command-presenter :type function :read-only t))

(defun default-command-permitted (bot message)
  "Commands are permitted to be invoked no matter what unless specified otherwise."
  (declare (ignorable bot message))
  t)

(defun default-command-evaluator (bot message)
  "Commands evaluate as nil unless specified otherwise."
  (declare (ignorable bot message))
  nil)

(defun default-command-presenter (value stream)
  "Generic function to display the evaluation of a command to users."
  (format stream "Result: ~A~%" value))

(defmacro defcommand (symbol aliases &rest options)
  "Define a command known by some aliases and assign it as a global variable."
  `(defparameter ,symbol
     (make-command :aliases ',aliases
		   ,@options)))

(defun command (command &optional bot)
  "Return a command designated by `command'."
  (cond ((command-p command) command)
	((stringp command)
	 (command (read-from-string command)
		  bot))
	((and bot (symbolp command))
	 (bot-command-by-alias bot command))
	(t (error "Invalid command designator!"))))

(defun command-print (command stream)
  "Print a readable form of a command to a stream."
  (format stream "~S" (command-suitable-alias command)))

(defun command-suitable-alias (command &optional bot)
  "Return an alias suitable for invoking a command."
  (car (command-aliases (command command bot))))

(defun command-permitted-p (command bot message)
  "Whether a command is invokable in a context."
  (funcall (command-permitted (command command bot))
	   bot message))

(defun command-eval (command bot message args)
  "Evaluate a command in a context."
  (apply (command-evaluator (command command bot))
	 `(bot ,message ,@args)))

(defun command-present (command value stream &optional bot)
  "Present a value via a stream."
  (funcall (command-presenter (command command bot))
	   value stream))

(defun command-invoke (command bot message args stream)
  "Invoke a top-level command, presenting the result to the user."
  (command-present command
		   (command-eval command bot message args)
		   stream
		   bot))
