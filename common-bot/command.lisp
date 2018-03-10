(in-package :common-bot)

(defstruct (command (:print-object command-print))
  "A bot command."
  ;; the recognized names for this command
  (aliases nil :type cons :read-only t)
  ;; a description of this command
  (documentation nil :type (or null string) :read-only t)
  ;; example bot expressions using this command
  (examples nil :type list :read-only t)
  ;; predicate indicating whether this command can be invoked in a context
  (permitted #'default-command-permitted :type function :read-only t)
  ;; function used to evaluate this command
  (evaluator #'default-command-evaluator :type function :read-only t))

(defun default-command-permitted (bot message)
  "Commands are permitted to be invoked no matter what unless specified otherwise."
  (declare (ignorable bot message))
  t)

(defun default-command-evaluator (bot message)
  "Commands evaluate as nil unless specified otherwise."
  (declare (ignorable bot message))
  nil)

(defmacro defcommand (name aliases-and-options lambda-list &body body)
  "Define a command known by some aliases and assign it as a global variable."
  (let ((car-list-p (listp (car aliases-and-options))))
    (let ((env-pars (subseq lambda-list 0 2))
	  (options (if car-list-p
		       (cdr aliases-and-options)))
	  (aliases (if car-list-p
		       (car aliases-and-options)
		       aliases-and-options)))
      `(defparameter ,name
	 (make-command :aliases ',aliases
		       :evaluator (lambda ,lambda-list
				    (declare (ignorable ,@env-pars))
				    ,@body)
		       ,@options)))))

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
