(in-package :uncommon-bot.core)

(defclass command ()
  ((aliases
    :initarg :aliases
    :type nil
    :accessor aliases)
   (description
    :initarg :description
    :type verbal-expression
    :accessor description)
   (eval-args
    :initform t
    :initarg :eval-args
    :type boolean
    :accessor eval-args))
  (:documentation "An invokable command."))

(defgeneric permitted-p (command environment)
  (:documentation "Whether a command is permitted in a evaluation environment."))

(defgeneric evaluate (command arguments environment)
  (:documentation "Evaluate a command with arguments in an environment."))

(defmethod permitted-p ((command command) (environment bot-environment))
  "Commands permitted by default."
  t)

(defmethod alias ((command command))
  "Get a suitable alias for a command."
  (car (aliases command)))

(defmethod invoke
    ((command command)
     (arguments list)
     (environment bot-environment))
  "Invoke a command with arguments in an environment if permitted."
  (if (permitted-p command environment)
      (evaluate command arguments environment)
      (error "Permission denied!")))

(defmethod command ((command command) (environment bot-environment))
  "Get a command designated by a command in an environment."
  (find command (commands environment)))

(defmethod command ((alias string) (environment bot-environment))
  "Get a command designated by a string in an environment."
  (find-if (lambda (command)
	     (find-if (lambda (command-alias)
			(string-equal alias command-alias))
		      (aliases command)))
	   (commands environment)))

(defmethod command ((alias symbol) (environment bot-environment))
  "Get a command designated by a symbol in an environment."
  (command (string alias) environment))

(defmethod command (object (environment bot-environment))
  "Anything else does not designate a command."
  nil)
