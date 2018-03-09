(in-package :common-bot.command)

(defmacro defcommand (aliases-and-options lambda-list &body body)
  "Define a command. `aliases-and-options' may be either a single symbol representing a single alias and no options, or it may be a list, the cdr of which are the options and the car of which may be either a single symbol representing a single alias or a list representing multiple aliases. All alias symbols have their `command' slot bound to the defined command.
`lambda-list' is the lambda list of the command function. The first three arguments passed to a command function when called are always a reference to the bot answering the message, the originating message, and a stream for communicating with the author of the message. As such, the lambda list must contain at least 3 required parameters. However, they are declared ignorable and aren't expected to be used in the body of the command function."
  (let ((aliases (if (listp aliases-and-options)
		     (let ((aliases (car aliases-and-options)))
		       (if (listp aliases)
			   aliases
			   (list aliases)))
		     (list aliases-and-options)))
	(options (if (listp aliases-and-options)
		     (cdr aliases-and-options)))
	(context-parameters (subseq lambda-list 0 3))
	(lambda-list (subseq lambda-list 3)))
    `(let* ((func (lambda ,lambda-list
		    (declare (ignorable ,@context-parameters))
		    ,@body))
	    (command (make-command
		      :function func
		      :lambda-list ,lambda-list
		      ,@options)))
       (loop
	  :for alias :in ,aliases
	  :do (setf (command alias) command)))))

(defstruct command
  "A bot command."
  ;; list of recognized names for this command
  (aliases nil :type list)
  ;; a string describing what the command is for
  (documentation nil :type string)
  ;; a function taking a bot and a message to determine whether the command is permitted to be invoked
  (permitted-p nil)
  ;; a list of example arglists for using this command
  (examples nil :type list)
  ;; the command lambda list
  (lambda-list nil :type list)
  ;; the function that is called when the command is invoked
  (function nil))

(defun command (command)
  "Get a command designated by `command'."
  (cond ((command-p command) command)
	((symbolp command)
	 (get command 'command))
	(t (error "Invalid command designator!"))))

(defun apply-command (command bot message arguments)
  "Apply a command function to arguments."
  (let ((buffer (make-string 0)))
    (with-output-to-string (stream buffer)
      (apply (command-function (command command))
	     (append (list bot message stream)
		     arguments)))
    (send message buffer)))

(defun command-call (command bot message &rest arguments)
  "Call a command function."
  (apply-command command bot message arguments))

(defun command-allowed (command bot message)
  "Whether a command is permitted to be invoked with a bot in the context of some message."
  (let ((permitted-p (command-permitted-p (command command))))
    (if permitted-p
	(funcall permitted-p bot message)
	t)))

(defun print-alias (alias bot &optional stream)
  "Format and print a command alias to a stream."
  (format stream "~A~A"
	  (bot-reasonable-prefix-string bot)
	  alias))

(defun command-reasonable-alias (command)
  "Return a suitable alias for designating a command."
  (car (command-aliases (command command))))

(defun command-print-reasonable-alias (command bot &optional stream)
  "Format and print a suitable alias for designating a command."
  (print-alias (command-reasonable-alias (command command)) bot stream))

(defun command-aliases-formatted (command bot)
  "Return a list of formatted alias strings for a command with a bot."
  (mapcar (lambda (alias)
	    (print-alias alias bot))
	  (command-aliases (command command))))

(defun format-parameter (parameter &optional type stream)
  "Format and print a parameter of a lambda list."
  (case type
    (&rest (format stream "~A*" parameter))
    (&optional (format stream "[~A]" parameter))
    (otherwise (format stream "~A" parameter))))

(defun lambda-list-template-expression (lambda-list)
  "Format the parameters of a lambda list."
  (loop
     :with type
     :for parameter :in lambda-list
     :if (typep parameter 'lambda-list-keyword)
     :do (setf type parameter)
     :else :collect (format-parameter parameter type)))

(defun command-template-expression (command)
  "Return a psuedo bot expression that describes the syntax of a command."
  (car (command-reasonable-alias command)
       (lambda-list-template-expression
	(command-lambda-list (command command)))))

(defun command-print-syntax (command bot &optional stream)
  "Format and print a suitable syntax string for a command."
  (print-bot-expression
   bot
   (command-template-expression command)
   stream))

(defun command-description (command bot &optional stream)
  "Format and print a description of a command."
  (format stream "`~A`~%  *~A*~%"
	  (command-print-syntax command bot)
	  (command-documentation command))
  
