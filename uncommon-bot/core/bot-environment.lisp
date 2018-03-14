(in-package :uncommon-bot.core)

(defclass bot-environment (environment)
  ((parent
    :initarg :parent
    :initform nil
    ;; :type (or null bot-environment) ; this line seems to cause all kinds of weird issues??
    :accessor parent)
   (syntax
    :initarg :syntax
    :initform nil
    :type (or null bot-syntax)
    :accessor local-syntax)
   (semantics
    :initarg :semantics
    :initform nil
    :type (or null bot-semantics)
    :accessor local-semantics)
   (text-protocol
    :initarg :text-protocol
    :initform nil
    :type (or null text-protocol)
    :accessor local-text-protocol)
   (language
    :initarg :language
    :initform nil
    :type (or null language)
    :accessor local-language)
   (personality
    :initarg :personality
    :initform nil
    :type (or null personality)
    :accessor local-personality)
   (commands
    :initarg :commands
    :initform (vector)
    :type (vector command)
    :accessor local-commands)
   (bindings
    :initarg :bindings
    :initform (make-hash-table)
    :type hash-table
    :accessor bindings))
  (:documentation "An environment for printing, reading, and evaluating bot expressions."))

(defmethod text-protocol ((environment bot-environment))
  "Return the final text protocol of a bot environment."
  (or (local-text-protocol environment)
      (and (parent environment)
	   (syntax (parent environment)))))

(defmethod language ((environment bot-environment))
  "Return the final language of a bot environment."
  (or (local-language environment)
      (and (parent environment)
	   (language (parent environment)))))

(defmethod personality ((environment bot-environment))
  "Return the final personality of a bot environment."
  (or (local-personality environment)
      (and (parent environment)
	   (personality (parent environment)))))

(defmethod commands ((environment bot-environment))
  "Return the final commands available in a bot environment."
  (concatenate 'vector
	       (local-commands environment)
	       (and (parent environment)
		    (commands (parent environment)))))

(defun binding (name environment)
  "Return an effective binding in an environment by a name."
  (or (gethash name (bindings environment))
      (and (parent environment)
	   (binding name (parent environment)))))

(defun bind (name value environment &optional local)
  "Make a local binding in an environment of a name to a value."
  (if (or local
	  (gethash name (bindings environment)))
      (setf (gethash name (bindings environment))
	    value)
      (if (and (parent environment)
	       (binding name (parent environment)))
	  (bind name value (parent environment))
	  (setf (gethash name (bindings environment))
		value))))

(defun push-command (command environment)
  "Add a new command to the known commands in an environment."
  (setf (local-commands environment)
	(concatenate 'vector
		     (commands environment)
		     (list command))))

(defun remove-command (command environment)
  "Delete a visible command."
  (let ((cmd (find command (local-commands environment))))
    (if cmd
	(setf (local-commands environment)
	      (remove-if (lambda (command)
			   (eq command cmd))
			 (local-commands environment)))
	(when (parent environment)
	  (remove-command command (parent environment))))))
