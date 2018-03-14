(in-package :uncommon-bot.core)

(defclass bot-syntax (syntax)
  ((prefixes
    :initarg prefixes
    :initform '("!")
    :type cons
    :accessor prefixes)
   (variable-prefix
    :initarg variable-prefix
    :initform "$"
    :type string
    :accessor variable-prefix)
   (delimiter
    :initarg delimiter
    :initform ";"
    :type string
    :accessor delimiter))
  (:documentation "A printer/reader syntax for bot expressions."))

(defmethod prefix ((syntax bot-syntax))
  "Get a suitable command prefix."
  (car (prefixes syntax)))

(defmethod uncommon-lisp-read
    ((syntax bot-syntax)
     (stream stream)
     (environment bot-environment))
  "Perform a generic read from `stream' in `environment' with `syntax'."
  (read stream))

(defun print-list (syntax object stream environment)
  "Print a list."
  (format stream "(")
  (loop
     :for item :in object
     :for x :from 1
     :for commap = (< x (length object))
     :do (progn
	   (uncommon-lisp-print syntax
				item
				stream
				environment)
	   (when commap 
	     (format stream ", "))))
  (format stream ")"))

;; this is a huge mess
(defun print-command-expression (syntax cmd args stream environment)
  "Print a command expression."
  (cond ((and (eq cmd (command 'get environment))
	      args)
	 (progn
	   (format stream "~A" (variable-prefix syntax))
	   (when (listp (car args))
	     (format stream "("))
	   (uncommon-lisp-print syntax
				(car args)
				stream
				environment)
	   (when (listp (car args))
	     (format stream ")"))))
	((and (eq cmd (command 'gets environment))
	      args)
	 (progn
	   (format stream "~A(" (variable-prefix syntax))
	   (uncommon-lisp-print syntax
				args
				stream
				environment)
	   (format stream ")")))
	((and (eq cmd (command 'quote environment))
	      args)
	 (progn
	   (format stream "'")
	   (uncommon-lisp-print syntax
				(car args)
				stream
				environment)
	   (format stream "'")))
	(t (progn
	     (format stream "~A~A" (prefix syntax) (alias cmd))
	     (loop
		:for arg :in args
		:do (progn
		      (format stream " ")
		      (uncommon-lisp-print syntax
					   arg
					   stream
					   environment)))
	     (format stream (delimiter syntax))))))

(defmethod uncommon-lisp-print
    ((syntax bot-syntax)
     (object list)
     (stream stream)
     (environment bot-environment))
  "Print a list or a command expression."
  (let ((cmd (command (car object) environment)))
    (if cmd
	(print-command-expression syntax cmd (cdr object) stream environment)
	(print-list syntax object stream environment))))

(defmethod uncommon-lisp-print
    ((syntax bot-syntax)
     (object command)
     (stream stream)
     (environment bot-environment))
  "Print a command"
  (uncommon-lisp-print syntax `(command ,(alias object)) stream environment))

(defmethod uncommon-lisp-print
    ((syntax bot-syntax)
     (object symbol)
     (stream stream)
     (environment bot-environment))
  "Print a symbol."
  (let* ((spacep (find #\Space (string object)))
	 (quote (if spacep #\" "")))
    (format stream "~A~A~A" quote object quote)))

(defmethod uncommon-lisp-print
    ((syntax bot-syntax)
     (object null)
     (stream stream)
     (environment bot-environment))
  "Print nil."
  (format stream "Nothing")) ;; i should localize this and the "false" strings

(defmethod uncommon-lisp-print
    ((syntax bot-syntax)
     (object formatted-object)
     (stream stream)
     (environment bot-environment))
  "Print a formatted object."
  (uncommon-lisp-print (text-protocol environment) object stream environment))

(defmethod uncommon-lisp-print
    ((syntax bot-syntax)
     (object localized-object)
     (stream stream)
     (environment bot-environment))
  "Print a localized object."
  (uncommon-lisp-print (language environment) object stream environment))

(defmethod uncommon-lisp-print
    ((syntax bot-syntax)
     (object verbal-expression)
     (stream stream)
     (environment bot-environment))
  "Print a verbal expression."
  (uncommon-lisp-print (personality environment) object stream environment))
