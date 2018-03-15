(in-package :uncommon-bot.core)

(defclass bot-syntax (syntax)
  ((prefixes
    :initarg prefixes
    :initform '("!")
    :type cons
    :accessor prefixes)
   (variable-prefix
    :initarg variable-prefix
    :initform #\$
    :type character
    :accessor variable-prefix))
  (:documentation "A printer/reader syntax for bot expressions."))

(defmethod prefix ((syntax bot-syntax))
  "Get a suitable command prefix."
  (car (prefixes syntax)))



;; reader

;; rewrite all of this more cleanly

(defun read-symbol (syntax stream environment)
  "Read a symbol from a stream."
  (declare (ignorable syntax stream environment))
  (if (position (peek-char nil stream)
		(concatenate 'string (format nil " ,)';.0123456789\"~%")
			     (list (variable-prefix syntax))))
      (error (format nil "Unexpected character ~S."
		     (peek-char nil stream)))
      (intern (read-until (format nil " ,)';\"~%") stream nil nil))))

(defun read-string (syntax stream environment)
  "Read a string from a stream."
  (declare (ignorable syntax stream environment))
  (if (char= #\" (read-char stream))
      (read-until #\" stream #\\)
      (error "Expected '\"'.")))

(defun read-number (syntax stream environment)
  "Read a number from a stream."
  (declare (ignorable syntax stream environment))
  (parse-integer (read-until (format nil " ,)';\"~%") stream nil nil)))

(defun read-variable (syntax stream environment)
  "Read a variable from a stream."
  (declare (ignorable syntax stream environment))
  (if (char= (variable-prefix syntax)
	     (read-char stream))
      `(get ,(if (char= #\( (peek-char nil stream))
		 (read-grouping syntax stream environment t)
		 (read-symbol syntax stream environment)))
      (error (format nil "Expected \"~A\"."
		     (variable-prefix syntax)))))

(defun read-command-expression (syntax stream environment)
  "Read a command expression from a stream."
  (read-member (prefixes syntax) stream)
  (loop
     :for object = (read-object syntax stream environment)
     :until (or (find (peek-char nil stream) '(#\; #\Newline))
		(progn
		  (eat-char #\Space stream)
		  (find (peek-char nil stream) '(#\; #\Newline))))
     :collect object :into result
     :finally (progn
		(when (char= (peek-char nil stream) #\;)
		  (read-char stream))
		(return (append result (list object))))))

(defun read-grouping (syntax stream environment &optional rec)
  "Read a grouping from a stream."
  (eat-char #\Space stream)
  (if (char= #\( (read-char stream))
      (if (char= #\) (peek-char nil stream))
	  (progn
	    (read-char stream)
	    nil)
	  (let ((object (read-object syntax stream environment rec)))
	    (if (char= #\) (read-char stream))
		object
		(error "Expected \")\"."))))
      (error "Expected \"(\".")))

(defun read-quote (syntax stream environment)
  "Read a string from a stream."
  (eat-char #\Space stream)
  (if (char= #\' (read-char stream))
      (let ((object (read-object syntax stream environment)))
	;; (if (char= #\' (read-char stream))
	    `(quote ,object)
	    ;; (error "Expected \"'\"."))
      )
      (error "Expected \"'\".")))

;; (defun read-until (until stream &optional escape)
;;   "Read from stream until a character."
;;   (coerce
;;    (loop
;;       :for c = (read-char stream)
;;       :until (char= c until)
;;       :if (and escape (char= c escape)) :collect (read-char stream)
;;       :else :collect c)
;;    'string))

(defun eat-char (char stream)
  "Consume a series of a character in a stream."
  (when (char= char (peek-char nil stream))
    (read-char stream)
    (eat-char char stream)))

(defun eat-chars (chars stream)
  "Consume a series of any of some characters in a stream."
  (when (find (peek-char nil stream) chars)
    (read-char stream)
    (eat-chars chars stream)))

(defun read-member (members stream)
  "Read from stream until matching a member of `members'."
  (loop
     :with buffer = (make-array 0
				:element-type 'character
				:fill-pointer 0
				:adjustable t)
     :for c = (peek-char nil stream)
     :until (find-if (lambda (member)
		       (string-equal member buffer))
		     members)
     :do (progn
	   (read-char stream)
	   (vector-push-extend c buffer))
     :finally (return buffer)))

(defun read-until (until stream &optional escape (consume-final-char t))
  "Read from stream until a character.
`until' can be either a single character or a string of possible characters."
  (loop
     :with result = (make-array 0
				:element-type 'character
				:fill-pointer 0
				:adjustable t)
     :for c = (peek-char nil stream)
     :until (if (stringp until)
		(position c until)
		(char= c until))
     :if (and escape (char= c escape))
     :do (progn
	   (read-char stream)
	   (vector-push-extend (read-char stream) result))
     :else :do (vector-push-extend (read-char stream) result)
     :finally (progn
		(when consume-final-char
		  (read-char stream))
		(return result))))

(defun read-object (syntax stream environment &optional ls)
  "Read an object from a stream."
  (eat-chars (list #\Space #\Newline) stream)
  (let* ((c (peek-char nil stream))
	 (object
	  (cond
	    ((position c (mapcar (lambda (prefix)
				   (char prefix 0))
				 (prefixes syntax)))
	     (read-command-expression syntax stream environment))
	    ((char= c (variable-prefix syntax))
	     (read-variable syntax stream environment))
	    (t (case c
		 (#\; (error "Unexpected \";\"."))
		 (#\, (error "Unexpected \",\"."))
		 (#\) (error "Unexpected \")\"."))
		 ((#\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		  (read-number syntax stream environment))
		 (#\( (read-grouping syntax stream environment))
		 (#\" (read-string syntax stream environment))
		 (#\' (read-quote syntax stream environment))
		 (otherwise (read-symbol syntax stream environment)))))))
    (handler-case
	(progn
	  (eat-char #\Space stream)
	  (if (char= #\, (peek-char nil stream))
	      (progn
		(read-char stream)
		(let ((result (cons object (read-object syntax stream environment t))))
		  (if ls
		      result
		      (cons 'list result))))
	      (if ls
		  (list object)
		  object)))
      (end-of-file ()
	(if ls
	    (list object)
	    object)))))

(defmethod uncommon-lisp-read
    ((syntax bot-syntax)
     (stream stream)
     (environment bot-environment))
  "Perform a generic read from `stream' in `environment' with `syntax'."
  (read-object syntax stream environment))



;; printer

(defun print-list (syntax object stream environment)
  "Print a list."
  (loop
     :for item :in object
     :for x :from 1
     :for commap = (< x (length object))
     :for enclosep = (listp item)
     :do (progn
	   (when enclosep
	     (format stream "("))
	   (uncommon-lisp-print syntax
				item
				stream
				environment)
	   (when enclosep
	     (format stream ")"))
	   (when commap 
	     (format stream ", ")))))

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
	     (format stream ";")))))

(defmethod uncommon-lisp-print
    ((syntax bot-syntax)
     (object list)
     (stream stream)
     (environment bot-environment))
  "Print a list or a command expression."
  (if (and (symbolp (car object))
	   (string-equal 'quote (car object))
	   (cdr object))
      (progn
	(format stream "'")
	(uncommon-lisp-print syntax
			     (cadr object)
			     stream
			     environment)
	;; (format stream "'")
	)
      (let ((cmd (and (symbolp (car object))
		      (command (car object) environment))))
	(if cmd
	    (print-command-expression syntax cmd (cdr object) stream environment)
	    (print-list syntax object stream environment)))))

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
