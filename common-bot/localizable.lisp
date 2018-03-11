(in-package :common-bot)

;; should i put some more gensyms in these macros?

(defstruct localizable
  "An object that can be localized.")

(defgeneric localize (object language personality)
  (:documentation "Localize a localizable object given some localization context."))

(defmethod localize (object language personality)
  "For non-localizable objects."
  object)

(defun localize-eval (object language personality)
  "Recursively localize an object or expression."
  (localize object language personality)) ;;todo 4 real

(defun make-localization-body (name parameters object body)
  "Make the body for a localization method."
  `((let ,(loop
	     :for parameter :in parameters
	     :collect `(,parameter
			(,(cat name parameter)
			  ,object)))
      (declare (ignorable ,@parameters))
      ,@body)))

(defmacro deflocalizable (name parameters &body doc-string-and-body)
  "Define a localizable object."
  (let ((car-string-p (stringp (car doc-string-and-body))))
    (let ((doc-string (when car-string-p
			(car doc-string-and-body)))
	  (body (if car-string-p
		    (cdr doc-string-and-body)
		    doc-string-and-body))
	  (obj-sym (gensym)))
      (let ((method-body (make-localization-body
			  name
			  parameters
			  obj-sym
			  body)))
	(let ((struct-doc-string-and-body
	       (if doc-string
		   (cons doc-string parameters)
		   parameters))
	      (method-doc-string-and-body
	       (if doc-string
		   (cons doc-string method-body)
		   method-body)))
	  `(progn
	     (defstruct (,name (:include localizable)
			       (:constructor ,name ,parameters))
	       ,@struct-doc-string-and-body)
	     (defmethod localize ((,obj-sym ,name) language personality)
	       ,@method-doc-string-and-body)))))))

(defmacro deflocalization (name-and-context parameters &body doc-string-and-body)
  "Define a localization of a localizable given a language and a bot personality."
  (let ((car-string-p (stringp (car doc-string-and-body))))
    (let ((doc-string (when car-string-p
			(car doc-string-and-body)))
	  (body (if car-string-p
		    (cdr doc-string-and-body)
		    doc-string-and-body))
	  (name (first name-and-context))
	  (language (second name-and-context))
	  (personality (third name-and-context))
	  (obj-sym (gensym)))
      (let ((method-body (make-localization-body
			  name
			  parameters
			  obj-sym
			  body)))
	(let ((method-doc-string-and-body
	       (if doc-string
		   (cons doc-string method-body)
		   method-body)))
	  `(defmethod localize
	       ((,obj-sym ,name)
		(language ,(if language
			       (cat language
				    'language)
			       'language))
		(personality ,(if personality
				  (cat personality
				       'personality)
				  'personality)))
	     ,@method-doc-string-and-body))))))

(defstruct language
  "Represents a natural language."
  (code nil :type symbol :read-only t)
  (name nil :type localizable :read-only t))

(defstruct personality
  "Represents a bot personality."
  (name nil :type localizable :read-only t))
