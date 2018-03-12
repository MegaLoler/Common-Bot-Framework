(defpackage :fmt
  (:documentation "Generalized formatted text representation.")
  (:use :cl :common-gateway)
  (:export concat
	   join
	   bold
	   italic
	   underline
	   strike
	   code
	   me
	   formatted))
(in-package :fmt)

(defstruct (concatenation (:print-object print-concatenated))
  "Represents a concatenation of values."
  (values nil :type list))

(defstruct (conjunction (:print-object print-joined))
  "Represents a conjuction of values."
  (values nil :type list)
  (delimiter nil :type t)
  (final-delimiter nil :type t))

(defstruct (bold (:print-object print-formatted))
  "Represents a bolded value."
  (value nil :type t))

(defstruct (italic (:print-object print-formatted))
  "Represents an italicized value."
  (value nil :type t))

(defstruct (underline (:print-object print-formatted))
  "Represents an underlined value."
  (value nil :type t))

(defstruct (strike (:print-object print-formatted))
  "Represents an struck out value."
  (value nil :type t))

(defstruct (code (:print-object print-formatted))
  "Represents an inline code value."
  (value nil :type t))

(defstruct (me (:print-object print-formatted))
  "Represents a `/me' expression."
  (value nil :type t))

(defmethod object-value ((object bold))
  "Get the value of a bolded object."
  (bold-value object))

(defmethod object-value ((object italic))
  "Get the value of a bolded object."
  (italic-value object))

(defmethod object-value ((object underline))
  "Get the value of a bolded object."
  (underline-value object))

(defmethod object-value ((object strike))
  "Get the value of a bolded object."
  (strike-value object))

(defmethod object-value ((object code))
  "Get the value of an inline code object."
  (code-value object))

(defmethod object-value ((object me))
  "Get the value of a `/me' expression."
  (me-value object))

(defun concat (&rest values)
  "Make a concatenation."
  (make-concatenation :values values))

(defun join (values &optional delimiter final-delimiter)
  "Make a conjunction."
  (make-conjunction :values values
		    :delimiter delimiter
		    :final-delimiter final-delimiter))

(defun bold (&rest values)
  "Make a bolded concatenation."
  (make-bold :value (make-concatenation :values values)))

(defun italic (&rest values)
  "Make an italicized concatenation."
  (make-italic :value (make-concatenation :values values)))

(defun underline (&rest values)
  "Make an underlined concatenation."
  (make-underline :value (make-concatenation :values values)))

(defun strike (&rest values)
  "Make a struck out concatenation."
  (make-strike :value (make-concatenation :values values)))

(defun code (&rest values)
  "Make a concatenation of inline code."
  (make-code :value (make-concatenation :values values)))

(defun me (&rest values)
  "Make a `/me' expression."
  (make-me :value (make-concatenation :values values)))

(defun print-concatenated (concatenation stream)
  "Generically print a concatenated object."
  (formatted concatenation nil stream))

(defun print-joined (conjunction stream)
  "Generically print a joined object."
  (formatted conjunction nil stream))

(defun print-formatted (object stream)
  "Generically print a formatted object."
  (formatted (object-value object) nil stream))

(defgeneric formatted (value gateway &optional stream)
  (:documentation "Format a value for some gateway."))

(defmethod formatted ((concatenation concatenation) gateway &optional stream)
  "Generically format a concatenation."
  (format stream "~{~A~}"
	  (mapcar (lambda (value)
		    (formatted value gateway))
		  (concatenation-values concatenation))))

(defmethod formatted ((conjunction conjunction) gateway &optional stream)
  "Generically format a conjunction."
  (let* ((values (conjunction-values conjunction))
	 (delimiter (or (conjunction-delimiter conjunction)
			" "))
	 (final-delimiter (or (conjunction-final-delimiter conjunction)
			      delimiter)))
    (format stream "~{~A~}"
	    (loop
	       :for value :in values
	       :for n :downfrom (length values)
	       :for penultimate-p = (= n 2)
	       :for ultimate-p = (= n 1)
	       :for d = (cond (penultimate-p final-delimiter)
			      (ultimate-p "")
			      (t delimiter))
	       :append (list (formatted value gateway)
			     (formatted d gateway))))))

(defmethod formatted ((me me) gateway &optional stream)
  "Generically format a `/me' expression."
  (formatted (italic (object-value me)) gateway stream))

(defmethod formatted (value gateway &optional stream)
  "Generically format a value."
  (format stream "~A" (stringify value)))
