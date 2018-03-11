(defpackage :fmt
  (:documentation "Generalized formatted text representation.")
  (:use :cl :common-gateway)
  (:export concat
	   bold
	   italic
	   underline
	   strike
	   formatted))
(in-package :fmt)

(defstruct (concatenation (:print-object print-concatenated))
  "Represents a concatenation of values."
  (values nil :type list))

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

(defun concat (&rest values)
  "Make a concatenation."
  (make-concatenation :values values))

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

(defun print-concatenated (concatenation stream)
  "Generically print a concatenated object."
  (formatted concatenation nil stream))

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

(defmethod formatted (value gateway &optional stream)
  "Generically format a value."
  (format stream "~A" (stringify value)))
