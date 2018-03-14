(in-package :uncommon-lisp)

(defclass environment ()
  ((parent
    :initarg :parent
    :initform nil
    :type (or null environment)
    :accessor parent)
   (reader
    :initarg :reader
    :initform (make-instance 'syntax)
    :type (or null syntax)
    :accessor local-reader)
   (printer
    :initarg :printer
    :initform (make-instance 'syntax)
    :type (or null syntax)
    :accessor local-printer)
   (semantics
    :initarg :semantics
    :initform (make-instance 'semantics)
    :type (or null semantics)
    :accessor local-semantics))
  (:documentation "An environment for printing, reading, and evaluating Uncommon Lisp expressions."))

(defmethod reader ((environment environment))
  "Return the final reader syntax of an environment."
  (or (local-reader environment)
      (and (parent environment)
	   (reader (parent environment)))))

(defmethod printer ((environment environment))
  "Return the final printer syntax of an environment."
  (or (local-printer environment)
      (and (parent environment)
	   (printer (parent environment)))))

(defmethod semantics ((environment environment))
  "Return the final semantics of an environment."
  (or (local-semantics environment)
      (and (parent environment)
	   (semantics (parent environment)))))
