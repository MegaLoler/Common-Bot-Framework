(in-package :uncommon-lisp)

(defclass environment ()
  ((parent
    :initarg :parent
    :initform nil
    :type (or null environment)
    :accessor parent)
   (syntax
    :initarg :syntax
    :initform (make-instance 'syntax)
    :type (or null syntax)
    :accessor local-syntax)
   (semantics
    :initarg :semantics
    :initform (make-instance 'semantics)
    :type (or null semantics)
    :accessor local-semantics))
  (:documentation "An environment for printing, reading, and evaluating Uncommon Lisp expressions."))

(defmethod syntax ((environment environment))
  "Return the final syntax of an environment."
  (or (local-syntax environment)
      (and (parent environment)
	   (syntax (parent environment)))))

(defmethod semantics ((environment environment))
  "Return the final semantics of an environment."
  (or (local-semantics environment)
      (and (parent environment)
	   (semantics (parent environment)))))
