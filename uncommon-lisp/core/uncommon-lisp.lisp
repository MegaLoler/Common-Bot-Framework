(in-package :uncommon-lisp)

(defgeneric uncommon-lisp-read (reader stream environment)
  (:documentation "The reader of Uncommon Lisp."))

(defgeneric uncommon-lisp-print (printer object stream environment)
  (:documentation "The printer of Uncommon Lisp."))

(defgeneric uncommon-lisp-eval (evaluator object environment)
  (:documentation "The evaluator of Uncommon Lisp."))

(defmethod uncommon-lisp-read (reader (stream stream) (environment environment))
  "Perform a top-level read from `stream' in `environment'."
  (uncommon-lisp-read (syntax environment) stream environment))

(defmethod uncommon-lisp-print (printer object (stream stream) (environment environment))
  "Perform a top-level print of `object' to `stream' in `environment'."
  (uncommon-lisp-print (syntax environment) object stream environment))

(defmethod uncommon-lisp-eval (evaluator object (environment environment))
  "Perform a generic evaluation of `object' in `environment'."
  (uncommon-lisp-eval (semantics environment) object environment))
