(in-package :uncommon-lisp)

(defclass semantics ()
  ()
  (:documentation "Semantics for evaluation."))

(defmethod uncommon-lisp-eval
    ((semantics semantics)
     object
     (environment environment))
  "Perform a generic evaluation of `object' in `environment'."
  (eval object))
