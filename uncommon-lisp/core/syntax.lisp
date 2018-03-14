(in-package :uncommon-lisp)

(defclass syntax ()
  ()
  (:documentation "A printer/reader syntax."))

(defmethod uncommon-lisp-read
    ((syntax syntax)
     (stream stream)
     (environment environment))
  "Perform a generic read from `stream' in `environment' with `syntax'."
  (read stream))

(defmethod uncommon-lisp-print
    ((syntax syntax)
     object
     (stream stream)
     (environment environment))
  "Perform a generic print of `object' to `stream' in `environment' with `syntax'."
  (prin1 object stream))
