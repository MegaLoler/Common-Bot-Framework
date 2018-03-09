(in-package :common-gateway)

(defmethod stringify ((object t))
  "Return a gateway friendly string representation for an object."
  (format nil "~A" object))
