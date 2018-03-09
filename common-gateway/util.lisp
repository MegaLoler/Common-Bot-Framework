(in-package :common-gateway.util)

(defmethod stringify ((object t))
  "Return a gateway friendly string representation for an object."
  (format nil "~A" object))
