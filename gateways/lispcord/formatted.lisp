(in-package :fmt)

(defmethod formatted ((value bold) (gateway lispcord-gateway:lispcord-gateway) &optional stream)
  "Discord formatted bolded value."
  (format stream "**~A**"
	  (formatted (bold-value value) gateway)))

(defmethod formatted ((value italic) (gateway lispcord-gateway:lispcord-gateway) &optional stream)
  "Discord formatted italicized value."
  (format stream "_~A_"
	  (formatted (italic-value value) gateway)))

(defmethod formatted ((value strike) (gateway lispcord-gateway:lispcord-gateway) &optional stream)
  "Discord formatted struck out value."
  (format stream "\~\~~A\~\~"
	  (formatted (strike-value value) gateway)))

(defmethod formatted ((value underline) (gateway lispcord-gateway:lispcord-gateway) &optional stream)
  "Discord formatted underlined value."
  (format stream "__~A__"
	  (formatted (underline-value value) gateway)))
