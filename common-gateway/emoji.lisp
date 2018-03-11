(defpackage :emoji
  (:documentation "Generalized emoji representation.")
  (:use :cl :common-gateway :fmt)
  (:export heart
	   smiley))
(in-package :emoji)

;; maybe use unicode chars for generic formatting?

;; also todo: defemoji macro to simplify emoji definitions

(defstruct (heart (:constructor heart)
		  (:print-object print-formatted))
  "Represents a heart emoji.")

(defstruct (smiley (:constructor smiley)
		   (:print-object print-formatted))
  "Represents a simple smiley.")

(defun print-formatted (object stream)
  "Generically print an emoji."
  (formatted object nil stream))

(defmethod formatted ((value heart) gateway &optional stream)
  "Generically format a heart emoji."
  (format stream "<3"))

(defmethod formatted ((value smiley) gateway &optional stream)
  "Generically format a simple smiley emoji."
  (format stream ":)"))
