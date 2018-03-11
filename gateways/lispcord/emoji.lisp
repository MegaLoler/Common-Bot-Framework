(in-package :emoji)

(defmethod formatted ((value heart) (gateway lispcord-gateway:lispcord-gateway) &optional stream)
  "Discord formatted heart emoji."
  (format stream ":heart:"))

(defmethod formatted ((value smiley) (gateway lispcord-gateway:lispcord-gateway) &optional stream)
  "Discord formatted simple smiley emoji."
  (format stream ":smiley:"))
