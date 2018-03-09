(in-package :common-gateway)

;; event definitions

;; list of known events
(defvar *events* '(:connect :disconnect :message))

(defun event-notify (gateway event &rest arguments)
  "Notify event listeners of a gateway of an event with arguments."
  (loop
     :for listener :across (gateway-get-listeners gateway event)
     :do (apply listener (cons gateway arguments))))

;; todo: instead of globally recognized events, make subclasses of gateways able to extend the known events

;; todo: make event struct to store documentation and lambda list approprate for handlers of the event

;; todo: event-listener struct and deflistener that ensures functions are appropriate for the event
